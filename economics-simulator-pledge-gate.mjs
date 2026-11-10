#!/usr/bin/env node
/*
 * economics-simulator-pledge-gate.mjs — what the built page shows and offers
 * for pledges, against the machine core's own state and verdicts.
 *
 * A fixed, stated set of histories — the generator gate's traces for seeds
 * 1..8 (as the page gate loads them) and the four hand-written scenarios, each
 * cut where the page's governance check stops accepting it — is loaded into
 * the built page through its loading path. At every state the history reaches
 * (states equal in every part a pledge depends on are visited once) the page
 * is put in live mode on that state and driven through real clicks in headless
 * Chromium. The page's runAttempt is replaced by a recorder, so a click that
 * commits a step records the step the page emitted and changes nothing; the
 * verdict on that step is the embedded core's appFold on the unchanged state.
 * The check computes no guard: expected regimes are the core state's rows,
 * accepted and refused signers are the core's verdicts, and a disabled offer
 * must name a guard the core's own refusal names.
 *
 *   D-REGIME-SHOWN       every pledge the page lists (purchase page, referente
 *                        panel, person page, pledge page) states its regime,
 *                        and the stated regime equals the core state's row
 *   D-OWNER-CORRECT      a pending pledge: the owner is offered a correction
 *                        (new amount > 0), signed by the owner, core-accepted
 *   D-OWNER-WITHDRAW     a pending pledge: the owner is offered a withdrawal,
 *                        correctPledge of amount 0 signed by the owner; the
 *                        core removes the pending row
 *   D-ACCEPTED-REFERENTE an accepted pledge: correction is offered with the
 *                        referente the only enabled signer; every other member
 *                        is shown disabled with the guard named
 *   D-SIGNER-SOUND       every enabled offer emits a step the core accepts;
 *                        every signer the core accepts for correctPledge on an
 *                        existing pledge and for pledge on an existing purchase
 *                        is offered enabled, in every class of amount; every
 *                        disabled offer is refused by the core and names a
 *                        guard of that refusal
 *   D-READ-ONLY          the tracked page is byte-identical after the run
 *   D-BUDGET             the run, and the selftest, fit the stated wall budget
 *
 * Usage:
 *   node economics-simulator-pledge-gate.mjs [--html PATH]   production run
 *   node economics-simulator-pledge-gate.mjs --selftest      controls that the
 *                                                            rows can fail
 */

import { mkdtempSync, readFileSync, readdirSync, writeFileSync } from 'node:fs';
import { createHash } from 'node:crypto';
import { dirname, join } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { tmpdir } from 'node:os';
import { loadCore } from './economics-simulator-generator-gate.mjs';
import { statedHistories as generatedHistories } from './economics-simulator-page-gate.mjs';
import { Browser, findChromium, sleep, rmQuiet } from './economics-simulator-teaching-gate.mjs';

const REPO = dirname(fileURLToPath(import.meta.url));
const argVal = flag => {
  const i = process.argv.indexOf(flag);
  return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : null;
};
const HTML = argVal('--html') || join(REPO, 'economics-simulator.html');
const CORE = join(REPO, 'economics-simulator-core.mjs');
const SCENARIOS = join(REPO, 'economics-simulator-scenarios');
const ALL = process.argv.includes('--all');   // print every red, not the first eight
const sha256 = s => createHash('sha256').update(s).digest('hex');

/* --- the stated configuration (D-SIGNER-SOUND, D-BUDGET) ---------------------- */
const CONFIG = Object.freeze({
  budgetMs: 150000,   // wall budget of this gate, and of its selftest
});
const ROWS = ['D-REGIME-SHOWN', 'D-OWNER-CORRECT', 'D-OWNER-WITHDRAW', 'D-ACCEPTED-REFERENTE',
  'D-SIGNER-SOUND', 'D-READ-ONLY', 'D-BUDGET'];
const PAGE_ROWS = ROWS.slice(0, 5);

/* --- the stated histories ------------------------------------------------------ */

/* a hand-written scenario cut where the core's governance check stops
   accepting it, as the page loads exactly what that check accepts */
function scenarioHistory(core, sc) {
  const wrap = sc.wrap;
  const accepted = k => {
    const env = { ...wrap, steps: wrap.steps.slice(0, k) };
    try { core.verifyIntegratedV1(env, { appliedOnly: true }); core.verifyGovernedIntegrated(env); return true; }
    catch { return false; }
  };
  let lo = 0, hi = wrap.steps.length;
  while (lo < hi) { const mid = (lo + hi + 1) >> 1; if (accepted(mid)) lo = mid; else hi = mid - 1; }
  const env = { ...wrap, steps: wrap.steps.slice(0, lo) };
  const ver = core.verifyIntegratedV1(env, { appliedOnly: true });
  return { seed: sc.name, generated: wrap.steps.length, env, states: ver.states };
}

async function statedHistories() {
  const core = await loadCore(CORE);
  const scenarios = readdirSync(SCENARIOS).filter(f => f.endsWith('.json')).sort()
    .map(f => scenarioHistory(core, JSON.parse(readFileSync(join(SCENARIOS, f), 'utf8'))));
  return [...await generatedHistories(), ...scenarios];
}

/* the parts of a state a pledge's display and offers depend on: purchases with
   their pledges, accounts, and who is a responsabile. States equal in all of
   them are the same state for this gate. */
const stateKey = agg => {
  const a = agg.appFold !== undefined ? agg.appFold : agg.payload;
  return JSON.stringify([a.collections, a.conti, agg.members.map(([k, m]) => [k, m.roles])]);
};
const payloadOf = agg => agg.appFold !== undefined ? agg.appFold : agg.payload;

/* every distinct state of every history, with where it was first reached */
function visits(histories) {
  const seen = new Set(), out = [];
  histories.forEach((h, hi) => h.states.forEach((agg, i) => {
    const k = stateKey(agg);
    if (seen.has(k)) return;
    seen.add(k);
    out.push({ h: hi, i, agg });
  }));
  return out;
}

/* --- the page: injected once, then asked about one state at a time ------------ */

/* Runs in the page. It drives the page's own controls and reads what they
   show; it holds no guard. Every verdict is appFold on the unchanged state. */
function installInPage() {
  if (window.__pg) return true;
  const txt = el => el ? el.textContent.replace(/\s+/g, ' ').trim() : '';
  window.__emitted = [];
  runAttempt = (tag, args) => {
    window.__emitted.push({ tag, args: JSON.parse(JSON.stringify(args)) });
    return { ok: true, state, flow: '' };
  };
  const verdict = (tag, args) => {
    try {
      const r = appFold(curView(), state, args.author, leanEventJson({ tag, ...args }));
      if (!r.ok) return { ok: false, failed: (r.failed || []).map(f => ({ code: f.code, law: f.law })) };
      const col = r.state.collections.find(x => x.id === args.c);
      return { ok: true, after: col ? { pending: col.pending.map(p => [p.user, p.amount]),
        accepted: col.accepted.map(p => [p.user, p.amount]) } : null,
        conto: bal(r.state.conti, args.user) };
    } catch (e) { return { ok: false, error: String(e && e.message) }; }
  };
  const live = i => {
    presSet(i);
    window.__keep = pres;
    pres = null;
    navStack = [{ view: 'group' }];
    closePop(); flow = null;
  };
  const done = () => { closePop(); flow = null; pres = window.__keep; navStack = [{ view: 'group' }]; };
  const goTo = nav => { navStack = [{ view: 'group' }]; closePop(); flow = null; window.__emitted = []; nav(); };
  const navColl = c => () => go({ view: 'collection', c });
  const navPerson = u => () => go({ view: 'person', u, hat: 'member' });
  const navPledge = (c, u) => () => { go({ view: 'collection', c }); goPledge(c, u); };

  /* what the page lists, place by place */
  const rowsOf = (root, where, c, uOf) => [...root.querySelectorAll('.row')].map(row => ({
    where, c: row.dataset.pledgeC !== undefined ? Number(row.dataset.pledgeC) : c,
    user: uOf(row), label: txt(row.querySelector('.stag')), amount: txt(row.querySelector('.amt')) }));
  const userOfRow = row => row.dataset.pledgeU !== undefined ? row.dataset.pledgeU
    : (row.querySelector('[data-u]') || { dataset: {} }).dataset.u;
  const display = () => {
    const out = [];
    for (const col of state.collections) {
      goTo(navColl(col.id));
      const view = document.getElementById('view');
      const panel = document.getElementById('refpanel');
      const list = [...view.querySelectorAll('.card > .rows .row[data-pledge-c]')];
      out.push(...list.map(row => ({ where: 'purchase', c: col.id, user: userOfRow(row),
        label: txt(row.querySelector('.stag')), amount: txt(row.querySelector('.amt')) })));
      if (panel) out.push(...rowsOf(panel, 'refpanel', col.id, userOfRow));
      for (const p of [...col.pending, ...col.accepted]) {
        goTo(navPledge(col.id, p.user));
        const head = document.querySelector('#view .vhead');
        out.push({ where: 'pledge', c: col.id, user: p.user, label: txt(head && head.querySelector('.stag')),
          amount: txt(head && head.querySelector('.money .v')) });
      }
    }
    for (const k of members()) {
      goTo(navPerson(k));
      const list = [...document.querySelectorAll('#view .row[data-pledge-c]')];
      out.push(...list.map(row => ({ where: 'person', c: Number(row.dataset.pledgeC), user: userOfRow(row),
        owner: k, label: txt(row.querySelector('.stag')), amount: txt(row.querySelector('.amt')) })));
    }
    return out;
  };

  /* one control, driven by a path of choices; the result is what is on screen */
  const observe = () => {
    const pop = document.getElementById('pop');
    const emitted = window.__emitted.slice();
    if (pop.hidden) return { kind: 'closed', emitted };
    const am = document.getElementById('am-n');
    if (am) return { kind: 'amount', min: Number(am.min), max: Number(am.max), val: Number(am.value), emitted };
    const ask = txt(pop.querySelector('.tiny'));
    const chips = [...pop.querySelectorAll('.chips .chip')].map(b => {
      const gb = b.querySelector('.gb');
      const clone = b.cloneNode(true);
      const g = clone.querySelector('.gb');
      if (g) g.remove();
      return { id: b.dataset.id === undefined ? null : b.dataset.id, label: txt(clone),
        enabled: !b.disabled, guard: b.disabled ? txt(gb) : null };
    });
    return { kind: 'chips', ask, chips, emitted };
  };
  const drive = (nav, pick, path) => {
    goTo(nav);
    const btn = pick();
    if (!btn) return { kind: 'absent' };
    if (btn.disabled) return { kind: 'disabled', guard: txt(btn.querySelector('.gb')) };
    btn.click();
    for (const p of path) {
      if (document.getElementById('pop').hidden) return { kind: 'lost' };
      if ('chip' in p) {
        const b = [...document.querySelectorAll('#pop .chips .chip')].find(x => x.dataset.id === p.chip);
        if (!b) return { kind: 'lost' };
        b.click();
      } else {
        const n = document.getElementById('am-n');
        n.value = String(p.amount);
        n.dispatchEvent(new Event('input'));
        n.dispatchEvent(new Event('change'));
        const ok = document.getElementById('am-ok');
        if (ok.disabled) return { kind: 'lost' };
        ok.click();
      }
    }
    return observe();
  };
  /* every dialog reachable from one control: each chip list, each amount range,
     and for each enabled chip and sampled amount the step the page emits with
     the core's verdict on it */
  const explore = (nav, pick) => {
    const nodes = [];
    const walk = path => {
      const o = drive(nav, pick, path);
      if (o.kind === 'absent' || o.kind === 'disabled' || o.kind === 'lost') { nodes.push({ path, ...o }); return; }
      if (o.kind === 'chips') {
        nodes.push({ path, kind: 'chips', ask: o.ask, chips: o.chips });
        for (const ch of o.chips) if (ch.enabled && ch.id !== null) walk([...path, { chip: ch.id }]);
      } else if (o.kind === 'amount') {
        nodes.push({ path, kind: 'amount', min: o.min, max: o.max, val: o.val });
        const vs = [...new Set([o.min, o.min + 1, o.val, o.max - 1, o.max])].filter(v => v >= o.min && v <= o.max);
        for (const v of vs) {
          const r = drive(nav, pick, [...path, { amount: v }]);
          nodes.push({ path: [...path, { amount: v }], kind: 'leaf', v, emitted: r.emitted,
            verdicts: (r.emitted || []).map(e => verdict(e.tag, e.args)) });
        }
      } else {
        nodes.push({ path, kind: 'leaf', v: null, emitted: o.emitted,
          verdicts: o.emitted.map(e => verdict(e.tag, e.args)) });
      }
    };
    walk([]);
    return nodes;
  };
  const nthControl = (scope, k) => () => document.querySelectorAll(scope)[k] || null;
  const CORRECT = '[data-act][title=".correctPledge"]';
  const entries = () => {
    const out = [];
    for (const col of state.collections) {
      for (const p of [...col.pending, ...col.accepted]) {
        goTo(navPledge(col.id, p.user));
        const nPledge = document.querySelectorAll('#view ' + CORRECT).length;
        for (let k = 0; k < nPledge; k++)
          out.push({ entry: 'pledge-page', c: col.id, user: p.user,
            nodes: explore(navPledge(col.id, p.user), nthControl('#view ' + CORRECT, k)) });
        goTo(navColl(col.id));
        const own = [...document.querySelectorAll('#refpanel ' + CORRECT)]
          .map((b, k) => [b, k]).filter(([b]) => b.dataset.u === p.user);
        for (const [, k] of own)
          out.push({ entry: 'referente-panel', c: col.id, user: p.user,
            nodes: explore(navColl(col.id), nthControl('#refpanel ' + CORRECT, k)) });
      }
      out.push({ entry: 'purchase-pledge', c: col.id, user: null,
        nodes: explore(navColl(col.id), nthControl('#view [data-task="pledge"]', 0)) });
    }
    for (const k of members())
      out.push({ entry: 'person-pledge', c: null, user: k,
        nodes: explore(navPerson(k), nthControl('#hat-member [data-task="pledge"]', 0)) });
    return out;
  };

  /* the core's verdict on every candidate step, for completeness */
  const candidates = () => {
    const rows = [];
    const mem = members();
    for (const col of state.collections) {
      for (const u of mem) {
        const row = col.accepted.find(p => p.user === u) || col.pending.find(p => p.user === u);
        const b = Math.max(0, bal(state.conti, u));
        const push = (tag, cur, vs) => {
          for (const signer of mem) for (const v of vs) {
            const args = { author: signer, user: u, c: col.id, v };
            rows.push({ tag, c: col.id, user: u, signer, v, cur, verdict: verdict(tag, args) });
          }
        };
        push('pledge', 0, [...new Set([1, b])].filter(v => v >= 1));
        if (row) push('correctPledge', row.amount,
          [...new Set([0, 1, row.amount - 1, row.amount + 1, row.amount + b])].filter(v => v >= 0 && v !== row.amount));
      }
    }
    return rows;
  };
  const names = () => ({
    members: members().map(k => [k, nm(k)]),
    colls: state.collections.map(c => [c.id, lbl(c.id)]),
  });
  window.__pg = { live, done, display, entries, candidates, names, eur };
  return true;
}
const INSTALL = `(${installFn()})()`;
function installFn() { return installInPage.toString(); }

const LOAD = text => `(() => {
  if (pres) exitPresentation();
  importText(${JSON.stringify(text)});
  return pres ? pres.n.env.steps.length : -1;
})()`;

/* --- reading the page's words back into the core's terms ---------------------- */

/* the regime a label states; null when it states none */
function statedRegime(label) {
  if (/^pendente\b/.test(label) && /ritira/.test(label)) return 'pending';
  if (/^accettato\b/.test(label) && /solo il referente/.test(label)) return 'accepted';
  return null;
}
const classOf = (cur, v) => v === 0 ? 'zero' : v < cur ? 'lower' : v > cur ? 'raise' : 'same';
const guardsOf = verdict => verdict.failed ? verdict.failed.flatMap(f => [f.code, f.law].filter(Boolean)) : [];

/* --- ledger ------------------------------------------------------------------- */

function newLedger() {
  const reds = new Map(ROWS.map(r => [r, []]));
  const checks = new Map(ROWS.map(r => [r, 0]));
  return { reds, checks, states: 0, steps: 0, shapes: new Set(), ok(row, cond, why) {
    checks.set(row, checks.get(row) + 1);
    if (!cond) reds.get(row).push(why);
  } };
}

/* --- the judgement of one state ------------------------------------------------ */

/* the core state's pledges: (purchase, user) -> regime and amount; the accepted
   row is looked up first, as the core does */
function pledgesOf(payload) {
  const out = [];
  for (const col of payload.collections) {
    for (const p of col.accepted) out.push({ c: col.id, user: p.user, amount: p.amount, regime: 'accepted', referente: col.referente });
    for (const p of col.pending) out.push({ c: col.id, user: p.user, amount: p.amount, regime: 'pending', referente: col.referente });
  }
  return out;
}

function judgeDisplay(L, where, pledges, shown, eur) {
  const row = 'D-REGIME-SHOWN';
  const keyOf = (c, u) => `${c}|${u}`;
  const byPlace = place => shown.filter(s => s.where === place);
  const expectAt = (place, wanted, label) => {
    const got = byPlace(place).filter(s => label(s));
    L.ok(row, got.length === wanted.length,
      `${where}: ${place} lists ${got.length} pledges, the core state holds ${wanted.length}`);
    for (const w of wanted) {
      const hits = got.filter(s => s.c === w.c && s.user === w.user && (place !== 'person' || s.owner === w.user));
      L.ok(row, hits.length === 1, `${where}: ${place} lists ${hits.length} rows for the pledge of ${w.user} in purchase ${w.c}`);
      for (const h of hits) {
        const stated = statedRegime(h.label);
        L.ok(row, stated !== null,
          `${where}: ${place}: the pledge of ${w.user} in purchase ${w.c} states no regime (label «${h.label}»)`);
        if (stated !== null) L.ok(row, stated === w.regime,
          `${where}: ${place}: the pledge of ${w.user} in purchase ${w.c} is ${w.regime} in the core state, the page states ${stated} («${h.label}»)`);
        L.ok(row, h.amount === eur[w.amount],
          `${where}: ${place}: the pledge of ${w.user} in purchase ${w.c}: core amount ${w.amount}, page «${h.amount}»`);
      }
    }
  };
  const cols = [...new Set(pledges.map(p => p.c))];
  for (const place of ['purchase', 'refpanel', 'pledge', 'person'])
    expectAt(place, pledges, () => true);
  void cols; void keyOf;
}

/* one entry's dialogs, as the offered choices they hold:
   [{ tag, c, user, signer, enabled, guard, amounts: [{ v, class, emitted, verdict }] }] */
function offersOf(entry, names) {
  const keyOfMember = label => {
    const m = names.members.find(([, n]) => label === n || label === '★ ' + n);
    return m ? m[0] : null;
  };
  const collOf = label => {
    const m = names.colls.find(([, n]) => label.startsWith('«' + n + '»'));
    return m ? m[0] : null;
  };
  const roleOf = ask => /impegna|per chi/.test(ask) ? 'user' : /acquisto/.test(ask) ? 'coll'
    : /registra/.test(ask) ? 'signer' : 'unknown';
  const offers = [];
  const nodeAt = path => entry.nodes.find(n => JSON.stringify(n.path) === JSON.stringify(path));
  /* walk the chip lists in order; a path is the chosen ids so far */
  const visit = (path, ctx) => {
    const n = nodeAt(path);
    if (!n || n.kind !== 'chips') return;
    const role = roleOf(n.ask);
    for (const ch of n.chips) {
      const who = role === 'coll' ? collOf(ch.label) : keyOfMember(ch.label);
      const next = { ...ctx, [role === 'coll' ? 'c' : role]: who };
      if (role === 'signer') {
        const leaves = entry.nodes.filter(x => x.kind === 'leaf' && x.path.length >= path.length + 1 &&
          JSON.stringify(x.path.slice(0, path.length + 1)) === JSON.stringify([...path, { chip: ch.id }]));
        offers.push({ ...next, enabled: ch.enabled, guard: ch.guard, leaves, chipLabel: ch.label });
      } else if (ch.enabled && ch.id !== null) visit([...path, { chip: ch.id }], next);
      else offers.push({ ...next, signer: null, enabled: false, guard: ch.guard, leaves: [], chipLabel: ch.label, cut: role });
    }
  };
  const root = entry.nodes[0];
  if (root && root.kind !== 'chips' && root.kind !== 'absent' && root.kind !== 'disabled' && root.kind !== 'lost') {
    /* a control that opens no signer choice offers the one signer it signs with */
    const leaves = entry.nodes.filter(x => x.kind === 'leaf');
    const signer = leaves.length && leaves[0].emitted.length ? leaves[0].emitted[0].args.author : null;
    if (signer !== null) offers.push({ c: entry.c, user: entry.user, signer, enabled: true, guard: null, leaves, chipLabel: null });
  } else visit([], { c: entry.c, user: entry.user });
  return offers;
}

/* the offers of one control that opens on the signer chips (correction, withdrawal) */
function judgeOffers(L, where, ctx) {
  const { pledges, entries, candidates, names } = ctx;
  const row = 'D-SIGNER-SOUND';
  /* chips are told apart by the names the page shows: two equal names would hide a swap */
  L.ok(row, new Set(names.members.map(([, n]) => n)).size === names.members.length &&
    new Set(names.colls.map(([, n]) => n)).size === names.colls.length,
    `${where}: the page gives two members or two purchases the same name, so a swap of choices would pass`);
  const accepted = (tag, c, u) => candidates.filter(r => r.tag === tag && r.c === c && r.user === u && r.verdict.ok);
  const refused = (tag, c, u) => candidates.filter(r => r.tag === tag && r.c === c && r.user === u && !r.verdict.ok);
  const checkLeaf = (tag, o, lf) => {
    for (const i in lf.emitted) {
      const e = lf.emitted[i], v = lf.verdicts[i];
      L.steps++;
      L.ok(row, e.tag === tag, `${where}: ${o.entry}: the page emits ${e.tag}, expected ${tag}`);
      L.ok(row, e.args.author === o.signer, `${where}: ${o.entry}: the signer chosen is ${o.signer}, the step is signed by ${e.args.author}`);
      L.ok(row, v.ok === true,
        `${where}: ${o.entry}: enabled offer ${tag} of ${e.args.user} in purchase ${e.args.c} signed by ${o.signer} (amount ${e.args.v}) is refused by the core: ${guardsOf(v).join(' | ') || v.error}`);
    }
  };

  /* per pledge: the correction controls, over every entry that offers them */
  for (const p of pledges) {
    const mine = entries.filter(e => (e.entry === 'pledge-page' || e.entry === 'referente-panel') && e.c === p.c && e.user === p.user);
    const offers = mine.flatMap(e => offersOf(e, names).map(o => ({ ...o, entry: e.entry })));
    const members = names.members.map(([k]) => k);
    const enabled = offers.filter(o => o.enabled && o.signer !== null);
    const ownerOfRegime = p.regime === 'pending' ? p.user : p.referente;
    const coreOk = accepted('correctPledge', p.c, p.user);
    const coreSigners = [...new Set(coreOk.map(r => r.signer))];

    /* soundness: every enabled signer's emitted steps are accepted */
    for (const o of enabled) for (const lf of o.leaves) checkLeaf('correctPledge', o, lf);
    /* named disabled: refused by the core, naming a guard the core names */
    for (const o of offers.filter(x => !x.enabled && x.signer !== null)) {
      const rs = refused('correctPledge', p.c, p.user).filter(r => r.signer === o.signer);
      L.ok(row, rs.length > 0, `${where}: ${p.regime} pledge of ${p.user} in purchase ${p.c}: ${o.signer} is shown disabled but the core accepts a correction signed by them`);
      L.ok(row, !!o.guard && rs.some(r => guardsOf(r.verdict).includes(o.guard)),
        `${where}: ${p.regime} pledge of ${p.user} in purchase ${p.c}: ${o.signer} disabled with guard «${o.guard}», not among the core's ${[...new Set(rs.flatMap(r => guardsOf(r.verdict)))].join(' | ')}`);
    }
    /* completeness: every signer the core accepts is offered enabled, in every class of amount */
    const reach = new Set(enabled.flatMap(o => o.leaves.flatMap(lf => lf.emitted.map((e, i) => `${o.signer}|${classOf(p.amount, e.args.v)}|${lf.verdicts[i].ok}`))));
    for (const r of coreOk) {
      const cls = classOf(r.cur, r.v);
      L.ok(row, enabled.some(o => o.signer === r.signer),
        `${where}: ${p.regime} pledge of ${p.user} in purchase ${p.c}: the core accepts a correction signed by ${r.signer}, the page does not offer it enabled`);
      L.ok(row, reach.has(`${r.signer}|${cls}|true`),
        `${where}: ${p.regime} pledge of ${p.user} in purchase ${p.c}: the core accepts a ${cls} correction signed by ${r.signer}, the page offers none`);
    }
    /* the rows of the regime */
    const ownerRow = p.regime === 'pending' ? 'D-OWNER-CORRECT' : 'D-ACCEPTED-REFERENTE';
    if (p.regime === 'pending') {
      const mineOwner = enabled.filter(o => o.signer === p.user);
      const positive = mineOwner.flatMap(o => o.leaves.flatMap(lf => lf.emitted.map((e, i) => ({ e, v: lf.verdicts[i] }))))
        .filter(x => x.e.args.v > 0 && x.e.args.v !== p.amount && x.v.ok);
      const wanted = coreOk.some(r => r.signer === p.user && r.v > 0);
      L.ok('D-OWNER-CORRECT', !wanted || positive.length > 0,
        `${where}: pending pledge of ${p.user} in purchase ${p.c}: the owner is offered no correction to a new amount above 0`);
      const zero = mineOwner.flatMap(o => o.leaves.flatMap(lf => lf.emitted.map((e, i) => ({ e, v: lf.verdicts[i] }))))
        .filter(x => x.e.args.v === 0 && x.v.ok);
      const wantZero = coreOk.some(r => r.signer === p.user && r.v === 0);
      L.ok('D-OWNER-WITHDRAW', !wantZero || zero.length > 0,
        `${where}: pending pledge of ${p.user} in purchase ${p.c}: the owner is offered no withdrawal`);
      for (const z of zero) {
        const gone = z.v.after && !z.v.after.pending.some(([u]) => u === p.user);
        L.ok('D-OWNER-WITHDRAW', gone === true, `${where}: pending pledge of ${p.user} in purchase ${p.c}: the core does not remove the pending row on the withdrawal`);
      }
    } else {
      L.ok('D-ACCEPTED-REFERENTE', JSON.stringify([...new Set(enabled.map(o => o.signer))].sort()) === JSON.stringify(coreSigners.sort()) &&
        (coreSigners.length === 0 || coreSigners.every(s => s === p.referente)),
        `${where}: accepted pledge of ${p.user} in purchase ${p.c}: enabled signers ${[...new Set(enabled.map(o => o.signer))].join(', ') || 'none'}, the core accepts ${coreSigners.join(', ') || 'none'}`);
      for (const m of members.filter(k => k !== p.referente))
        L.ok('D-ACCEPTED-REFERENTE', offers.some(o => o.signer === m && !o.enabled && o.guard),
          `${where}: accepted pledge of ${p.user} in purchase ${p.c}: ${m} is not shown disabled with a guard`);
    }
    void ownerOfRegime; void ownerRow;
  }

  /* pledge dialogs: from the purchase page and from the person page */
  for (const e of entries.filter(x => x.entry === 'purchase-pledge' || x.entry === 'person-pledge')) {
    const offers = offersOf(e, names);
    const enabled = offers.filter(o => o.enabled && o.signer !== null);
    for (const o of enabled) for (const lf of o.leaves) checkLeaf('pledge', { ...o, entry: e.entry }, lf);
    for (const o of offers.filter(x => !x.enabled)) {
      const who = o.user, c = o.c;
      const rs = o.signer === null
        ? refused('pledge', c, who).filter(r => r.signer === who)
        : refused('pledge', c, who).filter(r => r.signer === o.signer);
      L.ok(row, who === undefined || who === null || c === undefined || c === null || rs.length > 0,
        `${where}: ${e.entry}: pledge of ${who} in purchase ${c} signed by ${o.signer || who} is shown disabled but the core accepts it`);
      L.ok(row, !!o.guard && (rs.length === 0 || rs.some(r => guardsOf(r.verdict).includes(o.guard))),
        `${where}: ${e.entry}: pledge of ${who} in purchase ${c} signed by ${o.signer || who} disabled with guard «${o.guard}», not among the core's ${[...new Set(rs.flatMap(r => guardsOf(r.verdict)))].join(' | ')}`);
    }
    for (const r of candidates.filter(x => x.tag === 'pledge' && x.verdict.ok &&
        (e.entry === 'purchase-pledge' ? x.c === e.c : x.user === e.user)))
      L.ok(row, enabled.some(o => o.signer === r.signer && o.user === r.user && o.c === r.c),
        `${where}: ${e.entry}: the core accepts a pledge of ${r.user} in purchase ${r.c} signed by ${r.signer}, the page does not offer it enabled`);
  }
}

/* --- the page, state by state ---------------------------------------------------- */

async function probeState(b, s, v) {
  await b.eval(s, `window.__pg.live(${v.i})`);
  try {
    const eurs = JSON.parse(await b.eval(s, `JSON.stringify(Object.fromEntries(${JSON.stringify([...new Set(pledgesOf(payloadOf(v.agg)).map(p => p.amount))])}.map(a => [a, window.__pg.eur(a)])))`));
    const shown = JSON.parse(await b.eval(s, 'JSON.stringify(window.__pg.display())'));
    const entries = JSON.parse(await b.eval(s, 'JSON.stringify(window.__pg.entries())'));
    const candidates = JSON.parse(await b.eval(s, 'JSON.stringify(window.__pg.candidates())'));
    const names = JSON.parse(await b.eval(s, 'JSON.stringify(window.__pg.names())'));
    return { eurs, shown, entries, candidates, names };
  } finally { await b.eval(s, 'window.__pg.done()'); }
}

async function runPage(html, histories, browser, { firstRed = false } = {}) {
  const L = newLedger();
  const own = !browser;
  const profile = own ? mkdtempSync(join(tmpdir(), 'rg-pledge-profile-')) : null;
  const b = browser || new Browser(findChromium(), profile);
  try {
    if (own) await sleep(400);
    const s = await b.page('file://' + html, 1280, 900);
    await b.eval(s, INSTALL);
    for (let hi = 0; hi < histories.length; hi++) {
      const h = histories[hi];
      const loaded = await b.eval(s, LOAD(JSON.stringify(h.env)));
      L.ok('D-SIGNER-SOUND', loaded === h.env.steps.length, `history ${h.seed}: the page loaded ${loaded} steps of ${h.env.steps.length}`);
      if (loaded !== h.env.steps.length) continue;
      for (const v of visits([h]).filter(() => true)) {
        const where = `history ${h.seed} step ${v.i}`;
        const probe = await probeState(b, s, v);
        const payload = payloadOf(v.agg);
        const pledges = pledgesOf(payload);
        L.states++;
        reach(L, payload, pledges);
        judgeDisplay(L, where, pledges, probe.shown, probe.eurs);
        judgeOffers(L, where, { pledges, entries: probe.entries, candidates: probe.candidates, names: probe.names });
        if (firstRed && PAGE_ROWS.some(r => L.reds.get(r).length)) return L;
      }
    }
  } finally {
    if (own) { b.close(); rmQuiet(profile); }
  }
  return L;
}

/* the shapes the stated histories claim to judge; each must be reached */
const SHAPES = ['pending pledge', 'accepted pledge', 'pledge of the purchase referente', 'pledge of a member who is not a responsabile',
  'purchase with no pledge', 'two purchases open together', 'member who cannot pledge for lack of credit'];
function reach(L, payload, pledges) {
  if (pledges.some(p => p.regime === 'pending')) L.shapes.add('pending pledge');
  if (pledges.some(p => p.regime === 'accepted')) L.shapes.add('accepted pledge');
  if (pledges.some(p => p.user === p.referente)) L.shapes.add('pledge of the purchase referente');
  if (pledges.some(p => p.user !== p.referente)) L.shapes.add('pledge of a member who is not a responsabile');
  if (payload.collections.some(c => !c.pending.length && !c.accepted.length)) L.shapes.add('purchase with no pledge');
  if (payload.collections.length > 1) L.shapes.add('two purchases open together');
  const bals = new Map(payload.conti);
  if (payload.collections.length && [...bals].some(([, v]) => v < 1)) L.shapes.add('member who cannot pledge for lack of credit');
}
function judgeReach(L) {
  for (const shape of SHAPES)
    L.ok('D-SIGNER-SOUND', L.shapes.has(shape), `the stated histories never reach: ${shape}`);
}

const overBudget = (what, elapsedMs, budgetMs) =>
  elapsedMs > budgetMs ? `${what} budget exceeded: ${Math.round(elapsedMs)} ms > ${budgetMs} ms` : null;

async function timedRun(html, histories, browser, budgetMs = CONFIG.budgetMs) {
  const t0 = performance.now();
  const L = await runPage(html, histories, browser);
  const elapsedMs = performance.now() - t0;
  const over = overBudget('pledge page', elapsedMs, budgetMs);
  L.ok('D-BUDGET', over === null, over);
  return { L, elapsedMs, budgetMs };
}

/* --- report ------------------------------------------------------------------------ */

function report(L, required) {
  let bad = 0;
  for (const row of ROWS) {
    const n = L.checks.get(row);
    const reds = [...new Set(L.reds.get(row))];
    if (required.includes(row) && n === 0) reds.unshift('row never evaluated: no check executed');
    if (reds.length) {
      bad += 1;
      console.error(`ROW-RED ${row}`);
      reds.slice(0, ALL ? reds.length : 8).forEach(r => console.error(`  - ${r}`));
      if (!ALL && reds.length > 8) console.error(`  - … and ${reds.length - 8} more`);
    } else if (required.includes(row)) {
      console.log(`ROW-OK ${row} (${n} checks${row === 'D-SIGNER-SOUND' ? `, ${L.states} states, ${L.steps} emitted steps` : ''})`);
    }
  }
  return bad;
}

async function main() {
  const before = sha256(readFileSync(HTML));
  const histories = await statedHistories();
  const { L, elapsedMs, budgetMs } = await timedRun(HTML, histories);
  judgeReach(L);
  L.ok('D-READ-ONLY', sha256(readFileSync(HTML)) === before, 'the tracked page changed during the run');
  const bad = report(L, ROWS);
  if (bad) { console.error(`RED: ${bad} rows`); process.exit(1); }
  console.error(`pledge page wall ${Math.round(elapsedMs)} ms of ${budgetMs} ms`);
  console.log(`GREEN: ${histories.length} histories, ${L.states} states, offers = core verdicts`);
}

/* --- selftest: each row can fail ----------------------------------------------------- */

const once = (text, from, to) => {
  const n = text.split(from).length - 1;
  if (n !== 1) throw new Error(`mutant not applied: ${n} occurrences of «${from.slice(0, 70)}»`);
  return text.replace(from, () => to);
};

/* an edit of the built page that falsifies one offer or label; `row` is the
   row that must go RED and `why` what its message must say. An edit that does
   not apply exactly once is a broken control, not a caught mutant. */
const MUTANTS = [
  { id: 'owner-offered-on-an-accepted-pledge', row: 'D-SIGNER-SOUND', why: /enabled offer correctPledge .* is refused by the core/,
    edit: h => once(h, 'else if (!res.ok) { ch.dis',
      'else if (!res.ok && !(col.accepted.some(q => q.user === p.user) && k === p.user)) { ch.dis') },
  { id: 'referente-offered-on-a-pending-pledge', row: 'D-SIGNER-SOUND', why: /enabled offer correctPledge .* is refused by the core/,
    edit: h => once(h, 'else if (!res.ok) { ch.dis',
      'else if (!res.ok && !(!col.accepted.some(q => q.user === p.user) && k === col.referente)) { ch.dis') },
  { id: 'withdrawal-hidden', row: 'D-OWNER-WITHDRAW', why: /offered no withdrawal/,
    edit: h => once(h, '<button type="button" class="task" data-act="withdraw" data-u="${pu}" title=".correctPledge">Ritira l\'impegno</button>', '') },
  { id: 'owner-correction-hidden', row: 'D-OWNER-CORRECT', why: /offered no correction/,
    edit: h => once(h, '<button type="button" class="task" data-act="correct" data-u="${pu}" title=".correctPledge">Correggi importo</button>\n      <button type="button" class="task" data-act="withdraw"',
      '<button type="button" class="task" data-act="withdraw"') },
  { id: 'accepted-signers-other-than-the-referente-not-shown', row: 'D-ACCEPTED-REFERENTE', why: /is not shown disabled with a guard/,
    edit: h => once(h, '        return mem.map(k => {\n          const ch = person(k);\n          const res = cur',
      '        return mem.filter(k => !col.accepted.some(q => q.user === p.user) || k === col.referente).map(k => {\n          const ch = person(k);\n          const res = cur') },
  { id: 'disabled-signer-names-no-core-guard', row: 'D-SIGNER-SOUND', why: /disabled with guard «nessuna/,
    edit: h => once(h, 'ch.dis = res.failed[0].law || res.failed[0].code;', "ch.dis = 'nessuna guardia';") },
  { id: 'regime-label-swapped', row: 'D-REGIME-SHOWN', why: /the core state, the page states/,
    edit: h => once(h, 'class="stag ${REGIME[regime].cls}">${esc(REGIME[regime].text)}</span>',
      'class="stag ${REGIME[regime].cls}">${esc(REGIME[regime === \'pending\' ? \'accepted\' : \'pending\'].text)}</span>') },
  { id: 'responsabile-enabled-as-author-of-anothers-pledge', row: 'D-SIGNER-SOUND', why: /enabled offer pledge .* is refused by the core/,
    edit: h => once(h, "tag === 'pledge' && p.user !== undefined && p.user !== k) ch.dis",
      "tag === 'pledge' && p.user !== undefined && p.user !== k && !resp(k)) ch.dis") },
  { id: 'correction-range-above-the-balance', row: 'D-SIGNER-SOUND', why: /enabled offer correctPledge .* is refused by the core/,
    edit: h => once(h, 'return { min: accepted ? 0 : 1, max: mx,', 'return { min: accepted ? 0 : 1, max: mx + 1,') },
];

/* the core states the check expects, altered in a copy (the page still loads
   the true envelope): every purchase's pending and accepted pledges exchanged */
const withStates = (histories, alter) => histories.map(h => ({ ...h, states: h.states.map(alter) }));
const swapRegimes = agg => {
  const c = JSON.parse(JSON.stringify(agg));
  const a = payloadOf(c);
  for (const col of a.collections) [col.pending, col.accepted] = [col.accepted, col.pending];
  return c;
};

async function selftest() {
  const t0 = performance.now();
  const doc = readFileSync(HTML, 'utf8');
  const work = mkdtempSync(join(tmpdir(), 'rg-pledge-selftest-'));
  const profile = mkdtempSync(join(tmpdir(), 'rg-pledge-profile-'));
  const b = new Browser(findChromium(), profile);
  const bad = [];
  try {
    await sleep(400);
    const histories = await statedHistories();
    const base = await runPage(HTML, histories, b);
    judgeReach(base);
    const baseReds = ROWS.filter(r => base.reds.get(r).length);
    if (baseReds.length || base.states === 0)
      bad.push(`control: the unmutated page is not green with states compared: ${baseReds.join(', ') || 'no state was compared'}`);
    else console.log(`control: the unmutated page is green (${base.states} states compared)`);
    for (const m of MUTANTS) {
      let html;
      try { html = m.edit(doc); } catch (e) { bad.push(`${m.id}: ${e.message}`); continue; }
      if (html === doc) { bad.push(`${m.id}: the edit did not change the page`); continue; }
      const path = join(work, m.id + '.html');
      writeFileSync(path, html);
      const L = await runPage(path, histories, b, { firstRed: true });
      const reds = L.reds.get(m.row);
      const hit = reds.find(r => m.why.test(r));
      if (hit) console.log(`mutant ${m.id}: ${m.row} red as expected — ${hit.slice(0, 110)}`);
      else bad.push(`${m.id}: ${m.row} did not go red for the stated reason (${L.checks.get(m.row)} checks, ${reds.length} reds)`);
    }
    const flipped = await runPage(HTML, withStates(histories, swapRegimes), b, { firstRed: true });
    if (flipped.reds.get('D-REGIME-SHOWN').length) console.log('control altered-state: D-REGIME-SHOWN red against a core state with the regimes exchanged');
    else bad.push('altered-state: D-REGIME-SHOWN stayed green against an altered core state: the expected regimes are not read from it');
    const few = await runPage(HTML, histories.slice(0, 1), b);
    judgeReach(few);
    const missing = few.reds.get('D-SIGNER-SOUND').filter(r => /never reach/.test(r));
    if (missing.length) console.log(`control reach: a one-history set is red — ${missing[0]}`);
    else bad.push('reach: a one-history set reaches every claimed shape, so the reach check cannot fail');
    const { L: tight } = await timedRun(HTML, histories.slice(0, 1), b, 0);
    if (tight.reds.get('D-BUDGET').length) console.log('control budget: D-BUDGET red with a budget of 0 ms');
    else bad.push('budget: D-BUDGET stayed green with a budget of 0 ms');
  } finally { b.close(); rmQuiet(profile); rmQuiet(work); }
  const elapsedMs = performance.now() - t0;
  const over = overBudget('selftest wall', elapsedMs, CONFIG.budgetMs);
  if (over) bad.push(over);
  if (bad.length) { console.error(`RED: selftest, ${bad.length} problems`); bad.forEach(x => console.error(' - ' + x)); return 1; }
  console.log(`GREEN: selftest, ${MUTANTS.length} mutants, each went red; ${Math.round(elapsedMs)} ms of ${CONFIG.budgetMs} ms`);
  return 0;
}

export { INSTALL, LOAD, statedHistories, visits };
if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  if (process.argv.includes('--selftest')) process.exit(await selftest());
  else await main();
}
