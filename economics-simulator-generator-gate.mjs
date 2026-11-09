#!/usr/bin/env node
/*
 * economics-simulator-generator-gate.mjs — a seeded generator plays many
 * reachable histories through the PRODUCTION core of the simulator and checks,
 * after every step, the laws the Lean proves.
 *
 * The generator carries no transition of its own. Every step it draws is
 * handed to the core module's `applyIntegrated`; the verdicts come from the
 * core's own law checkers (`lawViolations`, `kgLawViolations`,
 * `bgLawViolations`), each tied to its Lean theorem through the claim
 * manifest (`CLAIMS`, `proofState`): a law whose theorem is not proved is
 * reported, never asserted.
 *
 *   - The event surface is DISCOVERED: the constructors of every pinned Lean
 *     event vocabulary (economics-simulator-lean-surface.mjs, the one
 *     discovery the claim gate also imports). The coverage denominator is that
 *     extent; a discovered constructor the generator never produced and the
 *     core never applied is RED naming it. Vocabularies the simulator does not
 *     present are reported with their reason.
 *   - Each step is drawn from what the current state enables (members, roles,
 *     collections, pledges, open and bound questions, live authorizations).
 *     A fixed minority (INVALID_SHARE) is deliberately invalid; the core must
 *     refuse each, and every refusal must leave the aggregate unchanged.
 *   - A violation prints seed, trace, step, the event, the law and its
 *     theorem and the event prefix as a reactivegas-integrated.trace v1
 *     envelope, labelled a transcription divergence between the JS core and
 *     the Lean: a divergence is investigated, never made green by weakening
 *     a law.
 *   - The run is deterministic (seeded PRNG, the fixed configuration below),
 *     its output is the same text twice, and its wall time must stay inside
 *     BUDGET_MS.
 *
 * Usage:
 *   node economics-simulator-generator-gate.mjs             # the CI run
 *   node economics-simulator-generator-gate.mjs --selftest  # the controls
 *   node economics-simulator-generator-gate.mjs --selftest --only <control>
 *
 * --selftest shows the gate can fail where it claims to: a constructor the
 * generator cannot produce, an omitted production, core mutants (a refund one
 * short, a doubled approval, a proposer counted above one admin, a refusal
 * that changes state), invalid steps the core wrongly accepts, a law whose
 * theorem is not proved, a nondeterministic comparison and an exceeded budget
 * — each RED for its stated reason on the production generator and a one-edit
 * copy of the production core — then the unmodified gate GREEN.
 */

import { readFileSync, writeFileSync, mkdtempSync, rmSync } from 'node:fs';
import { createHash } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { tmpdir } from 'node:os';
import { deriveCorePin, assertCorePinReachable, discoverVocabularies,
  NOT_PRESENTED_VOCABULARIES } from './economics-simulator-lean-surface.mjs';

const REPO = dirname(fileURLToPath(import.meta.url));
const CORE = resolve(REPO, 'economics-simulator-core.mjs');
const CLAIM_GATE = resolve(REPO, 'economics-simulator-claim-gate.mjs');
const sha256 = s => createHash('sha256').update(s).digest('hex');
const clone = x => JSON.parse(JSON.stringify(x));

/* --- the stated configuration (G1-DET, G1-BUDGET, G1-REACH) ------------------ */
const SEEDS = Array.from({ length: 32 }, (_, i) => i + 1);
const CONFIG = Object.freeze({
  seeds: SEEDS,
  tracesPerSeed: 8,
  stepsPerTrace: 120,
  invalidShare: [1, 8],       // one step in eight is deliberately invalid
  budgetMs: 60000,            // wall budget of this gate inside `just simulator`
});

/* --- seeded PRNG: mulberry32 ------------------------------------------------- */
function prng(seed) {
  let a = seed >>> 0;
  return () => {
    a = (a + 0x6D2B79F5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
const pick = (r, xs) => xs[Math.floor(r() * xs.length)];
const upto = (r, lo, hi) => lo + Math.floor(r() * (hi - lo + 1));

/* --- the laws the generator asserts, each tied to its Lean theorem ----------- */
/* a message the core's checkers print begins with `prefix`; `claim` is the
   CLAIMS row whose Lean declaration states the law */
const LAWS = [
  ['conservation (L6)', 'conservation'],
  ['solvent (L7)', 'solvency'],
  ['uniquePledges (L8)', 'l8-preserved'],
  ['QuestionClean', 'vote-clean'],
  ['domanda aperta con verdetto deciso', 'vote-nostale'],
  ['id aperti duplicati', 'vote-partition'],
  ['id chiusi duplicati', 'vote-partition'],
  ['partizione aperte/chiuse violata', 'vote-partition'],
  ['record di chiusura con verdetto open', 'vote-partition'],
  ['approvals_nodup', 'kg-nodup'],
  ['proposer_absent_above_one', 'kg-proposer'],
];
const REFUSAL_LAW = { law: 'refusal leaves the aggregate unchanged (S-12)', claim: 'atomic' };

function anchorOf(mod, claim) {
  const row = mod.CLAIMS[claim];
  return row ? `${row.d} (${row.f}:${row.l}, ${row.k}, ${mod.proofState(claim)})` : `${claim} (no claim row)`;
}

/* every violation the core's own checkers report on an aggregate, classified
   by law; a message no law claims is itself a violation (unmapped) */
function lawFindings(mod, gs) {
  const raw = [
    ...mod.lawViolations(gs.appFold),
    ...mod.kgLawViolations({ members: gs.members }, gs.appFold.votes),
    ...mod.bgLawViolations(gs),
  ];
  const asserted = [], notAsserted = [];
  for (const text of raw) {
    const law = LAWS.find(([prefix]) => text.startsWith(prefix));
    if (!law) { asserted.push({ law: 'unmapped law message', claim: null, text }); continue; }
    (mod.proofState(law[1]) === 'ok' ? asserted : notAsserted).push({ law: law[0], claim: law[1], text });
  }
  return { asserted, notAsserted };
}

/* --- constructor names an integrated event exercises ------------------------- */
const namesOf = ev => {
  const top = Object.keys(ev)[0];
  const inner = ev[top];
  if (top === 'propose') return [top, Object.keys(inner.proposal)[0]];
  if (top === 'direct' || top === 'app') return [top, Object.keys(inner)[0]];
  return [top];
};

/* --- productions: what the current state enables ------------------------------ */
/* each returns { signer, ev } drawn from the state, or null when nothing in the
   state enables it; whether the core accepts the step is the core's decision */
const KEYS = ['anna', 'bruno', 'carla', 'dora', 'elena', 'fabio', 'gaia', 'hugo'];
const ROLE_SETS = [
  [{ appRole: { name: 'socio' } }],
  [{ adminRole: { admin: 'publicAdmin' } }],
  [],
];
const ADMIN_ROLES = [{ adminRole: { admin: 'publicAdmin' } }];
const app = (signer, ctor, args) => ({ signer, ev: { app: { [ctor]: args } } });
const live = (c, verdict, kind) => c.live.filter(a => a.verdict === verdict && kind in a.target);

const PRODUCTIONS = {
  admitMember: c => {
    const fresh = KEYS.filter(k => !c.keys.includes(k));
    if (!fresh.length || !c.admins.length) return null;
    const key = pick(c.r, fresh);
    return { signer: pick(c.r, c.admins),
      ev: { direct: { admitMember: { key, email: key + '@toy.example', roles: pick(c.r, ROLE_SETS) } } } };
  },
  /* the walk keeps a group that can still act: the sole admin neither leaves
     nor loses the role through a draw */
  departure: c => {
    const keys = c.keys.filter(k => k !== c.sole);
    return c.admins.length && keys.length
      ? { signer: pick(c.r, c.admins), ev: { propose: { proposal: { departure: pick(c.r, keys) } } } } : null;
  },
  changeRoles: c => {
    if (!c.admins.length || !c.keys.length) return null;
    const key = pick(c.r, c.keys);
    const roles = key === c.sole ? ADMIN_ROLES : pick(c.r, ROLE_SETS);
    return { signer: pick(c.r, c.admins), ev: { propose: { proposal: { changeRoles: { key, roles: clone(roles) } } } } };
  },
  approve: c => {
    const open = c.pending.filter(([, p]) => c.admins.some(a => !p.approvals.includes(a)));
    if (!open.length) return null;
    const [pid, p] = pick(c.r, open);
    const pool = c.admins.filter(a => !p.approvals.includes(a));
    const others = pool.filter(a => a !== p.proposer);
    return { signer: pick(c.r, others.length ? others : pool), ev: { approve: { proposalId: pid } } };
  },
  openPurchase: c => {
    const free = [1, 2, 3, 4, 5].filter(id => !c.cols.some(col => col.id === id));
    return free.length && c.admins.length ? app(pick(c.r, c.admins), 'openPurchase', { c: pick(c.r, free) }) : null;
  },
  grantPermission: c => {
    const a = live(c, 'positive', 'permission');
    return a.length && c.admins.length ? app(pick(c.r, c.admins), 'grantPermission', { c: pick(c.r, a).target.permission.c }) : null;
  },
  denyPermission: c => {
    const a = live(c, 'negative', 'permission');
    return a.length && c.admins.length ? app(pick(c.r, c.admins), 'denyPermission', { c: pick(c.r, a).target.permission.c }) : null;
  },
  deposit: c => {
    const user = pick(c.r, c.keys);
    const signers = c.admins.filter(a => a !== user);
    return signers.length ? app(pick(c.r, signers), 'deposit', { user, v: upto(c.r, 1, 20) }) : null;
  },
  withdraw: c => {
    const funded = c.keys.filter(k => c.bal(k) > 0);
    if (!funded.length) return null;
    const user = pick(c.r, funded);
    const signers = c.admins.filter(a => a !== user);
    return signers.length ? app(pick(c.r, signers), 'withdraw', { user, v: upto(c.r, 1, c.bal(user)) }) : null;
  },
  transferCassa: c => {
    if (c.admins.length < 2) return null;
    const signer = pick(c.r, c.admins);
    return app(signer, 'transferCassa', { from_: pick(c.r, c.admins.filter(a => a !== signer)), v: upto(c.r, 1, 10) });
  },
  donate: c => c.admins.length ? app(pick(c.r, c.admins), 'donate', { v: upto(c.r, 1, 30) }) : null,
  backdonate: c => {
    const a = live(c, 'positive', 'backdonation');
    return a.length && c.admins.length ? app(pick(c.r, c.admins), 'backdonate', { w: pick(c.r, a).target.backdonation.w }) : null;
  },
  pledge: c => {
    const funded = c.keys.filter(k => c.bal(k) > 0);
    if (!funded.length || !c.cols.length || !c.admins.length) return null;
    const user = pick(c.r, funded);
    return app(pick(c.r, c.admins), 'pledge',
      { user, c: pick(c.r, c.cols).id, v: upto(c.r, 1, c.bal(user)) });
  },
  acceptPledge: c => pledgeOf(c, 'pending', 'acceptPledge'),
  refusePledge: c => pledgeOf(c, 'pending', 'refusePledge'),
  correctPledge: c => {
    const col = pick(c.r, c.cols.filter(x => x.accepted.length));
    if (!col) return null;
    const p = pick(c.r, col.accepted);
    return app(col.referente, 'correctPledge', { user: p.user, c: col.id, v: upto(c.r, 0, p.amount + c.bal(p.user)) });
  },
  closePurchase: c => {
    const col = pick(c.r, c.cols.filter(x => x.permitted && !x.pending.length));
    return col ? app(col.referente, 'closePurchase', { c: col.id }) : null;
  },
  failPurchase: c => {
    const col = pick(c.r, c.cols.filter(x => !x.pending.length));
    return col ? app(col.referente, 'failPurchase', { c: col.id }) : null;
  },
  openQuestion: c => c.admins.length
    ? app(pick(c.r, c.admins), 'openQuestion', { questionId: c.freshQ(), kind: questionKind(c) }) : null,
  cast: c => {
    const open = c.open.filter(([, q]) => typeof q.kind === 'object' && q.kind.permission
      ? c.admins.includes(q.kind.permission.designee) : c.admins.length);
    if (!open.length) return null;
    const [qid, q] = pick(c.r, open);
    const designee = typeof q.kind === 'object' && q.kind.permission ? q.kind.permission.designee : null;
    const signer = designee || (c.admins.length ? pick(c.r, c.admins) : null);
    return signer ? app(signer, 'cast', { questionId: qid, ballot: ballotOf(c.r) }) : null;
  },
  renounce: c => {
    const open = c.open.filter(([, q]) => c.admins.includes(q.proposer));
    if (!open.length) return null;
    const [qid, q] = pick(c.r, open);
    return app(q.proposer, 'renounce', { questionId: qid });
  },
  openBound: c => {
    if (!c.admins.length) return null;
    const target = c.cols.length && c.r() < 0.7
      ? { permission: { c: pick(c.r, c.cols).id } } : { backdonation: { w: upto(c.r, 1, 2) } };
    return app(pick(c.r, c.admins), 'openBound',
      { questionId: c.freshQ(), kind: questionKind(c), target });
  },
};
const ballotOf = r => r() < 0.7 ? 'assent' : 'dissent';
function pledgeOf(c, side, ctor) {
  const col = pick(c.r, c.cols.filter(x => x[side].length));
  if (!col) return null;
  const p = pick(c.r, col[side]);
  return app(col.referente, ctor, { user: p.user, c: col.id });
}
function questionKind(c) {
  return c.admins.length && c.r() < 0.5 ? { permission: { designee: pick(c.r, c.admins) } } : 'collective';
}

/* deliberately invalid steps: each states the Lean anchor of the refusal it
   needs and is drawn only when the state offers the occasion */
const INVALIDS = {
  renounceByNonProposer: { claim: 'vote-validate', draw: c => {
    const open = c.open.filter(([, q]) => c.keys.some(k => k !== q.proposer));
    if (!open.length) return null;
    const [qid, q] = pick(c.r, open);
    return app(pick(c.r, c.keys.filter(k => k !== q.proposer)), 'renounce', { questionId: qid });
  } },
  ballotByNonDesignee: { claim: 'vote-validate', draw: c => {
    const open = c.open.filter(([, q]) => typeof q.kind === 'object' && q.kind.permission &&
      c.admins.some(a => a !== q.kind.permission.designee));
    if (!open.length) return null;
    const [qid, q] = pick(c.r, open);
    return app(pick(c.r, c.admins.filter(a => a !== q.kind.permission.designee)), 'cast',
      { questionId: qid, ballot: pick(c.r, ['assent', 'dissent']) });
  } },
  unboundGrant: { claim: 'inv-grant', draw: c => {
    if (!c.admins.length) return null;
    const bound = live(c, 'positive', 'permission').map(a => a.target.permission.c);
    const free = [1, 2, 3, 4, 5].filter(id => !bound.includes(id));
    return free.length ? app(pick(c.r, c.admins), 'grantPermission', { c: pick(c.r, free) }) : null;
  } },
  spentAuthorization: { claim: 'inv-grant', draw: c => {
    const again = c.spent.filter(s => !c.live.some(a => JSON.stringify([a.target, a.verdict]) === s.key));
    return again.length ? pick(c.r, again).step : null;
  } },
  approveOwnProposal: { claim: 'kg-approve-guard', draw: c => {
    if (c.admins.length < 2) return null;
    const own = c.pending.filter(([, p]) => c.admins.includes(p.proposer));
    if (!own.length) return null;
    const [pid, p] = pick(c.r, own);
    return { signer: p.proposer, ev: { approve: { proposalId: pid } } };
  } },
  approveTwice: { claim: 'kg-approve-guard', draw: c => {
    const done = c.pending.filter(([, p]) => p.approvals.length);
    if (!done.length) return null;
    const [pid, p] = pick(c.r, done);
    return { signer: pick(c.r, p.approvals), ev: { approve: { proposalId: pid } } };
  } },
  strangerSigner: { claim: 'auth', draw: c =>
    app('zz-' + upto(c.r, 1, 9), 'donate', { v: upto(c.r, 1, 30) }) },
};

/* --- the surface: discovered extent plus the productions that cover it ------- */
function buildSurface({ repo = REPO, productions = PRODUCTIONS, invalids = INVALIDS } = {}) {
  const pin = deriveCorePin(repo, 'HEAD').commit;
  assertCorePinReachable(pin, repo);
  const all = discoverVocabularies(repo, pin);
  const routed = all.filter(v => !(v.key in NOT_PRESENTED_VOCABULARIES));
  const notPresented = all.filter(v => v.key in NOT_PRESENTED_VOCABULARIES)
    .map(v => ({ key: v.key, reason: NOT_PRESENTED_VOCABULARIES[v.key] }));
  return { pin, routed, notPresented, productions, invalids };
}

/* --- one seeded trace through the production core ---------------------------- */
function stateContext(mod, gs, r, extra) {
  const keys = mod.memberKeys({ members: gs.members });
  const votes = gs.appFold.votes;
  const admins = keys.filter(k => mod.isAdminView(k, { members: gs.members }));
  return { r, keys, admins, sole: admins.length === 1 ? admins[0] : null,
    cols: gs.appFold.collections, live: gs.appFold.live || [], open: votes.openQuestions,
    pending: gs.pendingBase,
    bal: k => mod.bal(gs.appFold.conti, k),
    freshQ: () => `q${extra.questions++}`, spent: extra.spent };
}

function envelopeOf(mod, steps) {
  let gs = mod.bootAggregate();
  const out = [];
  for (const { signer, ev } of steps) {
    const input = clone(gs);
    const det = mod.applyIntegrated(gs, signer, clone(ev));
    if (det.refused) break;
    out.push({ input, signer, event: clone(ev), result: { tag: 'applied', aggregate: clone(det.gs), change: det.change } });
    gs = det.gs;
  }
  return { schema: 'reactivegas-integrated.trace', version: 1, initial: mod.bootAggregate(), steps: out };
}

function divergence(mod, base, found) {
  const prefix = base.prefix.concat(found.applied ? [found.step] : []);
  const envelope = envelopeOf(mod, prefix);
  let replays = true;
  try { mod.verifyIntegratedV1(envelope); } catch { replays = false; }
  return { label: 'TRANSCRIPTION DIVERGENCE', seed: base.seed, step: base.index, signer: found.step.signer,
    event: found.step.ev, law: found.law, claim: found.claim, theorem: found.theorem, kind: found.kind || null,
    text: found.text, envelope, replays };
}

function generateTrace(seed, config, surface) {
  const mod = config.core;
  const r = prng(seed);
  const extra = { questions: 0, spent: [] };
  const tally = {}, kinds = {}, notAsserted = new Set();
  const prefix = [];
  let gs = mod.bootAggregate();
  const out = { steps: 0, applied: 0, refused: 0, invalidRefused: 0, tally, kinds, notAsserted, report: null };
  const bump = (name, field) => {
    const t = tally[name] || (tally[name] = { attempted: 0, applied: 0 });
    t[field]++;
  };
  for (let index = 0; index < config.stepsPerTrace; index++) {
    const c = stateContext(mod, gs, r, extra);
    const wantInvalid = r() * config.invalidShare[1] < config.invalidShare[0];
    const choose = table => {
      const cands = Object.entries(table).map(([name, p]) => [name, (p.draw || p)(c)])
        .filter(([, step]) => step);
      if (!cands.length) return null;
      return cands[Math.floor(r() * cands.length)];
    };
    const invalid = wantInvalid ? choose(surface.invalids) : null;
    const drawn = invalid || choose(surface.productions);
    if (!drawn) break;
    const [name, step] = drawn;
    const isInvalid = !!invalid;
    const before = mod.canonAggregate(gs);
    const liveBefore = (gs.appFold.live || []).length;
    for (const n of namesOf(step.ev)) bump(n, 'attempted');
    out.steps++;
    let det;
    const base = { seed, index, prefix };
    try { det = mod.applyIntegrated(gs, step.signer, clone(step.ev)); }
    catch (e) {
      out.report = divergence(mod, base, { step, applied: false, law: 'applyIntegrated is total on generated events',
        claim: null, theorem: null, kind: isInvalid ? name : null, text: `threw: ${e.message}` });
      return out;
    }
    if (det.refused) {
      out.refused++;
      if (mod.canonAggregate(gs) !== before || (det.gs && mod.canonAggregate(det.gs) !== before)) {
        out.report = divergence(mod, base, { step, applied: false, law: REFUSAL_LAW.law, claim: REFUSAL_LAW.claim,
          theorem: anchorOf(mod, REFUSAL_LAW.claim), kind: isInvalid ? name : null,
          text: `refused (${det.refused}) but the aggregate changed` });
        return out;
      }
      if (isInvalid) { out.invalidRefused++; kinds[name] = (kinds[name] || 0) + 1; }
      continue;
    }
    for (const n of namesOf(step.ev)) bump(n, 'applied');
    const found = lawFindings(mod, det.gs);
    found.notAsserted.forEach(f => notAsserted.add(`${f.law} -> ${f.claim}`));
    if (found.asserted.length) {
      const f = found.asserted[0];
      out.report = divergence(mod, base, { step, applied: true, law: f.law, claim: f.claim,
        theorem: f.claim ? anchorOf(mod, f.claim) : null, kind: isInvalid ? name : null, text: f.text });
      return out;
    }
    if (isInvalid) {
      out.report = divergence(mod, base, { step, applied: true, law: 'invalid step refused', claim: surface.invalids[name].claim,
        theorem: anchorOf(mod, surface.invalids[name].claim), kind: name,
        text: `the core accepted ${name}` });
      return out;
    }
    out.applied++;
    prefix.push(step);
    const key = (det.gs.appFold.live || []).length < liveBefore ? spentKey(gs, det.gs) : '';
    if (key) extra.spent.push({ key, step });
    gs = det.gs;
  }
  return out;
}

/* the authorization a step spent: live before, gone after, its target still
   present — an authorization whose collection left was pruned, not spent */
function spentKey(pre, post) {
  const key = a => JSON.stringify([a.target, a.verdict]);
  const left = (post.appFold.live || []).map(key);
  const present = t => !('permission' in t) || post.appFold.collections.some(col => col.id === t.permission.c);
  const gone = (pre.appFold.live || []).find(a => {
    const i = left.indexOf(key(a));
    if (i < 0) return present(a.target);
    left.splice(i, 1);
    return false;
  });
  return gone ? key(gone) : '';
}

/* --- the gate: all traces, coverage, budget ---------------------------------- */
async function loadCore(corePath) {
  return import(`${pathToFileURL(corePath).href}?audit=${Date.now()}-${Math.random()}`);
}

function runGenerator({ mod, surface, config = CONFIG, now = () => performance.now() }) {
  const t0 = now();
  const total = { steps: 0, applied: 0, refused: 0, invalidRefused: 0, tally: {}, kinds: {}, notAsserted: new Set() };
  let report = null;
  traces:
  for (const seed of config.seeds)
    for (let k = 0; k < config.tracesPerSeed; k++) {
      const res = generateTrace(seed * 1000003 + k, { ...config, core: mod }, surface);
      for (const f of ['steps', 'applied', 'refused', 'invalidRefused']) total[f] += res[f];
      for (const [n, t] of Object.entries(res.tally)) {
        const m = total.tally[n] || (total.tally[n] = { attempted: 0, applied: 0 });
        m.attempted += t.attempted; m.applied += t.applied;
      }
      for (const [n, v] of Object.entries(res.kinds)) total.kinds[n] = (total.kinds[n] || 0) + v;
      res.notAsserted.forEach(x => total.notAsserted.add(x));
      if (res.report) { report = { ...res.report, trace: k }; break traces; }
    }
  const elapsedMs = now() - t0;
  const reasons = [];
  if (report) reasons.push(`violation: ${report.law} at seed ${report.seed} step ${report.step}`);
  if (elapsedMs > config.budgetMs)
    reasons.push(`budget exceeded: ${Math.round(elapsedMs)} ms > ${config.budgetMs} ms`);
  if (!report) {
    const names = new Set(surface.routed.flatMap(v => v.constructors));
    for (const v of surface.routed)
      for (const c of v.constructors) {
        const t = total.tally[c];
        if (!t || !t.attempted) reasons.push(`${v.vocabulary}.${c}: discovered constructor never produced`);
        else if (!t.applied) reasons.push(`${v.vocabulary}.${c}: produced but never applied by the core`);
      }
    for (const p of Object.keys(surface.productions))
      if (!names.has(p)) reasons.push(`production ${p}: no discovered constructor of that name`);
    for (const kind of Object.keys(surface.invalids))
      if (!total.kinds[kind]) reasons.push(`invalid step ${kind}: never produced and refused`);
  }
  return { ok: reasons.length === 0, reasons, report, total, elapsedMs };
}

function describe(surface, result) {
  const lines = [];
  const ctors = surface.routed.reduce((n, v) => n + v.constructors.length, 0);
  lines.push(`surface: pin=${surface.pin.slice(0, 10)} vocabularies=${surface.routed.length} constructors=${ctors}`);
  for (const v of surface.routed) lines.push(`  ${v.vocabulary}: ${v.constructors.join(' ')}`);
  for (const n of surface.notPresented) lines.push(`not presented: ${n.key} — ${n.reason}`);
  const t = result.total;
  lines.push(`steps=${t.steps} applied=${t.applied} refused=${t.refused} invalid-refused=${t.invalidRefused}`);
  for (const [k, v] of Object.entries(t.kinds).sort()) lines.push(`  invalid ${k}: refused ${v}`);
  for (const v of surface.routed)
    lines.push(`  ${v.vocabulary}: ` + v.constructors.map(c => `${c}=${(t.tally[c] || {}).applied || 0}`).join(' '));
  for (const x of [...t.notAsserted].sort()) lines.push(`law not asserted (theorem not proved): ${x}`);
  return lines;
}

function printReport(rep) {
  console.error(`${rep.label}: ${rep.law}`);
  console.error(`  seed=${rep.seed} step=${rep.step} signer=${rep.signer} kind=${rep.kind}`);
  console.error(`  event: ${JSON.stringify(rep.event)}`);
  console.error(`  law: ${rep.law}${rep.theorem ? ' — theorem ' + rep.theorem : ''}`);
  console.error(`  ${rep.text}`);
  console.error(`  envelope (replays through the core verifier: ${rep.replays}): ${JSON.stringify(rep.envelope)}`);
}

async function production(corePath = CORE) {
  const mod = await loadCore(corePath);
  const surface = buildSurface();
  const result = runGenerator({ mod, surface });
  const lines = describe(surface, result);
  lines.push(`digest ${sha256(lines.join('\n')).slice(0, 16)}`);
  return { surface, result, lines };
}

/* --- selftest ---------------------------------------------------------------- */
function mutantCore(work, name, edits) {
  let src = readFileSync(CORE, 'utf8');
  for (const [needle, replacement] of edits) {
    if (src.split(needle).length !== 2)
      throw new Error(`mutant ${name}: edit did not apply exactly once: ${needle.slice(0, 60)}`);
    src = src.replace(needle, () => replacement);
  }
  const file = join(work, `core-${name}.mjs`);
  writeFileSync(file, src);
  return file;
}

const MUTANTS = [
  { name: 'refund-short-by-one', law: /^conservation/,
    edits: [['bump(acc, p.user, p.amount), m)', 'bump(acc, p.user, p.amount - 1), m)']] },
  { name: 'approval-list-duplicated', law: /^(approvals_nodup|invalid step refused)/,
    edits: [["return pend.approvals.includes(signer) ? 'alreadyApproved' : null;", 'return null;'],
      ['approvals: vtSetInsert(signer, pend.approvals)', 'approvals: [signer, ...pend.approvals]']] },
  { name: 'proposer-counted-above-one-admin', law: /^(proposer_absent_above_one|invalid step refused)/,
    edits: [["if (signer === pend.proposer && bgAdminCount(gs) > 1) return 'proposerSelfApproval';", '']] },
  { name: 'refusal-changes-state', law: /^refusal leaves the aggregate unchanged/,
    edits: [["if (!res.ok) return { refused: res.verr || 'rejected', failed: res.failed };",
      "if (!res.ok) { gs.appFold.conti.push(['zz', 1]); return { refused: res.verr || 'rejected', failed: res.failed }; }"]] },
  { name: 'renounce-by-non-proposer-accepted', law: /^invalid step refused/, kind: 'renounceByNonProposer',
    edits: [["return signer === q.proposer ? null : 'notProposer';", 'return null;']] },
  { name: 'ballot-by-non-designee-accepted', law: /^invalid step refused/, kind: 'ballotByNonDesignee',
    edits: [["return perm && signer !== perm.designee ? 'notDesignee' : null;", 'return null;']] },
];

/* every two-sided control starts from the unaltered production generator being
   GREEN: a control that passes because nothing is produced judges nothing */
async function greenBaseline(config) {
  const res = runGenerator({ mod: await loadCore(CORE), surface: buildSurface(), config });
  if (!res.ok) throw new Error('control mal costruito: the unaltered generator is not GREEN: ' + res.reasons.slice(0, 3).join('; '));
  return res;
}

const SMALL = { ...CONFIG, seeds: [1, 2], tracesPerSeed: 2, stepsPerTrace: 40 };

const CONTROLS = [
  { name: 'surface-shared', run: async () => {
    const extra = JSON.stringify({ 'lean/KelGroups/ZzSurface.lean':
      'inductive ZzSurfaceEvent where\n  | zzProbe\nderiving Repr\n' });
    const claimVocab = env => JSON.parse(execFileSync('node', [CLAIM_GATE, '--vocabularies'],
      { encoding: 'utf8', env: { ...process.env, ...env } }))
      .find(v => v.vocabulary === 'ZzSurfaceEvent');
    if (claimVocab({})) throw new Error('claim gate shows the probe vocabulary without the injected source');
    const seen = claimVocab({ RG_SURFACE_EXTRA: extra });
    if (!seen || seen.constructors.join() !== 'zzProbe')
      throw new Error('claim gate did not discover the injected vocabulary: its discovery is not the shared one');
    process.env.RG_SURFACE_EXTRA = extra;
    let mine;
    try { mine = buildSurface().routed.find(v => v.vocabulary === 'ZzSurfaceEvent'); }
    finally { delete process.env.RG_SURFACE_EXTRA; }
    if (!mine || mine.constructors.join() !== 'zzProbe')
      throw new Error('generator did not discover the injected vocabulary');
    return 'claim gate and generator both discover the injected ZzSurfaceEvent.zzProbe, neither without it';
  } },
  { name: 'unproduced-constructor', run: async () => {
    await greenBaseline();
    process.env.RG_SURFACE_EXTRA = JSON.stringify({ 'lean/KelGroups/ZzSurface.lean':
      'inductive ZzSurfaceEvent where\n  | zzProbe\nderiving Repr\n' });
    let surface;
    try { surface = buildSurface(); } finally { delete process.env.RG_SURFACE_EXTRA; }
    const res = runGenerator({ mod: await loadCore(CORE), surface });
    if (!res.reasons.some(x => /^ZzSurfaceEvent\.zzProbe: discovered constructor never produced/.test(x)))
      throw new Error(`a discovered constructor the generator cannot produce was not RED: ${res.reasons.join('; ')}`);
    return 'RED: ' + res.reasons.filter(x => /zzProbe/.test(x)).join('; ');
  } },
  { name: 'omitted-constructor', run: async () => {
    const full = buildSurface();
    const app = full.routed.find(v => v.vocabulary === 'AppEvent');
    const victim = app.constructors[app.constructors.length - 1];
    await greenBaseline();
    const { [victim]: _dropped, ...productions } = full.productions;
    const res = runGenerator({ mod: await loadCore(CORE), surface: buildSurface({ productions }) });
    if (!res.reasons.some(x => x.startsWith(`AppEvent.${victim}: discovered constructor never produced`)))
      throw new Error(`omitting the ${victim} production was not RED naming it: ${res.reasons.join('; ')}`);
    return `RED naming AppEvent.${victim}`;
  } },
  { name: 'undrawn-invalid-step', run: async () => {
    await greenBaseline();
    const invalids = { ...INVALIDS, neverDrawn: { claim: 'auth', draw: () => null } };
    const res = runGenerator({ mod: await loadCore(CORE), surface: buildSurface({ invalids }) });
    if (!res.reasons.some(x => /^invalid step neverDrawn: never produced and refused/.test(x)))
      throw new Error(`an invalid step that is never drawn was not RED: ${res.reasons.join('; ')}`);
    return 'RED: ' + res.reasons.filter(x => /^invalid step/.test(x)).join('; ');
  } },
  ...MUTANTS.map(m => ({ name: `mutant-${m.name}`, run: async work => {
    const mod = await loadCore(mutantCore(work, m.name, m.edits));
    const res = runGenerator({ mod, surface: buildSurface() });
    const rep = res.report;
    if (!rep) throw new Error(`mutant ${m.name} was not found: ${res.reasons.join('; ') || 'green'}`);
    if (!m.law.test(rep.law)) throw new Error(`mutant ${m.name} found as the wrong law: ${rep.law}`);
    if (m.kind && rep.kind !== m.kind) throw new Error(`mutant ${m.name} found as the wrong step kind: ${rep.kind}`);
    if (res.elapsedMs > CONFIG.budgetMs) throw new Error(`mutant ${m.name} found after the CI budget`);
    if (!rep.replays) throw new Error(`mutant ${m.name}: the reported envelope does not replay through the core verifier`);
    return `found: ${rep.law} at seed ${rep.seed} step ${rep.step}`;
  } })),
  { name: 'law-without-proved-theorem', run: async work => {
    const real = await loadCore(mutantCore(work, 'refund-short-by-one', MUTANTS[0].edits));
    const mod = { ...real, proofState: id => id === 'conservation' ? 'enun' : real.proofState(id) };
    const res = runGenerator({ mod, surface: buildSurface() });
    if (res.report && /^conservation/.test(res.report.law))
      throw new Error(`a law with no proved theorem was asserted: ${res.report.law}`);
    if (![...res.total.notAsserted].some(x => /^conservation/.test(x)))
      throw new Error('the unproved law was not reported as not asserted');
    return 'conservation reported as not asserted while its theorem is unproved';
  } },
  { name: 'coverage-by-construction', run: async () => {
    const one = { ...CONFIG, seeds: [1], tracesPerSeed: 2, stepsPerTrace: 120 };
    const surface = buildSurface();
    const res = runGenerator({ mod: await loadCore(CORE), surface, config: one });
    const missed = res.reasons.filter(x => /constructor never produced|never applied/.test(x));
    if (res.report || missed.length)
      throw new Error(`two traces of ${one.stepsPerTrace} steps do not cover the extent: ${missed.join('; ')}`);
    return `every discovered constructor applied within ${res.total.steps} steps`;
  } },
  { name: 'every-law-can-fire', run: async () => {
    const mod = await loadCore(CORE);
    const q = (assents, dissents) => ({ kind: 'collective', proposer: 'anna', assents, dissents });
    const rec = (questionId, verdict) => ({ questionId, question: q([], []), verdict, cause: 'tally' });
    const pledge = (user, amount) => ({ user, amount });
    const crafted = {
      'conservation (L6)': ['conservation', a => { a.appFold.casse = [['anna', 5]]; }],
      'solvent (L7)': ['solvency', a => { a.appFold.conti = [['anna', -1]]; }],
      'uniquePledges (L8)': ['l8-preserved', a => { a.appFold.collections = [{ id: 1, referente: 'anna', permitted: false,
        accepted: [pledge('anna', 1)], pending: [pledge('anna', 2)] }]; }],
      'QuestionClean': ['vote-clean', a => { a.appFold.votes.openQuestions = [['a', q(['anna', 'anna'], [])]]; }],
      'domanda aperta con verdetto deciso': ['vote-nostale', a => { a.appFold.votes.openQuestions = [['a', q(['anna'], [])]]; }],
      'id aperti duplicati': ['vote-partition', a => { a.appFold.votes.openQuestions = [['a', q([], [])], ['a', q([], [])]]; }],
      'id chiusi duplicati': ['vote-partition', a => { a.appFold.votes.closed = [rec('a', 'negative'), rec('a', 'negative')]; }],
      'partizione aperte/chiuse violata': ['vote-partition', a => { a.appFold.votes.openQuestions = [['a', q([], [])]];
        a.appFold.votes.closed = [rec('a', 'negative')]; }],
      'record di chiusura con verdetto open': ['vote-partition', a => { a.appFold.votes.closed = [rec('a', 'open')]; }],
      'approvals_nodup': ['kg-nodup', a => { a.pendingBase = [['depart:x', { proposal: { departure: 'x' }, proposer: 'bruno',
        approvals: ['anna', 'anna'] }]]; }],
      'proposer_absent_above_one': ['kg-proposer', a => { a.pendingBase = [['depart:x', { proposal: { departure: 'x' }, proposer: 'anna',
        approvals: ['anna'] }]]; }],
    };
    for (const [prefix] of LAWS)
      if (!crafted[prefix]) throw new Error(`no violating aggregate for the law ${prefix}`);
    for (const [prefix, [claim, make]] of Object.entries(crafted)) {
      const agg = mod.bootAggregate();
      agg.members = [...agg.members, ['bruno', { key: 'bruno', email: 'bruno@toy.example', roles: ADMIN_ROLES }]];
      if (lawFindings(mod, agg).asserted.length) throw new Error('the clean two-admin aggregate violates a law');
      make(agg);
      const hit = lawFindings(mod, agg).asserted.find(f => f.claim === claim && f.law === prefix);
      if (!hit) throw new Error(`the law ${prefix} did not fire on its violating aggregate`);
    }
    return `${Object.keys(crafted).length} laws each fire on a violating aggregate and name their theorem`;
  } },
  { name: 'determinism', run: async () => {
    const mod = await loadCore(CORE);
    const surface = buildSurface();
    const text = seeds => describe(surface, runGenerator({ mod, surface, config: { ...SMALL, seeds } })).join('\n');
    const a = text([1, 2]), b = text([1, 2]), c = text([3, 4]);
    if (a !== b) throw new Error('two runs with the same seeds differ');
    if (a === c) throw new Error('different seeds give identical output: the comparison cannot fail');
    return `two runs identical (${sha256(a).slice(0, 12)}), other seeds differ (${sha256(c).slice(0, 12)})`;
  } },
  { name: 'budget', run: async () => {
    await greenBaseline();
    const res = runGenerator({ mod: await loadCore(CORE), surface: buildSurface(),
      config: { ...SMALL, budgetMs: 0 } });
    if (!res.reasons.some(x => /^budget exceeded/.test(x)))
      throw new Error(`an exceeded budget was not RED: ${res.reasons.join('; ')}`);
    return 'RED: ' + res.reasons.find(x => /^budget exceeded/.test(x));
  } },
];

async function selftest(only) {
  const work = mkdtempSync(join(tmpdir(), 'rg-generator-gate-'));
  const failures = [];
  try {
    const controls = only ? CONTROLS.filter(c => only.includes(c.name)) : CONTROLS;
    if (only && controls.length !== only.length)
      throw new Error('unknown control in --only: ' + only.join(','));
    for (const ctl of controls) {
      try { console.log(`control ${ctl.name}: ${await ctl.run(work)}`); }
      catch (e) { failures.push(`${ctl.name}: ${e.message}`); console.error(`control ${ctl.name}: FAIL ${e.message}`); }
    }
    if (!only) {
      const p = await production();
      if (!p.result.ok) failures.push('unmodified production gate is not GREEN: ' + p.result.reasons.join('; '));
      else console.log(`control production-green: ${p.result.total.steps} steps, ${p.surface.routed.length} vocabularies`);
    }
  } finally {
    try { rmSync(work, { recursive: true, force: true }); } catch { /* housekeeping */ }
  }
  if (failures.length) { console.error(`RED: ${failures.length} controls failed`); return 1; }
  console.log('SELFTEST OK');
  return 0;
}

/* --- CLI --------------------------------------------------------------------- */
let code = 1;
if (process.argv.includes('--selftest')) {
  const i = process.argv.indexOf('--only');
  code = await selftest(i < 0 ? null : process.argv[i + 1].split(','));
} else {
  const p = await production();
  p.lines.forEach(l => console.log(l));
  if (p.result.ok) {
    console.error(`generator wall ${Math.round(p.result.elapsedMs)} ms of ${CONFIG.budgetMs} ms`);
    console.log('GREEN');
    code = 0;
  } else {
    if (p.result.report) printReport(p.result.report);
    console.error(`RED: ${p.result.reasons.length} problemi`);
    p.result.reasons.forEach(x => console.error(' - ' + x));
  }
}
process.exit(code);
