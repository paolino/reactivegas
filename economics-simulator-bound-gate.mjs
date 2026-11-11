#!/usr/bin/env node
/*
 * economics-simulator-bound-gate.mjs — what the page shows of #76's bound
 * questions and live authorizations, #68's assent rule and #81's refund path.
 *
 * Every row is judged on the real page, driven through its own controls in
 * headless Chromium (the browser client is the teaching gate's), and every
 * displayed value is compared with the core state the page holds at that
 * moment (window.RG.state): the expected value is read from the producer at
 * run time, never typed here. A row whose journey cannot reach its control is
 * RED naming the missing control; a row that checked nothing is RED.
 *
 *   S5B-BOUND-SHOWN     each open question shows what it is bound to (a
 *                       purchase's permission, or a quota) and its proposer; a
 *                       question opened without a target shows none
 *   S5B-LIVE-SHOWN      the live authorizations shown are exactly the core's,
 *                       each with verdict and target; spending one removes it
 *                       in the same step; an unbound closure adds none
 *   S5B-68              a new base proposal shows zero approvals; with several
 *                       admins the proposer's own approval is refused with the
 *                       #68 refusal; a sole admin's separate approval enacts
 *   S5B-L7-RENOUNCE     the proposer renounces a question bound to a purchase
 *                       holding pledges: closed negative, a live negative
 *                       authorization, and the deny refunds every pledger
 *   S5B-L7-DEPART       the same after the proposer's departure, whether or
 *                       not the proposer is the purchase's referente
 *   S5B-REACH           each story above ran from the page's controls or from
 *                       a committed scenario the page loads
 *   S5B-FIDELITY        no shown value differs from the core state
 *
 * Usage:
 *   node economics-simulator-bound-gate.mjs [--html PATH]   production run
 *   node economics-simulator-bound-gate.mjs --selftest      controls that the
 *                                                           rows can fail
 */

import { mkdtempSync, readFileSync, readdirSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { tmpdir } from 'node:os';
import { Browser, findChromium, sleep, rmQuiet } from './economics-simulator-teaching-gate.mjs';

const REPO = dirname(fileURLToPath(import.meta.url));
const argVal = flag => {
  const i = process.argv.indexOf(flag);
  return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : null;
};
const HTML = argVal('--html') || join(REPO, 'economics-simulator.html');
const SCENARIOS = join(REPO, 'economics-simulator-scenarios');

/* --- injected helpers: real clicks, as the other gates do ------------------ */

const HELPERS = `
window.__rgClick = async sel => {
  const el = document.querySelector(sel);
  if (!el) throw new Error('controllo mancante: ' + sel);
  if (el.disabled) throw new Error('controllo disabilitato: ' + sel);
  const r = el.getBoundingClientRect();
  el.dispatchEvent(new MouseEvent('click', { bubbles: true,
    clientX: r.x + r.width / 2, clientY: r.y + r.height / 2 }));
  await new Promise(x => setTimeout(x, 70));
};
window.__rgHome = async () => {
  const el = document.querySelector('#crumbs [data-crumb="0"]');
  if (el && !el.disabled) await __rgClick('#crumbs [data-crumb="0"]');
};
window.__rgAmount = async v => {
  const n = document.getElementById('am-n');
  if (!n) throw new Error('controllo mancante: importo');
  n.value = String(v);
  n.dispatchEvent(new Event('input', { bubbles: true }));
  await __rgClick('#am-ok');
};
window.__rgAddNamed = async name => {
  await __rgClick('#gtasks [data-task="admitMember"]');
  const n = document.getElementById('un-n');
  if (!n) throw new Error('controllo mancante: nome');
  n.value = name;
  n.dispatchEvent(new Event('input', { bubbles: true }));
  await __rgClick('#un-ok');
  await __rgClick('#pop .chip[data-id="anna"]');
};
'helpers-ok';
`;

const C = sel => `await __rgClick(${JSON.stringify(sel)});`;
const CHIP = id => C(`#pop .chip[data-id="${id}"]`);
const HOME = 'await __rgHome();';
const DEPOSIT = (cassiere, user, v) =>
  C(`[data-key="member:${cassiere}"]`) + C('[data-hat="cassiere"]') +
  C('#hat-cassiere [data-task="deposit"]') + CHIP(user) + `await __rgAmount(${v});` + HOME;
const ELECT = (target, proposer, approver) =>
  C('#gtasks [data-task="elect"]') + CHIP(target) + CHIP(proposer) +
  C(`#govcard [data-bgapprove][data-bgsigner="${approver}"]`);
const PLEDGE = (c, user, v) =>
  C(`[data-key="pile:${c}"]`) + C('#view [data-task="pledge"]') + CHIP(user) + CHIP(user) +
  `await __rgAmount(${v});` + HOME;

/* the shared fixture: two responsabili (anna, bruno), two other members with
   funds, one purchase (Olio, referente bruno) holding an accepted pledge of
   carla (30) and a pending one of dora (20) */
const FIXTURE =
  `await __rgAddNamed('Bruno');` + ELECT('bruno', 'anna', 'anna') +
  `await __rgAddNamed('Carla');` + `await __rgAddNamed('Dora');` +
  DEPOSIT('anna', 'carla', 100) + DEPOSIT('anna', 'dora', 100) +
  C('[data-key="cassa:bruno"]') + C('[data-hat="cassiere"]') +
  C('#hat-cassiere [data-task="openPurchase"]') + C('#pop [data-l="Olio"]') + HOME +
  PLEDGE(1, 'carla', 30) +
  C('[data-key="pile:1"]') + C('#refpanel [data-act="accept"][data-u="carla"]') + HOME +
  PLEDGE(1, 'dora', 20);
const OPEN_BOUND = proposer =>
  C('[data-key="pile:1"]') + C('#view [data-kgpropose]') + CHIP(proposer);

/* --- observation: the core state and what the page shows of it ------------- */

const OBSERVE = `(() => {
  const st = RG.state;
  const txt = el => el.textContent.replace(/\\s+/g, ' ').trim();
  const lastEntry = document.querySelector('#log .entry');
  return JSON.stringify({
    conti: st.conti,
    collections: st.collections.map(c => ({ id: c.id, referente: c.referente,
      accepted: c.accepted, pending: c.pending })),
    open: st.votes.openQuestions.map(([qid, q]) => ({ qid, proposer: q.proposer })),
    closed: st.votes.closed.map(r => ({ qid: r.questionId, verdict: r.verdict, cause: r.cause })),
    bindings: st.bindings, live: st.live,
    approvals: RG.bg.pendingBase.map(([pid, p]) => ({ pid, proposer: p.proposer, approvals: p.approvals })),
    members: RG.bg.members.map(([k]) => k),
    questions: [...document.querySelectorAll('[data-qid]')].map(el => ({
      qid: el.dataset.qid, inGov: !!el.closest('#govcard'),
      bind: el.querySelector('.qbind') ? txt(el.querySelector('.qbind')) : null,
      prop: el.querySelector('.qprop') ? txt(el.querySelector('.qprop')) : null,
      renounce: [...el.querySelectorAll('[data-kgrenounce]')].map(b => b.dataset.kgsigner) })),
    lives: [...document.querySelectorAll('#govcard .liveauth')].map(txt),
    proposals: [...document.querySelectorAll('#govcard .vq')].filter(el => txt(el).startsWith('⚖')).map(el => ({
      text: txt(el),
      approvers: [...el.querySelectorAll('[data-bgapprove]')].map(b => b.dataset.bgsigner) })),
    govText: document.getElementById('govcard') ? txt(document.getElementById('govcard')) : '',
    last: lastEntry ? { rej: lastEntry.classList.contains('rej'), text: txt(lastEntry) } : null,
    shownConti: Object.fromEntries(RG.bg.members.map(([k]) => {
      const el = document.querySelector('[data-key="conto:' + k + '"]');
      const m = el && txt(el).match(/(-?\\d+)\\s*€/);
      return [k, m ? Number(m[1]) : null];
    })),
    nm: Object.fromEntries(RG.bg.members.map(([k]) => [k, nm(k)])),
    lbl: Object.fromEntries(st.collections.map(c => [c.id, lbl(c.id)])),
  });
})()`;

/* --- rows ------------------------------------------------------------------ */

const ROWS = ['S5B-BOUND-SHOWN', 'S5B-LIVE-SHOWN', 'S5B-68', 'S5B-L7-RENOUNCE',
  'S5B-L7-DEPART', 'S5B-REACH', 'S5B-FIDELITY'];

function newLedger() {
  const reds = new Map(ROWS.map(r => [r, []]));
  const checks = new Map(ROWS.map(r => [r, 0]));
  return {
    reds, checks,
    ok(rows, cond, why) {
      for (const row of [].concat(rows)) {
        checks.set(row, checks.get(row) + 1);
        if (!cond) reds.get(row).push(why);
      }
    },
  };
}

const parse = raw => JSON.parse(raw);
const targetWords = (t, snap) => 'permission' in t
  ? ['permesso', `«${snap.lbl[t.permission.c]}»`]
  : ['quota', String(t.backdonation.w)];
const tkey = t => 'permission' in t ? `permission:${t.permission.c}` : `backdonation:${t.backdonation.w}`;
const verdictWord = v => v === 'positive' ? 'positiva' : 'negativa';

/* every open question is shown with what the core binds it to (or nothing),
   and its proposer */
function checkQuestions(L, snap, where) {
  const bound = new Map(snap.bindings.map(([q, t]) => [q, t]));
  L.ok('S5B-BOUND-SHOWN', snap.open.length > 0, `${where}: nessuna domanda aperta da giudicare`);
  for (const q of snap.open) {
    const shown = snap.questions.filter(x => x.qid === q.qid);
    L.ok('S5B-BOUND-SHOWN', shown.length > 0, `${where}: «${q.qid}» aperta ma non mostrata`);
    for (const x of shown) {
      const t = bound.get(q.qid);
      if (t) {
        const words = targetWords(t, snap);
        L.ok(['S5B-BOUND-SHOWN', 'S5B-FIDELITY'], x.bind !== null && words.every(w => x.bind.includes(w)),
          `${where}: «${q.qid}» è legata a ${tkey(t)} ma la pagina mostra «${x.bind}» (attese: ${words.join(' + ')})`);
      } else {
        L.ok(['S5B-BOUND-SHOWN', 'S5B-FIDELITY'], x.bind === null,
          `${where}: «${q.qid}» non ha un bersaglio ma la pagina mostra «${x.bind}»`);
      }
      L.ok('S5B-BOUND-SHOWN', x.prop !== null && x.prop.includes(snap.nm[q.proposer]),
        `${where}: «${q.qid}» non mostra chi l'ha aperta (${snap.nm[q.proposer]}): «${x.prop}»`);
    }
  }
  for (const [qid] of bound)
    L.ok('S5B-FIDELITY', snap.open.some(q => q.qid === qid),
      `${where}: legame di «${qid}» nello stato ma la domanda non è aperta`);
}

/* the authorizations shown are the core's, in order, with verdict and target */
/* the balances the page shows are the core's */
function checkConti(L, snap, row, where) {
  for (const k of snap.members) {
    const v = (snap.conti.find(([u]) => u === k) || [null, 0])[1];
    L.ok([row, 'S5B-FIDELITY'], snap.shownConti[k] === v,
      `${where}: conto di ${k} mostrato ${snap.shownConti[k]}, nello stato ${v}`);
  }
}

function checkLive(L, snap, where, extra) {
  const rows = ['S5B-LIVE-SHOWN', 'S5B-FIDELITY', ...(extra || [])];
  L.ok(rows, snap.lives.length === snap.live.length,
    `${where}: autorizzazioni vive nello stato ${snap.live.length}, mostrate ${snap.lives.length}`);
  snap.live.forEach((a, i) => {
    const shown = snap.lives[i] || '';
    const words = [verdictWord(a.verdict), ...targetWords(a.target, snap)];
    L.ok(rows, words.every(w => shown.includes(w)),
      `${where}: autorizzazione ${a.verdict} ${tkey(a.target)} mostrata come «${shown}» (attese: ${words.join(' + ')})`);
  });
}

function checkClosure(L, snap, qid, cause, word, row, where) {
  const r = snap.closed.find(x => x.qid === qid);
  L.ok(row, !!r && r.verdict === 'negative' && r.cause === cause,
    `${where}: lo stato non ha «${qid}» chiusa negativa con causa ${cause}: ${JSON.stringify(r)}`);
  L.ok([row, 'S5B-FIDELITY'], snap.govText.includes(`«${qid}»`) && snap.govText.includes('NEGATIVA') && snap.govText.includes(word),
    `${where}: la chiusura negativa «${qid}» (${cause}) non è mostrata con «${word}»`);
}

/* spend the negative authorization on c through the page, then judge the
   refund against the balances read before the step */
async function denyAndCheck(L, ev, row, where, expectLiveGone) {
  const pre = parse(await ev(OBSERVE));
  const col = pre.collections.find(c => c.id === 1);
  const pledged = new Map();
  if (col) for (const p of [...col.accepted, ...col.pending])
    pledged.set(p.user, (pledged.get(p.user) || 0) + p.amount);
  L.ok(row, pledged.size > 0, `${where}: l'acquisto non tiene impegni da rimborsare`);
  await ev(`(async () => { ${HOME} ${C('[data-key="pile:1"]')} ${C('#view [data-task="denyPermission"]')}
    ${C('#ext-apply')} ${HOME} return 'denied'; })()`);
  const post = parse(await ev(OBSERVE));
  const bal = (s, u) => (s.conti.find(([k]) => k === u) || [null, 0])[1];
  for (const [u, v] of pledged)
    L.ok(row, bal(post, u) === bal(pre, u) + v,
      `${where}: conto di ${u} ${bal(pre, u)} → ${bal(post, u)}, atteso +${v}`);
  L.ok(row, !post.collections.some(c => c.id === 1), `${where}: l'acquisto 1 esiste ancora`);
  L.ok(row, !post.live.some(a => 'permission' in a.target && a.target.permission.c === 1),
    `${where}: un'autorizzazione per l'acquisto 1 sopravvive al diniego`);
  checkLive(L, post, `${where} dopo il diniego`, [row]);
  checkConti(L, post, row, `${where} dopo il diniego`);
  if (expectLiveGone) L.ok(['S5B-LIVE-SHOWN', row], pre.lives.length > post.lives.length,
    `${where}: spendere l'autorizzazione non l'ha tolta dalla pagina (${pre.lives.length} → ${post.lives.length})`);
}

async function freshPage(b, url) {
  const s = await b.page(url, 1280, 900);
  await b.eval(s, `localStorage.clear(); 'cleared'`);
  await b.send('Page.navigate', { url }, s);
  await sleep(900);
  await b.eval(s, HELPERS);
  return s;
}

/* a journey step that cannot reach its control is a RED row, never a throw */
async function drive(L, row, what, ev, body) {
  try { await ev(`(async () => { ${body} return 'ok'; })()`); return true; }
  catch (e) {
    L.ok('S5B-REACH', false, `${what}: ${e.message.split('\n')[0]}`);
    L.ok(row, false, `${what}: storia non raggiungibile — ${e.message.split('\n')[0]}`);
    return false;
  }
}

/* --- journeys -------------------------------------------------------------- */

/* the committed scenario that carries a quota-bound question and an unbound
   one, discovered from the scenario files, loaded through the page */
function discoverScenario() {
  for (const f of readdirSync(SCENARIOS).filter(x => x.endsWith('.json')).sort()) {
    const sc = JSON.parse(readFileSync(join(SCENARIOS, f), 'utf8'));
    const apps = sc.wrap.steps.map(s => s.event.app).filter(Boolean);
    const quota = apps.some(a => a.openBound && 'backdonation' in a.openBound.target);
    const plain = apps.some(a => a.openQuestion);
    if (quota && plain) return { file: f, wrap: sc.wrap };
  }
  return null;
}

async function scenarioJourney(L, b, url) {
  const sc = discoverScenario();
  L.ok('S5B-REACH', !!sc, 'nessuno scenario committato apre una domanda legata a una quota e una libera');
  if (!sc) return;
  const s = await freshPage(b, url);
  const ev = e => b.eval(s, e);
  await ev(`importText(${JSON.stringify(JSON.stringify(sc.wrap))}); presSet(pres.n.env.steps.length); 'loaded'`);
  await sleep(300);
  const snap = parse(await ev(OBSERVE));
  const hasQuota = snap.bindings.some(([, t]) => 'backdonation' in t);
  const hasPlain = snap.open.some(q => !snap.bindings.some(([id]) => id === q.qid));
  L.ok('S5B-REACH', hasQuota && hasPlain,
    `${sc.file}: caricato, lo stato non ha insieme una domanda aperta legata a una quota e una libera`);
  checkQuestions(L, snap, `scenario ${sc.file}`);
  checkLive(L, snap, `scenario ${sc.file}`);
  L.ok('S5B-LIVE-SHOWN', snap.live.length > 0,
    `${sc.file}: nessuna autorizzazione viva da mostrare nello scenario`);
  L.ok('S5B-LIVE-SHOWN', snap.closed.some(r => !snap.live.some(a => a.questionId === r.qid)),
    `${sc.file}: nessuna chiusura senza autorizzazione (domanda libera chiusa)`);
}

async function renounceJourney(L, b, url) {
  const s = await freshPage(b, url);
  const ev = e => b.eval(s, e);
  if (!await drive(L, 'S5B-L7-RENOUNCE', 'preparazione', ev, FIXTURE)) return;
  if (!await drive(L, 'S5B-L7-RENOUNCE', 'apertura della domanda legata', ev, OPEN_BOUND('anna'))) return;
  const opened = parse(await ev(OBSERVE));
  checkQuestions(L, opened, 'domanda sul permesso aperta');
  L.ok('S5B-BOUND-SHOWN', opened.bindings.length === 1 &&
    opened.questions.some(x => x.bind && x.bind.includes('«Olio»')),
    'la domanda sul permesso di «Olio» non mostra «Olio» come bersaglio');
  const mine = opened.questions.flatMap(x => x.renounce);
  L.ok('S5B-L7-RENOUNCE', mine.length > 0 && mine.every(k => k === 'anna'),
    `la rinuncia è offerta a ${JSON.stringify(mine)}, non solo a chi ha proposto (anna)`);
  if (!await drive(L, 'S5B-L7-RENOUNCE', 'rinuncia di chi ha proposto', ev,
    C('[data-kgrenounce="permesso:1"][data-kgsigner="anna"]'))) return;
  const closed = parse(await ev(`(async () => { ${HOME} return ${OBSERVE}; })()`));
  checkClosure(L, closed, 'permesso:1', 'renounced', 'rinuncia', 'S5B-L7-RENOUNCE', 'dopo la rinuncia');
  L.ok('S5B-L7-RENOUNCE', closed.live.length === 1 && closed.live[0].verdict === 'negative' &&
    'permission' in closed.live[0].target && closed.live[0].target.permission.c === 1,
    `dopo la rinuncia manca l'autorizzazione negativa per l'acquisto 1: ${JSON.stringify(closed.live)}`);
  checkLive(L, closed, 'dopo la rinuncia', ['S5B-L7-RENOUNCE']);
  await denyAndCheck(L, ev, 'S5B-L7-RENOUNCE', 'diniego dopo la rinuncia', true);
  const end = parse(await ev(OBSERVE));
  L.ok('S5B-REACH', end.closed.some(r => r.cause === 'renounced') && end.collections.length === 0,
    'la storia della rinuncia non è arrivata fino al rimborso con i controlli della pagina');
}

async function departJourney(L, b, url, proposer, row, label) {
  const s = await freshPage(b, url);
  const ev = e => b.eval(s, e);
  if (!await drive(L, row, `${label}: preparazione`, ev, FIXTURE)) return;
  if (!await drive(L, row, `${label}: apertura della domanda legata`, ev, OPEN_BOUND(proposer))) return;
  const other = proposer === 'anna' ? 'bruno' : 'anna';
  // the other responsabile proposes the proposer's departure; the proposer
  // (not the proposer of the departure) approves it as a separate act
  if (!await drive(L, row, `${label}: proposta di uscita`, ev,
    HOME + C('#gtasks [data-task="departure"]') + CHIP(proposer) + CHIP(other) +
    C(`#govcard [data-bgapprove][data-bgsigner="${proposer}"]`))) return;
  const after = parse(await ev(OBSERVE));
  L.ok(row, !after.members.includes(proposer), `${label}: ${proposer} è ancora nel gruppo dopo la decisione`);
  checkClosure(L, after, 'permesso:1', 'proposerDeparted', 'uscita', row, `${label}: dopo l'uscita`);
  checkLive(L, after, `${label}: dopo l'uscita`, [row]);
  const refIsProposer = after.collections.length === 0;
  if (proposer === 'bruno') {
    // the referente left: refunded inside the departure step; nothing outlives the purchase
    L.ok(row, refIsProposer, `${label}: l'acquisto del referente uscito esiste ancora`);
    L.ok(row, after.live.length === 0, `${label}: un'autorizzazione sopravvive all'acquisto: ${JSON.stringify(after.live)}`);
    const bal = u => (after.conti.find(([k]) => k === u) || [null, 0])[1];
    L.ok(row, bal('carla') === 100 && bal('dora') === 100,
      `${label}: rimborsi nello stesso passo: carla ${bal('carla')}, dora ${bal('dora')}, attesi 100 e 100`);
  } else {
    L.ok(row, !refIsProposer, `${label}: l'acquisto è sparito pur con il referente ancora nel gruppo`);
    L.ok(row, after.live.length === 1 && after.live[0].verdict === 'negative',
      `${label}: manca l'autorizzazione negativa per l'acquisto 1: ${JSON.stringify(after.live)}`);
    await denyAndCheck(L, ev, row, `${label}: diniego dopo l'uscita`, true);
  }
  const end = parse(await ev(OBSERVE));
  L.ok('S5B-REACH', end.closed.some(r => r.cause === 'proposerDeparted') && end.collections.length === 0,
    `${label}: la storia dell'uscita non è arrivata fino al rimborso con i controlli della pagina`);
}

/* a spent positive authorization leaves the display in the same step */
async function grantJourney(L, b, url) {
  const s = await freshPage(b, url);
  const ev = e => b.eval(s, e);
  if (!await drive(L, 'S5B-LIVE-SHOWN', 'preparazione del permesso', ev, FIXTURE + OPEN_BOUND('anna') +
    C('#view [data-kgcast="permesso:1"][data-kgsigner="anna"][data-kgballot="assent"]'))) return;
  const live = parse(await ev(`(async () => { ${HOME} return ${OBSERVE}; })()`));
  L.ok('S5B-LIVE-SHOWN', live.live.length === 1 && live.live[0].verdict === 'positive',
    `dopo il sì manca l'autorizzazione positiva: ${JSON.stringify(live.live)}`);
  checkLive(L, live, 'dopo il sì');
  if (!await drive(L, 'S5B-LIVE-SHOWN', 'spesa del permesso', ev,
    C('[data-key="pile:1"]') + C('#view [data-task="grantPermission"]') + C('#ext-apply') + HOME)) return;
  const spent = parse(await ev(OBSERVE));
  L.ok('S5B-LIVE-SHOWN', spent.live.length === 0 && spent.lives.length === 0,
    `spesa l'autorizzazione resta mostrata: stato ${spent.live.length}, pagina ${spent.lives.length}`);
}

/* #68: a new base proposal opens at zero approvals; the proposer's own
   approval is refused above one admin; a sole admin approves separately */
async function assentJourney(L, b, url) {
  const s = await freshPage(b, url);
  const ev = e => b.eval(s, e);
  // one admin: the proposal waits, then the same admin approves it as a separate act
  if (!await drive(L, 'S5B-68', 'preparazione', ev, `await __rgAddNamed('Bruno');` +
    C('#gtasks [data-task="elect"]') + CHIP('bruno') + CHIP('anna'))) return;
  const solo = parse(await ev(OBSERVE));
  L.ok('S5B-68', solo.approvals.length === 1 && solo.approvals[0].approvals.length === 0,
    `una proposta nuova non parte da zero assensi: ${JSON.stringify(solo.approvals)}`);
  L.ok('S5B-68', solo.proposals.length === 1 && /assensi 0 \/ soglia/.test(solo.proposals[0].text),
    `la pagina non mostra la proposta a zero assensi: ${JSON.stringify(solo.proposals)}`);
  if (!await drive(L, 'S5B-68', 'assenso separato dell\'unico responsabile', ev,
    C('#govcard [data-bgapprove][data-bgsigner="anna"]'))) return;
  const enacted = parse(await ev(OBSERVE));
  L.ok('S5B-68', enacted.approvals.length === 0 && enacted.last && !enacted.last.rej,
    'con un solo responsabile l\'assenso separato non ha deciso la proposta');
  // several admins: a new proposal by anna, whose own approval is refused
  if (!await drive(L, 'S5B-68', 'proposta con più responsabili', ev,
    `await __rgAddNamed('Carla');` + C('#gtasks [data-task="elect"]') + CHIP('carla') + CHIP('anna'))) return;
  const pend = parse(await ev(OBSERVE));
  L.ok('S5B-68', pend.approvals.length === 1 && pend.approvals[0].approvals.length === 0 &&
    pend.approvals[0].proposer === 'anna', `proposta nuova non a zero assensi: ${JSON.stringify(pend.approvals)}`);
  const offered = pend.proposals.flatMap(p => p.approvers);
  L.ok('S5B-68', offered.includes('anna'),
    `l'assenso di chi propone (anna) non è mostrato come rifiutato: controlli offerti ${JSON.stringify(offered)}`);
  if (!await drive(L, 'S5B-68', 'assenso di chi ha proposto', ev,
    C('#govcard [data-bgapprove][data-bgsigner="anna"]'))) return;
  const refused = parse(await ev(OBSERVE));
  L.ok('S5B-68', refused.last && refused.last.rej && refused.last.text.includes('proposerSelfApproval'),
    `il rifiuto non è quello del #68 (proposerSelfApproval): «${refused.last && refused.last.text}»`);
  L.ok('S5B-68', !(refused.last && refused.last.text.includes('alreadyApproved')),
    'il rifiuto mostrato è "già approvato", non quello del #68');
  L.ok('S5B-68', refused.approvals.length === 1 && refused.approvals[0].approvals.length === 0,
    `l'assenso rifiutato ha cambiato lo stato: ${JSON.stringify(refused.approvals)}`);
  L.ok('S5B-REACH', refused.last && refused.last.rej,
    'la storia dell\'assenso non è arrivata fino al rifiuto con i controlli della pagina');
}

/* --- run ------------------------------------------------------------------- */

async function runRows(html, only) {
  const L = newLedger();
  const profile = mkdtempSync(join(tmpdir(), 'rg-bound-profile-'));
  const b = new Browser(findChromium(), profile);
  const url = 'file://' + html;
  const want = k => !only || only.includes(k);
  try {
    await sleep(400);
    const journeys = {
      scenario: () => scenarioJourney(L, b, url),
      renounce: () => renounceJourney(L, b, url),
      'depart-other': () => departJourney(L, b, url, 'anna', 'S5B-L7-DEPART', 'uscita di chi non è referente'),
      'depart-referente': () => departJourney(L, b, url, 'bruno', 'S5B-L7-DEPART', 'uscita del referente'),
      grant: () => grantJourney(L, b, url),
      assent: () => assentJourney(L, b, url),
    };
    for (const [name, run] of Object.entries(journeys)) {
      if (!want(name)) continue;
      try { await run(); }
      catch (e) { L.ok('S5B-REACH', false, `percorso ${name}: ${e.message.split('\n')[0]}`); }
    }
  } finally {
    b.close();
    rmQuiet(profile);
  }
  return L;
}

function report(L, required) {
  let bad = 0;
  for (const row of ROWS) {
    const n = L.checks.get(row);
    const reds = [...new Set(L.reds.get(row))];
    if (required.includes(row) && n === 0) reds.push('riga mai valutata: nessun controllo eseguito');
    if (reds.length) {
      bad += 1;
      console.error(`ROW-RED ${row}`);
      reds.forEach(r => console.error(`  - ${r}`));
    } else if (required.includes(row)) {
      console.log(`ROW-OK ${row} (${n} controlli)`);
    }
  }
  return bad;
}

async function main() {
  const L = await runRows(HTML, null);
  const bad = report(L, ROWS);
  if (bad) { console.error(`RED: ${bad} righe`); process.exit(1); }
  console.log(`GREEN: ${ROWS.length} righe giudicate sulla pagina, mostrato = stato del core`);
}

/* --- selftest: each displayed row can fail --------------------------------
   A mutant is an edit of the built page that removes or falsifies one
   display or effect. It must apply exactly once (an edit that matched
   nothing would report "caught" while testing nothing), and the journeys
   that judge its rows must go red on exactly those rows; the unmutated page
   is the control and must be green on the same journeys. */

const once = (text, from, to) => {
  const n = text.split(from).length - 1;
  if (n !== 1) throw new Error(`mutante non applicato: ${n} occorrenze di «${from.slice(0, 70)}»`);
  return text.replace(from, () => to);
};

const MUTANTS = [
  { id: 'binding-not-shown', journeys: ['renounce', 'scenario'], rows: ['S5B-BOUND-SHOWN'],
    edit: h => once(h, '${bindingHtml(qid)}', '') },
  { id: 'binding-shown-without-target', journeys: ['scenario'], rows: ['S5B-BOUND-SHOWN'],
    edit: h => once(h, "return t === null ? '' : `<div class=\"tiny qbind\">", "return t === null ? '<div class=\"tiny qbind\">decide: nulla</div>' : `<div class=\"tiny qbind\">") },
  { id: 'quota-shown-wrong', journeys: ['scenario'], rows: ['S5B-BOUND-SHOWN', 'S5B-FIDELITY'],
    edit: h => once(h, ': `quota ${t.backdonation.w}`;\n\n/* the question', ': `quota ${t.backdonation.w + 1}`;\n\n/* the question') },
  { id: 'proposer-not-shown', journeys: ['scenario'], rows: ['S5B-BOUND-SHOWN'],
    edit: h => once(h, '<div class="tiny qprop">aperta da ${esc(nm(q.proposer))}</div>', '') },
  { id: 'live-not-shown', journeys: ['scenario', 'grant'], rows: ['S5B-LIVE-SHOWN', 'S5B-FIDELITY'],
    edit: h => once(h, 'function liveAuthHtml(list) {\n  return list.length', 'function liveAuthHtml(list) {\n  return false') },
  { id: 'live-stays-after-spend', journeys: ['grant'], rows: ['S5B-LIVE-SHOWN'],
    edit: h => once(h, '${liveAuthHtml(withAuth(state).live)}', '${liveAuthHtml(window.__seen = (withAuth(state).live.length ? withAuth(state).live : (window.__seen || [])))}') },
  { id: 'live-verdict-swapped', journeys: ['scenario'], rows: ['S5B-LIVE-SHOWN', 'S5B-FIDELITY'],
    edit: h => once(h, "`autorizzazione viva ${a.verdict === 'positive' ? 'positiva' : 'negativa'}:", "`autorizzazione viva ${a.verdict === 'positive' ? 'negativa' : 'positiva'}:") },
  { id: 'proposer-approval-not-offered', journeys: ['assent'], rows: ['S5B-68'],
    edit: h => once(h, 'const open = bgAdmins.filter(k => !pp.approvals.includes(k));',
      'const open = bgAdmins.filter(k => !pp.approvals.includes(k) && !(k === pp.proposer && bgAdmins.length > 1));') },
  { id: 'zero-approvals-not-shown', journeys: ['assent'], rows: ['S5B-68'],
    edit: h => once(h, 'assensi ${pp.approvals.length} / soglia', 'assensi / soglia') },
  { id: 'proposer-approval-accepted', journeys: ['assent'], rows: ['S5B-68'],
    edit: h => once(h, "  if (signer === pend.proposer && bgAdminCount(gs) > 1) return 'proposerSelfApproval';\n", '') },
  { id: 'refusal-reads-already-approved', journeys: ['assent'], rows: ['S5B-68'],
    edit: h => once(h, "why: 'la proposta non è andata a buon fine: ' + verr,", "why: 'la proposta non è andata a buon fine: alreadyApproved',") },
  { id: 'sole-admin-cannot-approve', journeys: ['assent'], rows: ['S5B-68'],
    edit: h => once(h, "signer === pend.proposer && bgAdminCount(gs) > 1", "signer === pend.proposer && bgAdminCount(gs) > 0") },
  { id: 'renounce-control-removed', journeys: ['renounce'], rows: ['S5B-L7-RENOUNCE', 'S5B-REACH'],
    edit: h => once(h, '${castButtonsHtml(qid, q)}${renounceHtml(qid, q)}', '${castButtonsHtml(qid, q)}') },
  { id: 'renounce-offered-to-another', journeys: ['renounce'], rows: ['S5B-L7-RENOUNCE'],
    edit: h => once(h, 'data-kgsigner="${esc(q.proposer)}" title=".renounce"', 'data-kgsigner="${esc(vtFranchise(curView()).find(k => k !== q.proposer) || q.proposer)}" title=".renounce"') },
  { id: 'renounce-cause-not-named', journeys: ['renounce'], rows: ['S5B-L7-RENOUNCE', 'S5B-FIDELITY'],
    edit: h => once(h, "renounced: 'rinuncia di chi l’ha proposta'", "renounced: 'soglia'") },
  { id: 'departure-cause-not-named', journeys: ['depart-other'], rows: ['S5B-L7-DEPART', 'S5B-FIDELITY'],
    edit: h => once(h, "proposerDeparted: 'uscita di chi l’ha proposta'", "proposerDeparted: 'soglia'") },
  { id: 'deny-refunds-nobody', journeys: ['renounce', 'depart-other'], rows: ['S5B-L7-RENOUNCE', 'S5B-L7-DEPART'],
    edit: h => once(h, 'live: pulled[1], conti: refundAll(s.conti, ps), collections: rest', 'live: pulled[1], conti: s.conti, collections: rest') },
  { id: 'authorization-outlives-purchase', journeys: ['depart-referente'], rows: ['S5B-L7-DEPART'],
    edit: h => once(h, 'live: s.live.filter(a => targetPresent(s, a.target)) });', 'live: s.live });') },
];

async function selftest() {
  const doc = readFileSync(HTML, 'utf8');
  const work = mkdtempSync(join(tmpdir(), 'rg-bound-selftest-'));
  const bad = [];
  try {
    const journeys = [...new Set(MUTANTS.flatMap(m => m.journeys))];
    const base = await runRows(HTML, journeys);
    const judged = [...new Set(MUTANTS.flatMap(m => m.rows))];
    const baseReds = judged.filter(r => base.reds.get(r).length);
    if (baseReds.length) bad.push(`controllo: la pagina non mutata è rossa su ${baseReds.join(', ')}`);
    else console.log(`controllo: pagina non mutata verde su ${journeys.join(', ')}`);
    for (const m of MUTANTS) {
      let html;
      try { html = m.edit(doc); } catch (e) { bad.push(`${m.id}: ${e.message}`); continue; }
      if (html === doc) { bad.push(`${m.id}: la modifica non ha cambiato la pagina`); continue; }
      const path = join(work, m.id + '.html');
      writeFileSync(path, html);
      const L = await runRows(path, m.journeys);
      for (const row of m.rows) {
        if (L.reds.get(row).length) console.log(`mutante ${m.id}: ${row} rossa come atteso — ${L.reds.get(row)[0].slice(0, 110)}`);
        else bad.push(`${m.id}: ${row} è rimasta verde sulla pagina mutata`);
      }
    }
  } finally { rmQuiet(work); }
  if (bad.length) { console.error(`RED: selftest, ${bad.length} problemi`); bad.forEach(x => console.error(' - ' + x)); return 1; }
  console.log(`GREEN: selftest, ${MUTANTS.length} mutanti, ogni riga giudicata è diventata rossa`);
  return 0;
}

if (process.argv.includes('--selftest')) {
  process.exit(await selftest());
} else {
  await main();
}
