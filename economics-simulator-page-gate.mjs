#!/usr/bin/env node
/*
 * economics-simulator-page-gate.mjs — what the built page shows of the
 * histories the generator plays.
 *
 * A fixed, stated set of generated histories (the generator gate's own traces
 * for seeds 1..8, each cut where the page's own governance check stops
 * accepting it) is loaded into the built page through its loading path
 * (importText) and stepped one applied event at a time. After every step the
 * page's display of open questions (target and proposer), live authorizations
 * (verdict and target) and closed questions (verdict and cause) is compared
 * with the core state that step produced: the aggregates are the production
 * core's own, handed back by the generator's walk, and the check computes no
 * target, authorization or closure of its own.
 *
 *   G3-PAGE-STATES  after every loaded step the display equals the core state;
 *                   the stated set must reach each shape it claims to judge
 *   G3-READ-ONLY    the check reads the page and the core state and edits
 *                   nothing: the tracked page is byte-identical after the run
 *   G3-BUDGET       the run, and the selftest, fit the stated wall budget
 *
 * Usage:
 *   node economics-simulator-page-gate.mjs [--html PATH]   production run
 *   node economics-simulator-page-gate.mjs --selftest      controls that the
 *                                                          rows can fail
 *
 * --selftest edits the built page in a scratch copy — a purchase target shown
 * as a quota and the reverse, in a question and in an authorization; an
 * authorization still shown after it is spent; a closure with the other
 * verdict or cause; a proposer shown as someone else — and requires the row to
 * go RED for the stated reason. An edit that does not apply exactly once is a
 * broken control, not a caught mutant. It also hands the check a core state
 * with one authorization removed or one binding retargeted and requires RED,
 * which shows the expected values are read from the state, and a set of
 * histories that reaches too little and requires RED naming the missing shape.
 */

import { mkdtempSync, readFileSync, writeFileSync } from 'node:fs';
import { createHash } from 'node:crypto';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { tmpdir } from 'node:os';
import { CONFIG as GENERATOR, buildSurface, generateTrace, envelopeOf, loadCore, clone }
  from './economics-simulator-generator-gate.mjs';
import { Browser, findChromium, sleep, rmQuiet } from './economics-simulator-teaching-gate.mjs';

const REPO = dirname(fileURLToPath(import.meta.url));
const argVal = flag => {
  const i = process.argv.indexOf(flag);
  return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : null;
};
const HTML = argVal('--html') || join(REPO, 'economics-simulator.html');
const CORE = join(REPO, 'economics-simulator-core.mjs');
const sha256 = s => createHash('sha256').update(s).digest('hex');

/* --- the stated configuration (G3-PAGE-STATES, G3-BUDGET) ------------------- */
const CONFIG = Object.freeze({
  seeds: [1, 2, 3, 4, 5, 6, 7, 8],   // the generator gate's seeds; trace 0 of each
  budgetMs: 60000,                   // wall budget of this gate, and of its selftest
});
const ROWS = ['G3-PAGE-STATES', 'G3-READ-ONLY', 'G3-BUDGET'];

/* --- the generated histories ------------------------------------------------- */

/* the generator's trace for a seed, replayed through the production core. The
   core is wrapped only to record the steps it applied; every aggregate the
   check expects is the one the core returned for the step. The history ends
   where the core's own governance check stops accepting it: the page loads
   exactly what that check accepts. */
function generatedHistory(core, surface, seed) {
  const applied = [];
  const recording = { ...core, applyIntegrated(gs, signer, ev) {
    const det = core.applyIntegrated(gs, signer, ev);
    if (!det.refused) applied.push({ signer, ev: clone(ev) });
    return det;
  } };
  generateTrace(seed * 1000003, { ...GENERATOR, core: recording }, surface);
  const accepted = k => {
    try { core.verifyGovernedIntegrated(envelopeOf(core, applied.slice(0, k))); return true; }
    catch { return false; }
  };
  let lo = 0, hi = applied.length;
  while (lo < hi) { const mid = (lo + hi + 1) >> 1; if (accepted(mid)) lo = mid; else hi = mid - 1; }
  const env = envelopeOf(core, applied.slice(0, lo));
  return { seed, generated: applied.length, env,
    states: [env.initial, ...env.steps.map(s => s.result.aggregate)] };
}

async function statedHistories() {
  const core = await loadCore(CORE);
  const surface = buildSurface();
  return CONFIG.seeds.map(seed => generatedHistory(core, surface, seed));
}

/* --- the core's view of a state (F-CORE-VIEW) --------------------------------- */

/* what the core state holds of open questions, live authorizations and
   closures; a read of its fields and nothing more */
function coreView(agg) {
  const a = agg.appFold;
  const bound = new Map(a.bindings);
  return {
    bound: a.votes.openQuestions.map(([qid, q]) =>
      ({ qid, target: bound.get(qid) || null, proposer: q.proposer })),
    live: (a.live || []).map(l => ({ verdict: l.verdict, target: l.target })),
    closed: a.votes.closed.map(r => ({ qid: r.questionId, verdict: r.verdict, cause: r.cause })),
  };
}

/* --- the page's view of its display (F-PAGE-VIEW) ------------------------------ */

/* the group card as text: open questions with their target and proposer lines,
   live authorization lines, closed-question lines; null without the card */
const PAGE_VIEW = `(() => {
  const txt = el => el.textContent.replace(/\\s+/g, ' ').trim();
  const gov = document.getElementById('govcard');
  if (!gov) return null;
  return {
    questions: [...gov.querySelectorAll('[data-qid]')].map(el => ({ qid: el.dataset.qid,
      bind: el.querySelector('.qbind') ? txt(el.querySelector('.qbind')) : null,
      prop: el.querySelector('.qprop') ? txt(el.querySelector('.qprop')) : null })),
    live: [...gov.querySelectorAll('.liveauth')].map(txt),
    closed: [...gov.querySelectorAll('.tiny')].map(txt).filter(t => t.startsWith('🗳')),
  };
})()`;

/* the page's words, read back into the shape the core state has */
function shownTarget(text) {
  let m = /^permesso per l’acquisto «(.*)»$/.exec(text);
  if (m) return { kind: 'permission', label: m[1] };
  m = /^quota (\d+)$/.exec(text);
  return m ? { kind: 'backdonation', w: Number(m[1]) } : { unread: text };
}
function pageView(raw) {
  if (!raw) return null;
  return {
    bound: raw.questions.map(q => ({ qid: q.qid,
      target: q.bind === null ? null : shownTarget(q.bind.replace(/^decide: /, '')),
      proposer: q.prop === null ? null : q.prop.replace(/^aperta da /, '') })),
    live: raw.live.map(t => {
      const m = /^autorizzazione viva (positiva|negativa): (.*)$/.exec(t);
      return m ? { verdict: m[1], target: shownTarget(m[2]) } : { unread: t };
    }),
    closed: raw.closed.map(t => {
      const m = /^🗳 «(.*)» — (POSITIVA|NEGATIVA) \((.*)\) · sì \d+ \/ no \d+$/.exec(t);
      return m ? { qid: m[1], verdict: m[2], cause: m[3] } : { unread: t };
    }),
  };
}

/* the display's words for what the core holds: the vocabulary the page
   promises, and the page's own names for members and purchases */
const VERDICT_WORD = { positive: 'positiva', negative: 'negativa' };
const CLOSURE_WORD = { positive: 'POSITIVA', negative: 'NEGATIVA' };
const CAUSE_WORD = { tally: 'soglia', franchiseChange: 'elettorato', renounced: 'rinuncia', proposerDeparted: 'uscita' };
const targetKind = t => 'permission' in t ? 'permission' : 'backdonation';
const targetShown = (t, names) => 'permission' in t
  ? { kind: 'permission', label: names.lbl[t.permission.c] }
  : { kind: 'backdonation', w: t.backdonation.w };
const showTarget = t => t === null ? 'no target' : JSON.stringify(t);

/* --- ledger ------------------------------------------------------------------ */

function newLedger() {
  const reds = new Map(ROWS.map(r => [r, []]));
  const checks = new Map(ROWS.map(r => [r, 0]));
  return { reds, checks, compared: 0, shapes: new Set(), ok(row, cond, why) {
    checks.set(row, checks.get(row) + 1);
    if (!cond) reds.get(row).push(why);
  } };
}

/* --- the judgement of one loaded step ---------------------------------------- */

/* the shapes the stated histories claim to judge; each must be reached */
const SHAPES = ['question bound to a purchase', 'question bound to a quota', 'question bound to nothing',
  'positive authorization', 'negative authorization', 'authorization spent',
  'closed by tally', 'closed by renunciation', 'closed by the proposer leaving',
  'closed list longer than the page tail'];

function reached(core, prev, shapes) {
  for (const q of core.bound)
    shapes.add(q.target === null ? 'question bound to nothing'
      : 'permission' in q.target ? 'question bound to a purchase' : 'question bound to a quota');
  for (const l of core.live) shapes.add(`${l.verdict} authorization`);
  if (prev && core.live.length < prev.live.length) shapes.add('authorization spent');
  for (const r of core.closed)
    shapes.add(r.cause === 'tally' ? 'closed by tally' : r.cause === 'renounced' ? 'closed by renunciation'
      : r.cause === 'proposerDeparted' ? 'closed by the proposer leaving' : `closed by ${r.cause}`);
  if (core.closed.length > 3) shapes.add('closed list longer than the page tail');
}

function judgeStep(L, where, core, shown, names) {
  const row = 'G3-PAGE-STATES';
  L.compared++;
  if (!shown) return L.ok(row, false, `${where}: the page shows no group card`);
  const bound = (c, s) => {
    L.ok(row, s.qid === c.qid, `${where}: open question «${c.qid}» shown as «${s.qid}»`);
    const want = c.target === null ? null : targetShown(c.target, names);
    L.ok(row, JSON.stringify(s.target) === JSON.stringify(want),
      `${where}: bound target of «${c.qid}»: core ${showTarget(c.target && targetShown(c.target, names))}, page ${showTarget(s.target)}`);
    L.ok(row, s.proposer === names.nm[c.proposer],
      `${where}: proposer of «${c.qid}»: core ${names.nm[c.proposer]}, page ${s.proposer}`);
  };
  L.ok(row, shown.bound.length === core.bound.length,
    `${where}: open questions: core ${core.bound.length}, page ${shown.bound.length}`);
  core.bound.forEach((c, i) => shown.bound[i] && bound(c, shown.bound[i]));
  L.ok(row, shown.live.length === core.live.length,
    `${where}: live authorizations: core ${core.live.length}, page ${shown.live.length}`);
  core.live.forEach((c, i) => {
    const s = shown.live[i] || {};
    L.ok(row, s.verdict === VERDICT_WORD[c.verdict] && JSON.stringify(s.target) === JSON.stringify(targetShown(c.target, names)),
      `${where}: live authorization ${i + 1}: core ${c.verdict} ${targetKind(c.target)}, page ${JSON.stringify(s)}`);
  });
  const tail = shown.closed.length;
  L.ok(row, (tail > 0) === (core.closed.length > 0) && tail <= core.closed.length,
    `${where}: closed questions: core ${core.closed.length}, page shows ${tail}`);
  const suffix = core.closed.slice(core.closed.length - tail);
  suffix.forEach((c, i) => {
    const s = shown.closed[i] || {};
    L.ok(row, s.qid === c.qid && s.verdict === CLOSURE_WORD[c.verdict] && typeof s.cause === 'string' &&
      s.cause.includes(CAUSE_WORD[c.cause] || `?${c.cause}`),
      `${where}: closed «${c.qid}»: core ${c.verdict} by ${c.cause}, page ${JSON.stringify(s)}`);
  });
}

/* --- the page ---------------------------------------------------------------- */

/* the page loads the history through its own path, then is stepped to each
   state; a history the page does not take is RED, never skipped */
const LOAD = text => `(() => {
  if (pres) exitPresentation();
  window.__rgSeen = undefined;
  importText(${JSON.stringify(text)});
  return pres ? pres.n.env.steps.length : -1;
})()`;
const STEP_ALL = `JSON.stringify(Array.from({ length: pres.n.env.steps.length + 1 },
  (_, i) => { presSet(i); return ${PAGE_VIEW}; }))`;
const NAMES = (keys, ids) => `JSON.stringify({
  nm: Object.fromEntries(${JSON.stringify(keys)}.map(k => [k, nm(k)])),
  lbl: Object.fromEntries(${JSON.stringify(ids)}.map(c => [c, lbl(c)])) })`;

async function playHistory(L, b, s, hist) {
  const n = hist.env.steps.length;
  const loaded = await b.eval(s, LOAD(JSON.stringify(hist.env)));
  L.ok('G3-PAGE-STATES', loaded === n,
    `seed ${hist.seed}: the page loaded ${loaded} steps of ${n}`);
  if (loaded !== n) return;
  const cores = hist.states.map(coreView);
  const keys = [...new Set(cores.flatMap(c => c.bound.map(q => q.proposer)))];
  const ids = [...new Set(cores.flatMap(c => [...c.bound.map(q => q.target), ...c.live.map(l => l.target)])
    .filter(t => t && 'permission' in t).map(t => t.permission.c))];
  const names = JSON.parse(await b.eval(s, NAMES(keys, ids)));
  L.ok('G3-PAGE-STATES', new Set(Object.values(names.nm)).size === keys.length &&
    new Set(Object.values(names.lbl)).size === ids.length,
    `seed ${hist.seed}: the page gives two members or two purchases the same name, so a swap would pass`);
  const views = JSON.parse(await b.eval(s, STEP_ALL));
  cores.forEach((core, i) => {
    judgeStep(L, `seed ${hist.seed} step ${i}`, core, pageView(views[i]), names);
    reached(core, cores[i - 1], L.shapes);
  });
}

/* `firstRed` stops at the first history that goes red: a mutant is caught once */
async function runPage(html, histories, browser, { firstRed = false } = {}) {
  const L = newLedger();
  const own = !browser;
  const profile = own ? mkdtempSync(join(tmpdir(), 'rg-page-profile-')) : null;
  const b = browser || new Browser(findChromium(), profile);
  try {
    if (own) await sleep(400);
    const s = await b.page('file://' + html, 1280, 900);
    for (const hist of histories) {
      await playHistory(L, b, s, hist);
      if (firstRed && L.reds.get('G3-PAGE-STATES').length) break;
    }
  } finally {
    if (own) { b.close(); rmQuiet(profile); }
  }
  return L;
}

/* the run and its wall time; the budget is judged by the caller's row */
async function timedRun(html, histories, browser, budgetMs = CONFIG.budgetMs) {
  const t0 = performance.now();
  const L = await runPage(html, histories, browser);
  return { L, elapsedMs: performance.now() - t0, budgetMs };
}

/* the stated set must reach every shape it claims to judge */
function judgeReach(L) {
  for (const shape of SHAPES)
    L.ok('G3-PAGE-STATES', L.shapes.has(shape), `the stated histories never reach: ${shape}`);
}

/* --- report ------------------------------------------------------------------ */

function report(L, required) {
  let bad = 0;
  for (const row of ROWS) {
    const n = L.checks.get(row);
    const reds = [...new Set(L.reds.get(row))];
    if (required.includes(row) && n === 0) reds.unshift('row never evaluated: no check executed');
    if (row === 'G3-PAGE-STATES' && required.includes(row) && L.compared === 0)
      reds.unshift('no page check on generated states: 0 loaded steps had their display compared');
    if (reds.length) {
      bad += 1;
      console.error(`ROW-RED ${row}`);
      reds.slice(0, 8).forEach(r => console.error(`  - ${r}`));
      if (reds.length > 8) console.error(`  - … and ${reds.length - 8} more`);
    } else if (required.includes(row)) {
      console.log(`ROW-OK ${row} (${n} checks${row === 'G3-PAGE-STATES' ? `, ${L.compared} steps compared` : ''})`);
    }
  }
  return bad;
}

async function main() {
  const before = sha256(readFileSync(HTML));
  const histories = await statedHistories();
  const { L } = await timedRun(HTML, histories);
  judgeReach(L);
  L.ok('G3-READ-ONLY', sha256(readFileSync(HTML)) === before, 'the tracked page changed during the run');
  const bad = report(L, ['G3-PAGE-STATES', 'G3-READ-ONLY']);
  if (bad) { console.error(`RED: ${bad} rows`); process.exit(1); }
  console.log(`GREEN: ${histories.length} generated histories, display = core state`);
}

/* --- selftest: each row can fail ---------------------------------------------- */

const once = (text, from, to) => {
  const n = text.split(from).length - 1;
  if (n !== 1) throw new Error(`mutant not applied: ${n} occurrences of «${from.slice(0, 70)}»`);
  return text.replace(from, () => to);
};

/* the same histories with the core state the check expects altered in a copy;
   the page still loads the true envelope */
const withStates = (histories, alter) => histories.map(h => ({ ...h, states: h.states.map(st => alter(clone(st))) }));
const swapTarget = t => 'permission' in t ? { backdonation: { w: t.permission.c } } : { permission: { c: t.backdonation.w } };
const ALTERED = [
  { id: 'authorization-removed-from-state', alter: st => { (st.appFold.live || []).shift(); return st; } },
  { id: 'binding-retargeted-in-state', alter: st => {
    if (st.appFold.bindings.length) st.appFold.bindings[0][1] = swapTarget(st.appFold.bindings[0][1]);
    return st; } },
];

/* an edit of the built page that falsifies one display; `why` is what the
   RED row must say */
const MUTANTS = [
  { id: 'question-purchase-shown-as-quota', why: /bound target/,
    edit: h => once(h, 'decide: ${targetTx(t)}',
      "decide: ${targetTx('permission' in t ? { backdonation: { w: t.permission.c } } : t)}") },
  { id: 'question-quota-shown-as-purchase', why: /bound target/,
    edit: h => once(h, 'decide: ${targetTx(t)}',
      "decide: ${targetTx('backdonation' in t ? { permission: { c: t.backdonation.w } } : t)}") },
  { id: 'authorization-purchase-shown-as-quota', why: /live/,
    edit: h => once(h, ': ${targetTx(a.target)}`',
      ": ${targetTx('permission' in a.target ? { backdonation: { w: a.target.permission.c } } : a.target)}`") },
  { id: 'authorization-quota-shown-as-purchase', why: /live/,
    edit: h => once(h, ': ${targetTx(a.target)}`',
      ": ${targetTx('backdonation' in a.target ? { permission: { c: a.target.backdonation.w } } : a.target)}`") },
  { id: 'authorization-stays-after-spend', why: /live/,
    edit: h => once(h, '${liveAuthHtml(withAuth(state).live)}',
      '${liveAuthHtml(window.__rgSeen = withAuth(state).live.length >= (window.__rgSeen || []).length ? withAuth(state).live : window.__rgSeen)}') },
  { id: 'closure-verdict-swapped', why: /closed/,
    edit: h => once(h, "🗳 «${esc(r.questionId)}» — ${r.verdict === 'positive' ? 'POSITIVA' : 'NEGATIVA'}",
      "🗳 «${esc(r.questionId)}» — ${r.verdict === 'positive' ? 'NEGATIVA' : 'POSITIVA'}") },
  { id: 'closure-cause-swapped', why: /closed/,
    edit: h => once(h, "  tally: 'soglia',", "  tally: 'cambio di elettorato',") },
  { id: 'proposer-shown-as-another', why: /proposer/,
    edit: h => once(h, 'aperta da ${esc(nm(q.proposer))}',
      'aperta da ${esc(nm(vtFranchise(curView()).find(k => k !== q.proposer) || q.proposer))}') },
];

async function selftest() {
  const t0 = performance.now();
  const doc = readFileSync(HTML, 'utf8');
  const work = mkdtempSync(join(tmpdir(), 'rg-page-selftest-'));
  const profile = mkdtempSync(join(tmpdir(), 'rg-page-profile-'));
  const b = new Browser(findChromium(), profile);
  const bad = [];
  try {
    await sleep(400);
    const histories = await statedHistories();
    const base = await runPage(HTML, histories, b);
    judgeReach(base);
    const baseReds = ROWS.filter(r => base.reds.get(r).length);
    if (baseReds.length || base.compared === 0)
      bad.push(`control: the unmutated page is not green with steps compared: ${baseReds.join(', ') || 'no step was compared'}`);
    else console.log(`control: the unmutated page is green (${base.compared} steps compared)`);
    for (const m of MUTANTS) {
      let html;
      try { html = m.edit(doc); } catch (e) { bad.push(`${m.id}: ${e.message}`); continue; }
      if (html === doc) { bad.push(`${m.id}: the edit did not change the page`); continue; }
      const path = join(work, m.id + '.html');
      writeFileSync(path, html);
      const L = await runPage(path, histories, b, { firstRed: true });
      const reds = L.reds.get('G3-PAGE-STATES');
      const hit = reds.find(r => m.why.test(r));
      if (hit) console.log(`mutant ${m.id}: G3-PAGE-STATES red as expected — ${hit.slice(0, 110)}`);
      else bad.push(`${m.id}: G3-PAGE-STATES stayed green on the mutated page (${L.checks.get('G3-PAGE-STATES')} checks, ${reds.length} reds)`);
    }
    for (const a of ALTERED) {
      const L = await runPage(HTML, withStates(histories, a.alter), b, { firstRed: true });
      if (L.reds.get('G3-PAGE-STATES').length) console.log(`control ${a.id}: G3-PAGE-STATES red as expected`);
      else bad.push(`${a.id}: G3-PAGE-STATES stayed green against an altered core state: the expected values are not read from it`);
    }
    const few = await runPage(HTML, histories.slice(0, 1), b);
    judgeReach(few);
    const missing = few.reds.get('G3-PAGE-STATES').filter(r => /never reach/.test(r));
    if (missing.length) console.log(`control reach: a one-history set is red — ${missing[0]}`);
    else bad.push('reach: a one-history set reaches every claimed shape, so the reach check cannot fail');
    const { L: tight } = await timedRun(HTML, histories, b, 0);
    if (tight.reds.get('G3-BUDGET').length) console.log('control budget: G3-BUDGET red with a budget of 0 ms');
    else bad.push('budget: G3-BUDGET stayed green with a budget of 0 ms');
  } finally { b.close(); rmQuiet(profile); rmQuiet(work); }
  if (bad.length) { console.error(`RED: selftest, ${bad.length} problems`); bad.forEach(x => console.error(' - ' + x)); return 1; }
  console.log(`GREEN: selftest, ${MUTANTS.length} mutants, each went red; ${Math.round(performance.now() - t0)} ms`);
  return 0;
}

if (process.argv.includes('--selftest')) process.exit(await selftest());
else await main();
