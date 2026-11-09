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

/* --- ledger ------------------------------------------------------------------ */

function newLedger() {
  const reds = new Map(ROWS.map(r => [r, []]));
  const checks = new Map(ROWS.map(r => [r, 0]));
  return { reds, checks, compared: 0, shapes: new Set(), ok(row, cond, why) {
    checks.set(row, checks.get(row) + 1);
    if (!cond) reds.get(row).push(why);
  } };
}

/* --- the page ---------------------------------------------------------------- */

/* the page loads the history through its own path, then is stepped to each
   state; a history the page does not take is RED, never skipped */
const LOAD = (hist, text) => `(() => {
  if (pres) exitPresentation();
  window.__rgSeen = undefined;
  importText(${JSON.stringify(text)});
  return pres ? pres.n.env.steps.length : -1;
})()`;
const STEP_ALL = 'JSON.stringify(Array.from({ length: pres.n.env.steps.length + 1 }, (_, i) => { presSet(i); return i; }))';

async function playHistory(L, b, s, hist) {
  const n = hist.env.steps.length;
  const loaded = await b.eval(s, LOAD(hist, JSON.stringify(hist.env)));
  L.ok('G3-PAGE-STATES', loaded === n,
    `seed ${hist.seed}: the page loaded ${loaded} steps of ${n}`);
  if (loaded !== n) return;
  await b.eval(s, STEP_ALL);
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

/* the shapes the stated histories claim to judge; each must be reached */
const SHAPES = ['question bound to a purchase', 'question bound to a quota', 'question bound to nothing',
  'positive authorization', 'negative authorization', 'authorization spent',
  'closed by tally', 'closed by renunciation', 'closed by the proposer leaving',
  'closed list longer than the page tail'];

function judgeReach(L) {
  for (const shape of SHAPES)
    L.ok('G3-PAGE-STATES', L.shapes.has(shape), `the stated histories never reach: ${shape}`);
}

/* the run and its wall time; the budget is judged by the caller's row */
async function timedRun(html, histories, browser, budgetMs = CONFIG.budgetMs) {
  const t0 = performance.now();
  const L = await runPage(html, histories, browser);
  return { L, elapsedMs: performance.now() - t0, budgetMs };
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
      console.log(`ROW-OK ${row} (${n} checks)`);
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
