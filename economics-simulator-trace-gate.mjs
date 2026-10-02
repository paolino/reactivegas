#!/usr/bin/env node
/*
 * economics-simulator-trace-gate.mjs — reproducibility and conformance
 * verifier for the Lean trace corpus embedded in economics-simulator.html.
 *
 * Fresh on every run it:
 *   1. runs the committed lean/TraceDriverV1.lean in this repository's actual
 *      lake environment (the durable producer; its JSON is disposable);
 *   2. requires valid JSON with a nonempty corpus and nonempty steps;
 *   3. extracts the embedded LEAN_TRACES_V1 fixture and its stated sha256
 *      from the committed HTML;
 *   4. compares fresh Lean output against the embedded fixture by hash and by
 *      structure, reporting the first structural difference;
 *   5. executes the HTML's ACTUAL production JavaScript — the whole embedded
 *      script evaluated in a vm with inert browser shims — and invokes its
 *      own `traceConformance` (and `verifyTraceV1` over the fresh corpus);
 *      never a copied transition implementation;
 *   6. fails on any discontinuity, outcome mismatch, post-state difference,
 *      law violation, or missing/empty envelope;
 *   7. recognises every #81 row (L-1..L-6b, R-1, R-2 of
 *      specs/81-v5-lifecycle/spec.md) in the FRESH output of the production
 *      root by what the integrated step did, and judges each by replaying the
 *      fresh envelope (refused steps admitted) through the production verifier
 *      up to its first witness step; a row without a witness, or one the page
 *      does not reach, is RED;
 *   8. recognises, the same way, every #76 row of closure-derived economic
 *      authorization (ROWS76: a bound opening, a minted authorization, a
 *      grant, a deny and a voted backdonation each spending one, and the
 *      refused unbound grant, spent closure, bind by a non-proposer, bind
 *      after a ballot, bind of a closed question, backdonations with no
 *      closure, a negative closure, a closure bound to another share and a
 *      spent closure, and a grant and a deny each refused while only a
 *      closure of the opposite verdict for their collection and one of
 *      their verdict for another target are live) and judges each by the
 *      same replay;
 *   9. prints counts, the fresh sha, the row witnesses, and GREEN only after
 *      Lean regeneration equivalence, production-JS replay and every row
 *      succeed.
 *
 * Usage from any working directory:
 *   node /path/to/repo/economics-simulator-trace-gate.mjs
 *   node /path/to/repo/economics-simulator-trace-gate.mjs --selftest
 *
 * --selftest proves the gate can fail: a mutated post-state in a scratch
 * copy of the embedded envelope, an emptied embedded corpus, and a mutated
 * stated sha — each RED for its intended reason — plus one production mutant
 * per #81 row (the page's integrated transcription edited in a scratch copy:
 * renounce left open, departure closure dropped, closed positive, recorded
 * .tally, records discarded, every question closed, post-departure sweep
 * dropped, unrelated questions touched, non-proposer renounce and
 * non-designee ballot accepted), each RED on its row, and one per #76 row
 * (binding dropped, nothing minted, closure not spent, deny reading the
 * positive verdict, unbound grant applied, bind of an open or a closed
 * question accepted), each RED on its row — every one but the spent-closure
 * rows at its own witness step, judged alone on Lean's recorded input — then
 * production GREEN.
 * Temporary artifacts live in a fresh mkdtemp directory; the repo stays clean.
 */

import { readFileSync, writeFileSync, mkdtempSync, rmSync } from 'node:fs';
import { createHash } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { tmpdir } from 'node:os';
import vm from 'node:vm';

const REPO = dirname(fileURLToPath(import.meta.url));

/* Teardown of a disposable resource must never determine the gate's verdict:
   scratch-dir cleanup is housekeeping and cannot veto the exit code. */
function rmQuiet(p) {
  try { rmSync(p, { recursive: true, force: true }); } catch { /* housekeeping */ }
}
const HTML = join(REPO, 'economics-simulator.html');
const sha256 = b => createHash('sha256').update(b).digest('hex');

/* --- embedded fixture + stated sha extraction ------------------------------ */

function extractEmbedded(doc) {
  const fm = doc.match(/const LEAN_TRACES_V1 = (\{.*?\});\n/s);
  if (!fm) throw new Error('LEAN_TRACES_V1 non trovato nel documento');
  const sm = doc.match(/Raw output sha256:\n\s*([0-9a-f]{64})/);
  if (!sm) throw new Error('sha dichiarato del corpus non trovato nel documento');
  return { fixtureText: fm[1], statedSha: sm[1] };
}

/* --- execute the page's ACTUAL production script in an inert vm ------------ */

function loadProduction(doc) {
  const sm = doc.match(/<script>\n([\s\S]*?)\n<\/script>/);
  if (!sm) throw new Error('script di produzione non trovato nel documento');
  const src = sm[1];
  // universal inert stub: any property access yields another callable stub,
  // so define-time and render-time DOM traffic is absorbed without a browser
  const stubHandler = {
    get(t, p) {
      if (p === Symbol.toPrimitive) return () => '';
      if (p === Symbol.iterator) return function* () {};
      if (p === 'hidden' || p === 'disabled') return false;
      return STUB;
    },
    set() { return true; },
    apply() { return STUB; },
    construct() { return STUB; },
  };
  const STUB = new Proxy(function () {}, stubHandler);
  const ctx = {
    location: { search: '' },
    document: STUB,
    navigator: STUB,
    innerWidth: 1280, innerHeight: 800,
    performance: { now: () => 0 },
    requestAnimationFrame: () => 0,
    setTimeout: () => 0, clearTimeout: () => 0,
    console,
  };
  ctx.window = ctx;
  ctx.globalThis = ctx;
  vm.createContext(ctx);
  vm.runInContext(src, ctx, { filename: 'economics-simulator.html#script' });
  if (!ctx.window.RG || typeof ctx.window.RG.traceConformance !== 'function')
    throw new Error('il codice di produzione non espone traceConformance: esecuzione non provata');
  return { RG: ctx.window.RG, scriptSha: sha256(src) };
}

/* --- #81 rows (V-5 lifecycle, S-12 refusals) in the fresh Lean corpus -------
   Every row L-1..L-6b, R-1, R-2 of specs/81-v5-lifecycle/spec.md is
   recognised in the FRESH output of the production root
   (Reactivegas.apply) by what the integrated step did — never by a seed name
   or a step index. A row with no witness step is RED. A witnessed row is
   judged by replaying the fresh envelope through the page's production
   verifier (refused steps admitted, as the corpus records them) up to and
   including its first witness step: the page must reach the outcome the Lean
   integrated step recorded there, refusals and unchanged aggregates
   included. */

const ijson = x => JSON.stringify(x);
const iVotes = agg => agg.payload.votes;
const iOpen = (gs, qid) => (gs.openQuestions.find(([k]) => k === qid) || [])[1] || null;
const iIsResp = (agg, k) =>
  agg.members.some(([key, m]) => key === k && m.roles.some(r => 'adminRole' in r));
const iApp = (st, tag) => (st.event.app && st.event.app[tag]) || null;
const iAppRefused = st => st.result.tag === 'refused' &&
  !!(st.result.error && st.result.error.integrated && st.result.error.integrated.app);

/* the proposer's own renounce, applied through the integrated root */
function iRenounce(st) {
  const ev = iApp(st, 'renounce');
  if (!ev || st.result.tag !== 'applied') return null;
  const pre = iVotes(st.input), post = iVotes(st.result.aggregate);
  const q = iOpen(pre, ev.questionId);
  if (!q || q.proposer !== st.signer) return null;
  return { qid: ev.questionId, q, pre, post, added: post.closed.slice(pre.closed.length),
    others: pre.openQuestions.filter(([k]) => k !== ev.questionId) };
}

/* a committed departure of a proposer of open questions, in one transition */
function iDeparture(st) {
  if (st.result.tag !== 'applied' || !st.result.change || !st.result.change.memberRemoved)
    return null;
  const key = st.result.change.memberRemoved;
  const pre = iVotes(st.input), post = iVotes(st.result.aggregate);
  const mine = pre.openQuestions.filter(([, q]) => q.proposer === key);
  if (!mine.length) return null;
  return { key, pre, post, mine, added: post.closed.slice(pre.closed.length) };
}
const departedRecord = (d, qid) =>
  d.added.find(c => c.questionId === qid && c.cause === 'proposerDeparted');

const ROWS81_INTEGRATED = {
  'L-1': st => { const r = iRenounce(st); return !!r && !iOpen(r.post, r.qid); },
  'L-2': st => { const d = iDeparture(st);
    return !!d && d.mine.every(([k]) => !iOpen(d.post, k) && !!departedRecord(d, k)); },
  'L-3': st => {
    const r = iRenounce(st), d = iDeparture(st);
    return (!!r && r.added.some(c => c.questionId === r.qid && c.verdict === 'negative')) ||
      (!!d && d.mine.every(([k]) => (departedRecord(d, k) || {}).verdict === 'negative'));
  },
  'L-4': st => {
    const r = iRenounce(st), d = iDeparture(st);
    return (!!r && r.added.some(c => c.questionId === r.qid && c.cause === 'renounced')) ||
      (!!d && d.mine.every(([k]) => !!departedRecord(d, k)));
  },
  'L-5': st => { const d = iDeparture(st);
    return !!d && ijson(d.post.closed.slice(0, d.pre.closed.length)) === ijson(d.pre.closed) &&
      d.mine.every(([k, q]) => ijson((departedRecord(d, k) || {}).question) === ijson(q)); },
  'L-6': st => { const d = iDeparture(st);
    return !!d && d.pre.openQuestions.some(([, q]) => q.proposer !== d.key) &&
      d.added.filter(c => c.cause === 'proposerDeparted').length === d.mine.length; },
  'L-6a': st => { const d = iDeparture(st);
    return !!d && d.mine.every(([k]) => !!departedRecord(d, k)) &&
      d.added.some(c => c.cause === 'franchiseChange' &&
        (iOpen(d.pre, c.questionId) || {}).proposer !== d.key); },
  'L-6b': st => { const r = iRenounce(st);
    return !!r && r.others.length > 0 && r.added.length === 1 &&
      r.others.every(([k, q]) => ijson(iOpen(r.post, k)) === ijson(q)); },
  'R-1': st => {
    const ev = iApp(st, 'renounce');
    if (!ev || !iAppRefused(st)) return false;
    const q = iOpen(iVotes(st.input), ev.questionId);
    return !!q && iIsResp(st.input, st.signer) && q.proposer !== st.signer;
  },
  'R-2': st => {
    const ev = iApp(st, 'cast');
    if (!ev || !iAppRefused(st)) return false;
    const q = iOpen(iVotes(st.input), ev.questionId);
    const perm = q && q.kind && typeof q.kind === 'object' && q.kind.permission;
    return !!perm && iIsResp(st.input, st.signer) && perm.designee !== st.signer;
  },
};

/* #76 closure-derived economic authorization in the fresh Lean corpus, by
   the same method: a row is recognised by what the integrated step did to
   the payload's `bindings` and `live` (absent = empty, as Lean's encoder
   omits them), never by a seed name or an index; a refusal row is single
   fault — every economic guard but the authorization holds at its input. */
const iPay = agg => agg.payload;
const iBindings = agg => iPay(agg).bindings || [];
const iLive = agg => iPay(agg).live || [];
const iCol = (agg, c) => iPay(agg).collections.find(x => x.id === c) || null;
const permT = c => ({ permission: { c } });
const backT = w => ({ backdonation: { w } });
const comuneOf = agg => (iPay(agg).conti.find(([k]) => k === 'comune') || [null, 0])[1];
const liveCount = (agg, target, verdict) =>
  iLive(agg).filter(a => ijson(a.target) === ijson(target) && a.verdict === verdict).length;
const targetValid = (agg, t) => 'permission' in t ? !!iCol(agg, t.permission.c) : t.backdonation.w > 0;
const iApplied = st => st.result.tag === 'applied';
const earlierGrant = (steps, i, c) => steps.slice(0, i).some(p => iApplied(p) &&
  (iApp(p, 'grantPermission') || {}).c === c);
function iBindRefused(st) {
  const ev = iApp(st, 'openBound');
  if (!ev || !iAppRefused(st) || !iIsResp(st.input, st.signer) || !targetValid(st.input, ev.target))
    return null;
  return { ev, q: iOpen(iVotes(st.input), ev.questionId),
    closed: iVotes(st.input).closed.some(r => r.questionId === ev.questionId) };
}
const earlierBackdonate = (steps, i, w) => steps.slice(0, i).some(p => iApplied(p) &&
  (iApp(p, 'backdonate') || {}).w === w);
/* a refused backdonate whose economic guards all hold — responsabile signer,
   positive share, a comune covering every member — and no positive closure
   bound to its share is live */
function iBackdonateRefused(st) {
  const ev = iApp(st, 'backdonate');
  if (!ev || !iAppRefused(st) || !iIsResp(st.input, st.signer) || !(ev.w > 0) ||
      comuneOf(st.input) < st.input.members.length * ev.w ||
      liveCount(st.input, backT(ev.w), 'positive') !== 0) return null;
  return ev;
}
function iDenyRefused(st) {
  const ev = iApp(st, 'denyPermission');
  if (!ev || !iAppRefused(st) || !iCol(st.input, ev.c) || !iIsResp(st.input, st.signer) ||
      liveCount(st.input, permT(ev.c), 'negative') !== 0) return null;
  return ev;
}
/* a live closure under `verdict` for a target other than `target` */
const liveElsewhere = (agg, target, verdict) =>
  iLive(agg).some(a => a.verdict === verdict && ijson(a.target) !== ijson(target));
function iGrantRefused(st) {
  const ev = iApp(st, 'grantPermission');
  if (!ev || !iAppRefused(st) || !iCol(st.input, ev.c) || !iIsResp(st.input, st.signer) ||
      liveCount(st.input, permT(ev.c), 'positive') !== 0) return null;
  return ev;
}

const ROWS76_INTEGRATED = {
  'bind': st => {
    const ev = iApp(st, 'openBound');
    if (!ev || !iApplied(st)) return false;
    const has = agg => iBindings(agg).some(([q, t]) => q === ev.questionId && ijson(t) === ijson(ev.target));
    return !has(st.input) && has(st.result.aggregate) &&
      iOpen(iVotes(st.result.aggregate), ev.questionId).proposer === st.signer;
  },
  'mint': st => {
    if (!iApplied(st)) return false;
    const pre = iVotes(st.input), post = iVotes(st.result.aggregate);
    const added = post.closed.slice(pre.closed.length);
    const was = new Set(iLive(st.input).map(ijson));
    return iLive(st.result.aggregate).some(a => !was.has(ijson(a)) &&
      added.some(r => r.questionId === a.questionId && r.verdict === a.verdict) &&
      iBindings(st.input).some(([q, t]) => q === a.questionId && ijson(t) === ijson(a.target)));
  },
  'spend-grant': st => {
    const ev = iApp(st, 'grantPermission');
    return !!ev && iApplied(st) && iCol(st.result.aggregate, ev.c).permitted &&
      liveCount(st.input, permT(ev.c), 'positive') ===
        liveCount(st.result.aggregate, permT(ev.c), 'positive') + 1;
  },
  'spend-deny': st => {
    const ev = iApp(st, 'denyPermission');
    return !!ev && iApplied(st) && !!iCol(st.input, ev.c) && !iCol(st.result.aggregate, ev.c) &&
      liveCount(st.input, permT(ev.c), 'negative') === 1 &&
      liveCount(st.result.aggregate, permT(ev.c), 'negative') === 0;
  },
  'spend-backdonate': st => {
    const ev = iApp(st, 'backdonate');
    return !!ev && iApplied(st) && liveCount(st.input, backT(ev.w), 'positive') ===
      liveCount(st.result.aggregate, backT(ev.w), 'positive') + 1 &&
      comuneOf(st.input) - comuneOf(st.result.aggregate) === st.input.members.length * ev.w;
  },
  'refuse-unbound': (st, i, steps) => {
    const ev = iGrantRefused(st);
    return !!ev && !earlierGrant(steps, i, ev.c) &&
      iVotes(st.input).closed.some(r => r.verdict === 'positive');
  },
  'refuse-spent': (st, i, steps) => {
    const ev = iGrantRefused(st);
    return !!ev && earlierGrant(steps, i, ev.c);
  },
  'refuse-grant-opposite': st => {
    const ev = iGrantRefused(st);
    return !!ev && liveCount(st.input, permT(ev.c), 'negative') > 0;
  },
  'refuse-grant-other-target': st => {
    const ev = iGrantRefused(st);
    return !!ev && liveElsewhere(st.input, permT(ev.c), 'positive');
  },
  'refuse-deny-opposite': st => {
    const ev = iDenyRefused(st);
    return !!ev && liveCount(st.input, permT(ev.c), 'positive') > 0;
  },
  'refuse-deny-other-target': st => {
    const ev = iDenyRefused(st);
    return !!ev && liveElsewhere(st.input, permT(ev.c), 'negative');
  },
  'refuse-bind-other': st => {
    const r = iBindRefused(st);
    return !!r && !!r.q && r.q.proposer !== st.signer;
  },
  'refuse-bind-balloted': st => {
    const r = iBindRefused(st);
    return !!r && !!r.q && r.q.proposer === st.signer &&
      r.q.assents.length + r.q.dissents.length > 0;
  },
  'refuse-bind-closed': st => {
    const r = iBindRefused(st);
    return !!r && !r.q && r.closed;
  },
  'refuse-backdonate-unclosed': (st, i, steps) => {
    const ev = iBackdonateRefused(st);
    return !!ev && !earlierBackdonate(steps, i, ev.w) &&
      !iLive(st.input).some(a => ijson(a.target) === ijson(backT(ev.w)));
  },
  'refuse-backdonate-negative': st => {
    const ev = iBackdonateRefused(st);
    return !!ev && liveCount(st.input, backT(ev.w), 'negative') > 0;
  },
  'refuse-backdonate-other-w': st => {
    const ev = iBackdonateRefused(st);
    return !!ev && iLive(st.input).some(a => 'backdonation' in a.target &&
      a.target.backdonation.w !== ev.w && a.verdict === 'positive');
  },
  'refuse-backdonate-spent': (st, i, steps) => {
    const ev = iBackdonateRefused(st);
    return !!ev && earlierBackdonate(steps, i, ev.w);
  },
};

/* the witness step alone: the page's transition, applied to the input Lean
   recorded for it, must reach the recorded outcome — refused, or applied
   with exactly the recorded aggregate. A behaviour broken at that step is
   RED there even when an earlier step would already diverge. */
function localOutcome(st, RG) {
  const asGs = agg => ({ ...agg, appFold: agg.payload });
  const det = RG.applyIntegrated(JSON.parse(ijson(asGs(st.input))), st.signer, st.event);
  if (st.result.tag === 'refused')
    return det.refused ? null : 'applicato dove Lean rifiuta';
  if (det.refused) return 'rifiutato (' + det.refused + ') dove Lean applica';
  return RG.canonAggregate(det.gs) === RG.canonAggregate(asGs(st.result.aggregate))
    ? null : 'post-aggregato diverso da Lean';
}

function judgeRows(rows, fresh, RG, opts = {}) {
  const out = [];
  for (const [row, isWitness] of Object.entries(rows)) {
    let hit = null;
    for (const n of Object.keys(fresh)) {
      const steps = fresh[n].steps;
      const i = steps.findIndex((st, k) => { try { return isWitness(st, k, steps); } catch { return false; } });
      if (i >= 0) { hit = { n, i }; break; }
    }
    if (!hit) { out.push({ row, ok: false, why: 'nessun passo testimone nel corpus Lean fresco' }); continue; }
    const env = fresh[hit.n];
    if (opts.local) {
      let why;
      try { why = localOutcome(env.steps[hit.i], RG); } catch (e) { why = 'eccezione: ' + e.message; }
      if (why) {
        out.push({ row, ok: false, at: `${hit.n}#${hit.i}`, why: 'il passo da solo: ' + why });
        continue;
      }
    }
    try {
      RG.verifyTraceV1({ ...env, steps: env.steps.slice(0, hit.i + 1) }, { withRefusals: true });
      out.push({ row, ok: true, at: `${hit.n}#${hit.i}` });
    } catch (e) {
      out.push({ row, ok: false, at: `${hit.n}#${hit.i}`,
        why: 'la pagina non raggiunge l’esito Lean: ' + e.message.slice(0, 200) });
    }
  }
  return out;
}

/* --- one full gate evaluation --------------------------------------------- */

function runGate(opts) {
  const html = opts.html || HTML;
  const reasons = [];
  let doc;
  try { doc = readFileSync(html, 'utf8'); }
  catch (e) { return { ok: false, reasons: ['HTML illeggibile: ' + e.message] }; }

  // fresh Lean regeneration (reusable across selftest controls)
  let freshRaw = opts.freshRaw;
  if (!freshRaw) {
    try {
      freshRaw = execFileSync('nix',
        ['develop', REPO, '-c', 'lake', 'env', 'lean', join(REPO, 'lean', 'TraceDriverV1.lean')],
        { cwd: join(REPO, 'lean'), encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] });
    } catch (e) {
      return { ok: false, reasons: ['il driver Lean committato fallisce: ' +
        (String(e.stdout || '') + String(e.stderr || '')).slice(-400)] };
    }
  }
  const freshSha = sha256(freshRaw);
  let fresh;
  try { fresh = JSON.parse(freshRaw); }
  catch (e) { return { ok: false, reasons: ['output del driver non è JSON valido'] }; }
  const freshNames = Object.keys(fresh);
  if (!freshNames.length || freshNames.some(n => !Array.isArray(fresh[n].steps) || !fresh[n].steps.length))
    return { ok: false, reasons: ['corpus fresco vuoto o con envelope senza passi'] };

  // embedded fixture and stated sha
  let emb;
  try { emb = extractEmbedded(doc); }
  catch (e) { return { ok: false, reasons: [e.message] } }
  if (emb.statedSha !== freshSha)
    reasons.push(`sha dichiarato ≠ output fresco del driver — dichiarato=${emb.statedSha.slice(0, 12)}… fresco=${freshSha.slice(0, 12)}…`);
  let fixture;
  try { fixture = JSON.parse(emb.fixtureText); }
  catch (e) { reasons.push('fixture incorporata non è JSON valido'); }
  if (fixture) {
    const fixNames = Object.keys(fixture);
    if (!fixNames.length || fixNames.some(n => !Array.isArray((fixture[n] || {}).steps) || !fixture[n].steps.length))
      reasons.push('corpus incorporato vuoto o con envelope senza passi');
    else if (JSON.stringify(fixNames.sort()) !== JSON.stringify(freshNames.slice().sort()))
      reasons.push(`envelope divergenti — freschi=[${freshNames}] incorporati=[${fixNames}]`);
    else {
      outer:
      for (const n of freshNames) {
        const a = fresh[n], b = fixture[n];
        if (a.steps.length !== b.steps.length) {
          reasons.push(`trace ${n}: ${a.steps.length} passi freschi vs ${b.steps.length} incorporati`);
          break;
        }
        for (let i = 0; i < a.steps.length; i++)
          if (JSON.stringify(a.steps[i]) !== JSON.stringify(b.steps[i])) {
            reasons.push(`trace ${n} passo ${i}: primo scarto strutturale — fresco=` +
              JSON.stringify(a.steps[i]).slice(0, 160) + '… incorporato=' +
              JSON.stringify(b.steps[i]).slice(0, 160) + '…');
            break outer;
          }
        if (JSON.stringify(a.initial) !== JSON.stringify(b.initial)) {
          reasons.push(`trace ${n}: stato iniziale divergente`);
          break;
        }
      }
    }
  }

  // execute the production JavaScript and replay through ITS conformance
  let prod;
  try { prod = loadProduction(doc); }
  catch (e) { return { ok: false, reasons: [...reasons, 'esecuzione produzione fallita: ' + e.message] }; }
  let embSteps = 0;
  try {
    const tc = prod.RG.traceConformance();
    embSteps = tc.steps;
    if (!Number.isInteger(embSteps) || embSteps <= 0)
      reasons.push('traceConformance di produzione non ha replayato passi');
  } catch (e) {
    reasons.push('traceConformance di produzione ROSSO: ' + e.message);
  }
  let freshSteps = 0;
  try {
    for (const n of freshNames)
      freshSteps += prod.RG.verifyTraceV1(fresh[n], { withRefusals: true }).steps;
  } catch (e) {
    reasons.push('replay JS di produzione sul corpus fresco ROSSO: ' + e.message);
  }

  const rows81 = judgeRows(ROWS81_INTEGRATED, fresh, prod.RG);
  for (const r of rows81)
    if (!r.ok) reasons.push(`riga #81 ${r.row}${r.at ? ' @' + r.at : ''} ROSSA: ${r.why}`);
  const rows76 = judgeRows(ROWS76_INTEGRATED, fresh, prod.RG, { local: true });
  for (const r of rows76)
    if (!r.ok) reasons.push(`riga #76 ${r.row}${r.at ? ' @' + r.at : ''} ROSSA: ${r.why}`);

  if (reasons.length) return { ok: false, reasons, rows81, rows76 };
  return { ok: true, envelopes: freshNames.length, embSteps, freshSteps,
    freshSha, scriptSha: prod.scriptSha, rows81, rows76 };
}

/* --- selftest: three negative axes, then production GREEN ------------------ */

function selftest(work) {
  const doc = readFileSync(HTML, 'utf8');
  // one fresh Lean run, reused by every control
  const freshRaw = execFileSync('nix',
    ['develop', REPO, '-c', 'lake', 'env', 'lean', join(REPO, 'lean', 'TraceDriverV1.lean')],
    { cwd: join(REPO, 'lean'), encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] });
  const emb = extractEmbedded(doc);
  const controls = [
    {
      name: 'post-stato mutato nella fixture incorporata',
      expect: /scarto strutturale|divergen/,
      make: () => doc.replace(emb.fixtureText,
        emb.fixtureText.replace('"amount":30', '"amount":31')),
    },
    {
      name: 'corpus incorporato svuotato',
      expect: /vuoto|senza passi|non trovato/,
      make: () => doc.replace(/const LEAN_TRACES_V1 = \{.*?\};\n/s,
        'const LEAN_TRACES_V1 = {};\n'),
    },
    {
      name: 'sha dichiarato mutato',
      expect: /sha dichiarato ≠/,
      make: () => doc.replace(emb.statedSha,
        (emb.statedSha[0] === '0' ? '1' : '0') + emb.statedSha.slice(1)),
    },
  ];
  // production mutants: the page's integrated transcription mutated in a
  // scratch copy, one #81 behaviour at a time; each must turn its row RED
  // through the same judgment that accepts production. A mutant whose edit
  // does not apply exactly once is itself RED (it would test nothing).
  const mutant = (from, to) => () => {
    const pairs = Array.isArray(from) ? from.map((f, k) => [f, to[k]]) : [[from, to]];
    let out = doc;
    for (const [f, t] of pairs) {
      const n = out.split(f).length - 1;
      if (n !== 1) throw new Error(`mutante non applicato: «${f.slice(0, 60)}» trovato ${n} volte`);
      out = out.replace(f, t);
    }
    return out;
  };
  const HOOK_DEPART = '? vtCloseProposerQuestions(change.memberRemoved, cleaned.votes) : cleaned.votes;';
  controls.push(
    { name: 'renounce del proponente resta aperto (L-1)', expect: /riga #81 L-1 /,
      make: mutant("if (q) effected = { openQuestions: vtErase(questionId, gs.openQuestions),",
        "if (false) effected = { openQuestions: vtErase(questionId, gs.openQuestions),") },
    { name: 'l’uscita non chiude le domande del proponente (L-2)', expect: /riga #81 L-2 /,
      make: mutant(HOOK_DEPART, '? cleaned.votes : cleaned.votes;') },
    { name: 'l’uscita chiude positivo (L-3)', expect: /riga #81 L-(2|3) /,
      make: mutant("verdict: 'negative', cause: 'proposerDeparted'", "verdict: 'positive', cause: 'proposerDeparted'") },
    { name: 'l’uscita registra .tally (L-4)', expect: /riga #81 L-(2|4) /,
      make: mutant("verdict: 'negative', cause: 'proposerDeparted'", "verdict: 'negative', cause: 'tally'") },
    { name: 'l’uscita chiude e scarta i record (L-5)', expect: /riga #81 L-(2|5) /,
      make: mutant('closed: gs.closed.concat(mine.map(', 'closed: gs.closed.concat([].map(') },
    { name: 'l’uscita chiude ogni domanda aperta (L-6)', expect: /riga #81 L-(2|6) /,
      make: mutant(['const mine = gs.openQuestions.filter(([, q]) => q.proposer === proposer);',
                    'openQuestions: gs.openQuestions.filter(([, q]) => q.proposer !== proposer),'],
        ['const mine = gs.openQuestions.slice();', 'openQuestions: [],']) },
    { name: 'nessuna ricomputazione dopo l’uscita (L-6a)', expect: /riga #81 L-6a /,
      make: mutant('const swept = vtSweep(vtTheta, post, departed);', 'const swept = departed;') },
    { name: 'renounce tocca le domande estranee (L-6b)', expect: /riga #81 L-6b /,
      make: mutant("if (q) effected = { openQuestions: vtErase(questionId, gs.openQuestions),",
        "if (q) effected = { openQuestions: [],") },
    { name: 'renounce di un non proponente accettato (R-1)', expect: /riga #81 R-1 /,
      make: mutant("return signer === q.proposer ? null : 'notProposer';", 'return null;') },
    { name: 'voto di un non designato registrato (R-2)', expect: /riga #81 R-2 /,
      make: mutant("return perm && signer !== perm.designee ? 'notDesignee' : null;", 'return null;') },
  );
  // #76: the page's closure-derived authorization mutated, one behaviour at
  // a time; each must turn its row RED through the same judgment
  const BIND = '{ ...s, bindings: [[questionId, target], ...s.bindings] }';
  const SPEND = 'return effect({ ...s, live: pulled[1] });';
  const NOAUTH = 'if (pulled === null) return { ok: false, failed: [NOLIVE(target, verdict)] };';
  const BACKDONATE = "return spendThen(backT(args.w), 'positive', s, s1 => attempt(view, s1, { tag, author: signer, ...args }));";
  const TOY_ONLY = 'return attempt(view, s, { tag, author: signer, ...args });';
  const PULL_MATCH = 'sameTarget(a.target, target) && a.verdict === verdict';
  const TARGET_BLIND = 'a.verdict === verdict';
  const VERDICT_BLIND = 'sameTarget(a.target, target)';
  controls.push(
    { name: 'openBound non lega il bersaglio (bind)', expect: /riga #76 bind @[BC]#\d+ ROSSA: il passo da solo/,
      make: mutant(BIND, 's') },
    { name: 'la chiusura legata non conia l’autorizzazione (mint)', expect: /riga #76 mint @[BC]#\d+ ROSSA: il passo da solo/,
      make: mutant('if (target !== null) minted.push(', 'if (false) minted.push(') },
    { name: 'il permesso non spende la chiusura (spend-grant)', expect: /riga #76 spend-grant @[BC]#\d+ ROSSA: il passo da solo/,
      make: mutant(SPEND, 'return effect(s);') },
    { name: 'il diniego legge il verdetto positivo (spend-deny)', expect: /riga #76 spend-deny @[BC]#\d+ ROSSA: il passo da solo/,
      make: mutant("const pulled = pullLive(permT(c), 'negative', s.live);",
        "const pulled = pullLive(permT(c), 'positive', s.live);") },
    { name: 'permesso senza chiusura legata applicato (refuse-unbound)', expect: /riga #76 refuse-unbound @[BC]#\d+ ROSSA: il passo da solo/,
      make: mutant(NOAUTH, 'if (pulled === null) return effect(s);') },
    { name: 'chiusura spesa due volte (refuse-spent)', expect: /riga #76 refuse-spent /,
      make: mutant(SPEND, 'return effect(s);') },
    { name: 'legame di una domanda aperta altrui (refuse-bind-other)', expect: /riga #76 refuse-bind-other @[BC]#\d+ ROSSA: il passo da solo/,
      make: mutant('vtLookup(qid, s.votes.openQuestions) === null', 'true') },
    { name: 'legame dopo un voto (refuse-bind-balloted)', expect: /riga #76 refuse-bind-balloted @[BC]#\d+ ROSSA: il passo da solo/,
      make: mutant('vtLookup(qid, s.votes.openQuestions) === null', 'true') },
    { name: 'legame di una domanda chiusa (refuse-bind-closed)', expect: /riga #76 refuse-bind-closed @[BC]#\d+ ROSSA: il passo da solo/,
      make: mutant('!s.votes.closed.some(r => r.questionId === qid)', 'true') },
    { name: 'redistribuzione sul solo TOY_AUTH (spend-backdonate)', expect: /riga #76 spend-backdonate @[BC]#\d+ ROSSA: il passo da solo/,
      make: mutant(BACKDONATE, TOY_ONLY) },
    { name: 'redistribuzione senza chiusura (refuse-backdonate-unclosed)',
      expect: /riga #76 refuse-backdonate-unclosed @[BC]#\d+ ROSSA: il passo da solo/, make: mutant(BACKDONATE, TOY_ONLY) },
    { name: 'spesa cieca al verdetto: redistribuzione (refuse-backdonate-negative)',
      expect: /riga #76 refuse-backdonate-negative @[BC]#\d+ ROSSA: il passo da solo/, make: mutant(PULL_MATCH, VERDICT_BLIND) },
    { name: 'spesa cieca al bersaglio: redistribuzione (refuse-backdonate-other-w)',
      expect: /riga #76 refuse-backdonate-other-w @[BC]#\d+ ROSSA: il passo da solo/, make: mutant(PULL_MATCH, TARGET_BLIND) },
    { name: 'spesa cieca al verdetto: permesso (refuse-grant-opposite)',
      expect: /riga #76 refuse-grant-opposite @[BC]#\d+ ROSSA: il passo da solo/, make: mutant(PULL_MATCH, VERDICT_BLIND) },
    { name: 'spesa cieca al bersaglio: permesso (refuse-grant-other-target)',
      expect: /riga #76 refuse-grant-other-target @[BC]#\d+ ROSSA: il passo da solo/, make: mutant(PULL_MATCH, TARGET_BLIND) },
    { name: 'spesa cieca al verdetto: diniego (refuse-deny-opposite)',
      expect: /riga #76 refuse-deny-opposite @[BC]#\d+ ROSSA: il passo da solo/, make: mutant(PULL_MATCH, VERDICT_BLIND) },
    { name: 'spesa cieca al bersaglio: diniego (refuse-deny-other-target)',
      expect: /riga #76 refuse-deny-other-target @[BC]#\d+ ROSSA: il passo da solo/, make: mutant(PULL_MATCH, TARGET_BLIND) },
    { name: 'redistribuzione spesa due volte (refuse-backdonate-spent)',
      expect: /riga #76 refuse-backdonate-spent /, make: mutant(SPEND, 'return effect(s);') },
  );
  for (const c of controls) {
    const p = join(work, 'sab.html');
    let sab;
    try { sab = c.make(); }
    catch (e) {
      console.error(`SELFTEST RED: controllo «${c.name}»: ${e.message}`);
      return 1;
    }
    writeFileSync(p, sab);
    const r = runGate({ html: p, freshRaw });
    if (r.ok) {
      console.error(`SELFTEST RED: controllo «${c.name}» ACCETTATO dal gate`);
      return 1;
    }
    const text = r.reasons.join('\n');
    if (!c.expect.test(text)) {
      console.error(`SELFTEST RED: «${c.name}» fallito per il motivo sbagliato:\n${text.slice(0, 400)}`);
      return 1;
    }
    console.log(`controllo negativo «${c.name}»: RED come atteso — ${text.split('\n')[0].slice(0, 120)}`);
  }
  const green = runGate({ freshRaw });
  if (!green.ok) {
    console.error('SELFTEST RED: il gate di produzione non torna GREEN:\n' + green.reasons.join('\n'));
    return 1;
  }
  report(green, `selftest GREEN: ${controls.length} controlli negativi RED per il motivo atteso; `);
  return 0;
}

function report(r, prefix) {
  console.log((prefix || '') +
    `GREEN: ${r.envelopes} envelope; rigenerazione Lean identica (sha ${r.freshSha.slice(0, 12)}…); ` +
    `replay di produzione: ${r.embSteps} passi sul corpus incorporato + ${r.freshSteps} sul corpus fresco ` +
    `(script eseguito, sha ${r.scriptSha.slice(0, 12)}…); ` +
    `righe #81: ${r.rows81.map(x => x.row + '@' + x.at).join(' ')}; ` +
    `righe #76: ${r.rows76.map(x => x.row + '@' + x.at).join(' ')}`);
}

/* --- CLI ------------------------------------------------------------------- */

const work = mkdtempSync(join(tmpdir(), 'rg-trace-gate-'));
let code = 1;
try {
  if (process.argv.includes('--selftest')) {
    code = selftest(work);
  } else {
    const r = runGate({});
    if (r.ok) { report(r); code = 0; }
    else {
      console.error(`RED: ${r.reasons.length} problemi`);
      r.reasons.forEach(x => console.error(' - ' + x));
      code = 1;
    }
  }
} finally {
  rmQuiet(work);
}
process.exit(code);
