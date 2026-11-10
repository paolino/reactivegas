#!/usr/bin/env node
/*
 * economics-simulator-vote-corpus-oracle-gate.mjs — the simulator replays the
 * Lean vote corpus (lean/corpus/vote.json) step by step through its own
 * integrated root, with the threshold table of lean/corpus/vote.context.json
 * as the only policy, and agrees with every step Lean recorded.
 *
 *   O-REPLAY         the committed corpus + sidecar replay to PASS: per step,
 *                    accepted, post-state, change and voteError equal the
 *                    corpus's, through applyIntegrated (the code the page runs)
 *   O-ONE-ROOT       exactly one applyIntegrated call per replayed step
 *   O-THRESHOLD-ROUTES  the integrated root takes the threshold on the vote route and
 *                    on the base-hook route: a constant-zero threshold changes both
 *   O-SIDECAR-ONLY   a core whose named threshold throws replays to PASS, and
 *                    the table was queried: no named threshold takes part
 *   O-WITNESS        legacy and zero tables over the same domain disagree at a
 *                    step with a collective query at n >= 1; with none the
 *                    corpus is inadequate
 *   O-CONTROLS       replay-context contract R3.1 section 7 rows 1-9, each a
 *                    mutation of in-memory copies of the corpus/sidecar bytes
 *   O-PRE-REPLAY     rows 3, 5, 6, 7: refused before any step runs, naming
 *                    the rule
 *   O-ABORT          row 4: abort at the point of query, with n and domain
 *   O-MISMATCH       row 2: a mismatch names the step and the field
 *   O-DISTINCT       the three outcomes carry distinct tags
 *   O-BUDGET         the run fits the stated wall budget
 *
 * Usage:
 *   node economics-simulator-vote-corpus-oracle-gate.mjs
 *   node economics-simulator-vote-corpus-oracle-gate.mjs --selftest
 *
 * --selftest edits the core's source in a scratch copy (a threshold dropped on
 * the hook route, dropped on the vote route, ignored in the verdict, an abort
 * swallowed) and requires the matching row RED for its stated reason. An edit
 * that does not apply exactly once is a broken control, not a caught mutant.
 * The tracked corpus, sidecar and page are only read.
 */

import { readFileSync, writeFileSync, mkdtempSync, rmSync } from 'node:fs';
import { createHash } from 'node:crypto';
import { dirname, join } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { tmpdir } from 'node:os';

const REPO = dirname(fileURLToPath(import.meta.url));
const CORE_PATH = join(REPO, 'economics-simulator-core.mjs');
const ORACLE_PATH = join(REPO, 'economics-simulator-vote-corpus-oracle.mjs');
const CORPUS_PATH = join(REPO, 'lean', 'corpus', 'vote.json');
const CONTEXT_PATH = join(REPO, 'lean', 'corpus', 'vote.context.json');
const BUDGET_MS = 10000;   // each of the run and the selftest; the two add at most 20 s
const sha256 = b => createHash('sha256').update(b).digest('hex');

let oracle = null, oracleError = null;
try { oracle = await import(pathToFileURL(ORACLE_PATH).href); }
catch (e) { oracleError = e; }

const TAGS = { refusal: 'PRE-REPLAY REFUSAL', abort: 'RUNTIME ABORT',
  mismatch: 'CONFORMANCE MISMATCH', pass: 'PASS' };

/* --- scratch copies of the core ------------------------------------------- */

function rmQuiet(p) { try { rmSync(p, { recursive: true, force: true }); } catch { /* housekeeping */ } }

/* a core module built from source text; an edit is applied exactly once */
function edited(source, needle, replacement, what) {
  if (source.split(needle).length !== 2)
    throw new Error(`selftest: la modifica «${what}» non si applica esattamente una volta`);
  return source.replace(needle, replacement);
}
let scratchSeq = 0;
async function loadSource(work, source) {
  const p = join(work, `core-${scratchSeq++}.mjs`);
  writeFileSync(p, source);
  return import(pathToFileURL(p).href);
}
/* the machine's named threshold throws when reached */
const TRIPWIRE_NEEDLE = 'const vtTheta = VT_THRESHOLDS[VT_THRESHOLD_NAME];';
const tripwired = source => edited(source, TRIPWIRE_NEEDLE,
  "const vtTheta = () => { throw new Error('NAMED-THRESHOLD-REACHED'); };", 'named threshold throws');

/* the core, counting the integrated-root calls */
function counted(core) {
  let calls = 0;
  return { core: { ...core, applyIntegrated: (...a) => { calls++; return core.applyIntegrated(...a); } },
    calls: () => calls };
}

/* --- mutations of the bytes the replay reads (in memory only) ------------- */

const mutateContext = (bytes, f) => {
  const ctx = JSON.parse(Buffer.from(bytes).toString('utf8'));
  f(ctx);
  const out = Buffer.from(JSON.stringify(ctx));
  if (out.equals(Buffer.from(bytes))) throw new Error('mutazione non applicata: i byte sono invariati');
  return out;
};

/* --- the rows ------------------------------------------------------------- */

async function runAll(core, source, work) {
  const rows = [];
  const add = (id, ok, text) => { rows.push({ id, ok, text }); };
  const absent = () => `nessuna riproduzione del corpus voto: ${oracleError ? oracleError.message : 'modulo assente'}`;
  const row = async (id, f) => {
    if (!oracle) return add(id, false, absent());
    try { const r = await f(); add(id, r.ok, r.text); }
    catch (e) { add(id, false, 'eccezione: ' + String(e && e.stack || e).split('\n').slice(0, 3).join(' | ')); }
  };
  const corpusBytes = readFileSync(CORPUS_PATH);
  const contextBytes = readFileSync(CONTEXT_PATH);
  const onDisk = sha256(corpusBytes) + sha256(contextBytes);
  const verify = (c, x, theCore) => { const k = counted(theCore || core);
    return { outcome: oracle.verifyCorpus(c, x, k.core), calls: k.calls() }; };
  const describe = o => o.tag + (o.rules ? ' ' + o.rules.join(',') : '') +
    (o.step !== undefined ? ` step=${o.step}` : '') + (o.field ? ` field=${o.field}` : '') +
    (o.n !== undefined ? ` n=${o.n} domain={0..${o.domain}}` : '');
  let witnessInfo = null;

  await row('O-REPLAY', () => {
    const { outcome, calls } = verify(corpusBytes, contextBytes);
    const queried = outcome.queries ? outcome.queries.length : 0;
    return { ok: outcome.tag === TAGS.pass && queried > 0 && outcome.steps > 1 &&
        outcome.steps === JSON.parse(corpusBytes.toString('utf8')).steps.length,
      text: `${describe(outcome)} steps=${outcome.steps} table-queries=${queried} root-calls=${calls}` };
  });

  await row('O-ONE-ROOT', () => {
    const { outcome, calls } = verify(corpusBytes, contextBytes);
    return { ok: outcome.tag === TAGS.pass && calls === outcome.steps,
      text: `root-calls=${calls} steps=${outcome.steps}` };
  });

  /* the integrated root takes the threshold: the same event under the default and under a
     constant-zero threshold closes a question on the vote route and on the base-hook route */
  const routes = () => {
    const initial = JSON.parse(corpusBytes.toString('utf8')).initial;
    const gs0 = { members: initial.members, pendingBase: [], appFold: initial.appFold };
    const open = { app: { openQuestion: { kind: 'collective', questionId: 'q:route' } } };
    const admit = { direct: { admitMember: { email: 'zoe@route', key: 'zoe',
      roles: [{ adminRole: { admin: 'publicAdmin' } }] } } };
    const zero = () => 0;
    const closed = det => det.refused ? 'refused:' + det.refused : det.gs.appFold.votes.closed.length;
    const voteDefault = closed(core.applyIntegrated(gs0, 'anna', open));
    const voteZero = closed(core.applyIntegrated(gs0, 'anna', open, zero));
    const opened = core.applyIntegrated(gs0, 'anna', open).gs;
    const hookDefault = closed(core.applyIntegrated(opened, 'anna', admit));
    const hookZero = closed(core.applyIntegrated(opened, 'anna', admit, zero));
    return { ok: voteDefault === 0 && voteZero === 1 && hookDefault === 0 && hookZero === 1,
      text: `closed questions, default vs zero threshold — vote route ${voteDefault}/${voteZero}, hook route ${hookDefault}/${hookZero}` };
  };
  try { const r = routes(); add('O-THRESHOLD-ROUTES', r.ok, r.text); }
  catch (e) { add('O-THRESHOLD-ROUTES', false, 'eccezione: ' + String(e && e.stack || e).split('\n').slice(0, 3).join(' | ')); }

  await row('O-SIDECAR-ONLY', async () => {
    const trip = await loadSource(work, tripwired(source));
    const { outcome } = verify(corpusBytes, contextBytes, trip);
    const queried = outcome.queries ? outcome.queries.length : 0;
    return { ok: outcome.tag === TAGS.pass && queried > 0,
      text: `named threshold throwing: ${describe(outcome)} table-queries=${queried}` };
  });

  await row('O-WITNESS', () => {
    const { context } = oracle.validateContext(contextBytes, corpusBytes);
    const N = context.table.length - 1;
    const legacy = Array.from({ length: N + 1 }, (_, n) => [n, core.vtTheta(n)]);
    const zero = Array.from({ length: N + 1 }, (_, n) => [n, 0]);
    const withTable = table => verify(corpusBytes,
      mutateContext(contextBytes, c => { c.policies.threshold.table = table; })).outcome;
    const l = withTable(legacy), z = withTable(zero);
    if (l.tag !== TAGS.pass)
      return { ok: false, text: `la tabella legacyThreshold non riproduce il corpus: ${describe(l)}` };
    if (z.tag !== TAGS.mismatch)
      return { ok: false, text: `corpus inadeguato come testimone della politica: la tabella zero dà ${describe(z)}` };
    const at = l.queries.filter(q => q.step === z.step && q.n >= 1);
    if (!at.length)
      return { ok: false, text: `corpus inadeguato: nessuna query collettiva a n >= 1 al passo ${z.step}` };
    const first = l.queries.find(q => q.n >= 1);
    witnessInfo = { step: z.step, field: z.field, n: first.n, queriedAtStep: at.map(q => q.n) };
    return { ok: true, text: `primo passo che differisce: ${z.step} (campo ${z.field}), query a n=${at.map(q => q.n)}; ` +
      `prima query a n>=1 del replay: ${first.n}` };
  });

  const control = (n, name, bytes, expect) => row(`O-CONTROLS.${n}`, () => {
    const { outcome, calls } = verify(corpusBytes, bytes.ctx || contextBytes);
    const r = expect(outcome, calls);
    return { ok: r.ok, text: `${name}: ${describe(outcome)}${r.note ? ' — ' + r.note : ''}` };
  });
  const needWitness = () => { if (!witnessInfo) throw new Error('nessun testimone della politica: controllo non valutabile'); return witnessInfo; };
  const refusedBefore = rule => (o, calls) => ({ ok: o.tag === TAGS.refusal && calls === 0 &&
    o.rules.includes(rule), note: `steps-run=${calls}` });

  await control(1, 'none', {}, o => ({ ok: o.tag === TAGS.pass }));
  await control(2, 'zero table', { get ctx() { return mutateContext(contextBytes,
    c => { c.policies.threshold.table = c.policies.threshold.table.map(([k]) => [k, 0]); }); } },
  o => { const w = needWitness(); return { ok: o.tag === TAGS.mismatch && o.step === w.step,
    note: `witnessed step ${w.step}` }; });
  await control(3, 'digest corrupted', { get ctx() { return mutateContext(contextBytes, c => {
    const d = c.corpus.contentSha256; c.corpus.contentSha256 = (d[0] === '0' ? '1' : '0') + d.slice(1); }); } },
  refusedBefore('digest'));
  await control(4, 'table truncated below the witnessed n', { get ctx() { const w = needWitness();
    return mutateContext(contextBytes, c => { c.policies.threshold.table =
      c.policies.threshold.table.slice(0, w.n); }); } },
  (o, calls) => { const w = needWitness(); return { ok: o.tag === TAGS.abort && o.n === w.n &&
    o.domain === w.n - 1 && calls === o.step + 1,
  note: `witnessed n=${w.n}, root-calls=${calls}` }; });
  await control(5, 'duplicate key', { get ctx() { return mutateContext(contextBytes,
    c => { const t = c.policies.threshold.table; t.splice(2, 0, t[1].slice()); }); } },
  refusedBefore('table-rule-4'));
  await control(6, 'float value', { get ctx() { return mutateContext(contextBytes,
    c => { c.policies.threshold.table[2][1] = 1.5; }); } },
  refusedBefore('table-rule-1'));
  await control(7, 'unknown semantics', { get ctx() { return mutateContext(contextBytes,
    c => { c.policies.backdonateAuth.semantics = 'always-allow'; }); } },
  refusedBefore('semantics'));
  await control(8, 'labels, source and provenance changed', { get ctx() { return mutateContext(contextBytes, c => {
    c.policies.threshold.label = 'zeroThreshold'; c.policies.threshold.source.gitBlobOid = '0'.repeat(40);
    c.policies.backdonateAuth.label = 'renamed'; c.generatedFrom.sourceInputDigest = '0'.repeat(64);
    c.generatedFrom.exporter = 'elsewhere'; }); } },
  o => ({ ok: o.tag === TAGS.pass }));
  await control(9, 'restored', {}, o => {
    const same = sha256(readFileSync(CORPUS_PATH)) + sha256(readFileSync(CONTEXT_PATH)) === onDisk &&
      Buffer.from(contextBytes).equals(readFileSync(CONTEXT_PATH));
    return { ok: o.tag === TAGS.pass && same, note: `tracked bytes unchanged=${same}` };
  });

  const ctl = n => rows.find(r => r.id === `O-CONTROLS.${n}`);
  const grouped = (id, ns, what) => add(id, ns.every(n => ctl(n) && ctl(n).ok),
    `${what}: ` + ns.map(n => `control ${n} ${ctl(n) && ctl(n).ok ? 'ok' : 'FAILED'}`).join(', '));
  if (oracle) {
    grouped('O-PRE-REPLAY', [3, 5, 6, 7], 'refused before any step, naming the rule');
    grouped('O-ABORT', [4], 'abort at the point of query');
    grouped('O-MISMATCH', [2], 'mismatch names step and field');
    const tags = [2, 3, 4].map(n => ctl(n).text.split(': ')[1].split(' ').slice(0, 2).join(' '));
    const found = [TAGS.mismatch, TAGS.refusal, TAGS.abort];
    add('O-DISTINCT', found.every((t, i) => tags[i].startsWith(t)) && new Set(found).size === 3,
      'tags: ' + tags.join(' | '));
  } else for (const id of ['O-PRE-REPLAY', 'O-ABORT', 'O-MISMATCH', 'O-DISTINCT']) add(id, false, absent());
  return rows;
}

function report(rows, ms) {
  for (const r of rows) console.log(`${r.ok ? 'ROW-PASS' : 'ROW-FAIL'} ${r.id} ${r.text}`);
  const budget = ms <= BUDGET_MS;
  console.log(`${budget ? 'ROW-PASS' : 'ROW-FAIL'} O-BUDGET ${Math.round(ms)} ms of ${BUDGET_MS} ms`);
  return rows.every(r => r.ok) && budget;
}

/* --- selftest: each mutant core must turn its row RED for its reason ------- */

async function selftest(work, source) {
  if (!oracle) { console.error('SELFTEST RED: ' + (oracleError ? oracleError.message : 'modulo assente')); return 1; }
  const mutants = [
    { name: 'hook route drops the threshold',
      needle: 'const swept = vtSweep(theta, post, departed);', to: 'const swept = vtSweep(vtTheta, post, departed);',
      expect: [['O-SIDECAR-ONLY', 'NAMED-THRESHOLD-REACHED']] },
    { name: 'vote route drops the threshold',
      needle: 'const det = vtApply(view, s.votes, signer, ev, theta);', to: 'const det = vtApply(view, s.votes, signer, ev);',
      expect: [['O-SIDECAR-ONLY', 'NAMED-THRESHOLD-REACHED']] },
    { name: 'verdict ignores the threshold value',
      needle: 'const required = theta(vtFranchiseSize(view));',
      to: 'const required = Math.floor((vtFranchiseSize(view) + 1) / 2);',
      expect: [['O-WITNESS', 'inadeguato'], ['O-CONTROLS.2', 'PASS']] },
    { name: 'verdict swallows a domain abort',
      needle: 'const required = theta(vtFranchiseSize(view));',
      to: 'const required = (() => { try { return theta(vtFranchiseSize(view)); } catch { return 0; } })();',
      expect: [['O-CONTROLS.4', 'PASS']] },
  ];
  let bad = 0;
  const t0 = performance.now();
  const production = await runAll(await import(pathToFileURL(CORE_PATH).href), source, work);
  if (!production.every(r => r.ok)) { console.error('SELFTEST RED: la produzione non torna GREEN'); return 1; }
  for (const m of mutants) {
    const src = edited(source, m.needle, m.to, m.name);
    const rows = await runAll(await loadSource(work, src), src, work);
    for (const [id, why] of m.expect) {
      const r = rows.find(x => x.id === id);
      const ok = r && !r.ok && r.text.includes(why);
      console.log(`${ok ? 'CONTROL-RED' : 'CONTROL-NOT-RED'} ${m.name} -> ${id} (${r ? r.text.slice(0, 110) : 'riga assente'})`);
      if (!ok) bad++;
    }
  }
  const ms = performance.now() - t0;
  if (ms > BUDGET_MS) { console.error(`SELFTEST RED: ${Math.round(ms)} ms oltre il budget ${BUDGET_MS} ms`); bad++; }
  console.log(bad ? `SELFTEST RED: ${bad} controlli non rossi` :
    `selftest GREEN: ${mutants.length} mutanti RED per il motivo atteso; produzione GREEN; ${Math.round(ms)} ms`);
  return bad ? 1 : 0;
}

if (process.argv[1] && pathToFileURL(process.argv[1]).href === import.meta.url) {
  const work = mkdtempSync(join(tmpdir(), 'rg-vote-oracle-'));
  let code = 1;
  try {
    const source = readFileSync(CORE_PATH, 'utf8');
    if (process.argv.includes('--selftest')) code = await selftest(work, source);
    else {
      const t0 = performance.now();
      const core = await import(pathToFileURL(CORE_PATH).href);
      const rows = await runAll(core, source, work);
      const ok = report(rows, performance.now() - t0);
      console.log(ok ? `GREEN: ${rows.length} righe, riproduzione del corpus voto` : 'RED: righe fallite');
      code = ok ? 0 : 1;
    }
  } finally { rmQuiet(work); }
  process.exit(code);
}
