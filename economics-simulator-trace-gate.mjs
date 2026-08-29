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
 *   7. prints counts, the fresh sha, and GREEN only after BOTH Lean
 *      regeneration equivalence and production-JS replay succeed.
 *
 * Usage from any working directory:
 *   node /path/to/repo/economics-simulator-trace-gate.mjs
 *   node /path/to/repo/economics-simulator-trace-gate.mjs --selftest
 *
 * --selftest proves the gate can fail: a mutated post-state in a scratch
 * copy of the embedded envelope, an emptied embedded corpus, and a mutated
 * stated sha — each RED for its intended reason — then production GREEN.
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
    for (const n of freshNames) freshSteps += prod.RG.verifyTraceV1(fresh[n]).steps;
  } catch (e) {
    reasons.push('replay JS di produzione sul corpus fresco ROSSO: ' + e.message);
  }

  if (reasons.length) return { ok: false, reasons };
  return { ok: true, envelopes: freshNames.length, embSteps, freshSteps,
    freshSha, scriptSha: prod.scriptSha };
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
  for (const c of controls) {
    const p = join(work, 'sab.html');
    writeFileSync(p, c.make());
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
  report(green, 'selftest GREEN: 3 controlli negativi RED per il motivo atteso; ');
  return 0;
}

function report(r, prefix) {
  console.log((prefix || '') +
    `GREEN: ${r.envelopes} envelope; rigenerazione Lean identica (sha ${r.freshSha.slice(0, 12)}…); ` +
    `replay di produzione: ${r.embSteps} passi sul corpus incorporato + ${r.freshSteps} sul corpus fresco ` +
    `(script eseguito, sha ${r.scriptSha.slice(0, 12)}…)`);
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
  rmSync(work, { recursive: true, force: true });
}
process.exit(code);
