#!/usr/bin/env node
/*
 * economics-simulator-claim-gate.mjs — the executable verifier of the claim
 * manifest embedded in economics-simulator.html.
 *
 * The simulator's CHECK_RECEIPT asserts that every Lean declaration cited by
 * its CLAIMS manifest elaborates in the repository's Lean sources and that
 * the receipt is bound to a real run. This gate is what performs that
 * verification. Fresh on every run it:
 *
 *   1. reads the committed economics-simulator.html;
 *   2. extracts every theorem/definition citation directly from the embedded
 *      CLAIMS manifest — never from a copied citation list;
 *   3. validates row shape (including NON PROVATO rows carrying no refs);
 *   4. verifies every cited file:line and every pinned source sha256 against
 *      the repository Lean files;
 *   5. generates a temporary Lean driver with #check for the extracted
 *      distinct declaration set;
 *   6. runs it in the repository's actual lean/ lake environment;
 *   7. hashes the fresh driver output and compares it to CHECK_RECEIPT.sha;
 *   8. requires CHECK_RECEIPT.decls to equal the extracted citation set;
 *   9. exits nonzero with a precise reason on any mismatch, zero on GREEN.
 *
 * Usage, from a clean checkout (any working directory):
 *   node economics-simulator-claim-gate.mjs             # gate run
 *   node economics-simulator-claim-gate.mjs --selftest  # negative controls
 *
 * --selftest proves the gate can fail on all three mandatory axes — bogus
 * citation, mutated receipt sha, touched Lean source — each for its intended
 * reason, then runs the unmodified production gate GREEN. Temporary
 * artifacts live in a fresh mkdtemp directory; the repository stays clean.
 */

import { readFileSync, writeFileSync, mkdtempSync, mkdirSync, rmSync, cpSync } from 'node:fs';
import { createHash } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { tmpdir } from 'node:os';

const REPO = dirname(fileURLToPath(import.meta.url));
const HTML = join(REPO, 'economics-simulator.html');
const sha256 = b => createHash('sha256').update(b).digest('hex');

/* Parse the embedded CLAIMS manifest and CHECK_RECEIPT out of an HTML body. */
function extract(doc) {
  const mm = doc.match(/const CLAIMS = \{([\s\S]*?)\n\};/);
  if (!mm) throw new Error('manifesto CLAIMS non trovato nel documento');
  const rowRe = /'([a-z0-9-]+)':\s*\{ c: .*?k: '(teorema|definizione|NON PROVATO)', d: (null|'([A-Za-z_.]+)'), f: (null|'([^']+)'), l: (null|\d+) \}/g;
  const rows = [];
  let r;
  while ((r = rowRe.exec(mm[1])) !== null)
    rows.push({ id: r[1], k: r[2], d: r[4] || null, f: r[6] || null,
      l: r[7] === 'null' ? null : Number(r[7]) });
  if (!rows.length) throw new Error('nessuna riga estraibile dal manifesto');
  const rm = doc.match(/const CHECK_RECEIPT = \{[\s\S]*?sha: '([0-9a-f]{64})',[\s\S]*?decls: \[([\s\S]*?)\],[\s\S]*?sources: \{([\s\S]*?)\},\n\};/);
  if (!rm) throw new Error('CHECK_RECEIPT non trovato nel documento');
  return {
    rows,
    cited: [...new Set(rows.filter(x => x.d).map(x => x.d))].sort(),
    sha: rm[1],
    decls: [...rm[2].matchAll(/'([A-Za-z_.]+)'/g)].map(x => x[1]).sort(),
    sources: Object.fromEntries(
      [...rm[3].matchAll(/'([^']+)':\s*'([0-9a-f]{64})'/g)].map(x => [x[1], x[2]])),
  };
}

/*
 * Run the gate. opts:
 *   html        path to the artifact (default: committed HTML)
 *   sourcesRoot root for reading the pinned Lean sources (default: repo);
 *               the selftest's touched-source control overrides this so the
 *               SAME production hash-check path fails, before any lake run
 *   lakeRepo    repo whose lean/ lake environment runs the driver
 *   work        scratch dir for the generated driver and its output
 * Returns { ok: true } or { ok: false, reasons: [...] }. Hash/shape/line
 * problems are collected and reported before the lake run; a hash failure
 * therefore never requires a shadow Lean build.
 */
function runGate(opts) {
  const html = opts.html || HTML;
  const sourcesRoot = opts.sourcesRoot || REPO;
  const lakeRepo = opts.lakeRepo || REPO;
  const work = opts.work;
  const reasons = [];
  let ex;
  try { ex = extract(readFileSync(html, 'utf8')); }
  catch (e) { return { ok: false, reasons: [e.message] }; }

  for (const row of ex.rows) {
    if (row.k === 'NON PROVATO') {
      if (row.d || row.f || row.l) reasons.push(`${row.id}: NON PROVATO con riferimenti`);
    } else if (!row.d || !row.f || !row.l) {
      reasons.push(`${row.id}: riferimenti mancanti`);
    }
  }
  if (JSON.stringify(ex.decls) !== JSON.stringify(ex.cited))
    reasons.push('CHECK_RECEIPT.decls ≠ citazioni estratte — solo-ricevuta: [' +
      ex.decls.filter(d => !ex.cited.includes(d)) + '] solo-manifesto: [' +
      ex.cited.filter(d => !ex.decls.includes(d)) + ']');

  const srcCache = {};
  for (const [f, h] of Object.entries(ex.sources)) {
    let body;
    try { body = readFileSync(join(sourcesRoot, f), 'utf8'); }
    catch (e) { reasons.push(`sorgente illeggibile: ${f}`); continue; }
    srcCache[f] = body.split('\n');
    if (sha256(body) !== h) reasons.push(`hash sorgente divergente: ${f}`);
  }
  for (const row of ex.rows) {
    if (row.k === 'NON PROVATO' || !row.d || !row.f || !row.l) continue;
    if (!srcCache[row.f]) { reasons.push(`${row.id}: sorgente ${row.f} fuori dallo snapshot`); continue; }
    const line = srcCache[row.f][row.l - 1] || '';
    if (!line.includes(row.d))
      reasons.push(`${row.id}: ${row.f}:${row.l} non contiene ${row.d} — «${line.trim().slice(0, 60)}»`);
  }
  if (reasons.length) return { ok: false, reasons };

  // generate the #check driver from the EXTRACTED set and run it via lake
  const driverPath = join(work, 'claim-gate-driver.lean');
  writeFileSync(driverPath, ['import Reactivegas.Invariants', '',
    '-- generated by economics-simulator-claim-gate.mjs from the embedded manifest',
    ...ex.cited.map(d => `#check @${d}`), ''].join('\n'));
  let out;
  try {
    out = execFileSync('nix',
      ['develop', lakeRepo, '-c', 'lake', 'env', 'lean', driverPath],
      { cwd: join(lakeRepo, 'lean'), encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] });
  } catch (e) {
    const all = String(e.stdout || '') + '\n' + String(e.stderr || '');
    const errLines = all.split('\n').filter(l => /error/i.test(l)).slice(0, 4);
    return { ok: false, reasons: ['il driver #check generato fallisce nel lake env: ' +
      (errLines.length ? errLines.join(' | ') : all.slice(-400))] };
  }
  writeFileSync(join(work, 'claim-gate-output.txt'), out);
  const outSha = sha256(out);
  if (outSha !== ex.sha)
    return { ok: false, reasons: [`CHECK_RECEIPT.sha non legato all'output fresco del driver — embedded=${ex.sha.slice(0, 12)}… fresh=${outSha.slice(0, 12)}…`] };
  return { ok: true, rows: ex.rows.length, cited: ex.cited.length, sha: outSha };
}

/* --- selftest: the three mandatory negative axes, then production GREEN --- */

function selftest(work) {
  const doc = readFileSync(HTML, 'utf8');
  const controls = [
    {
      name: 'citazione fasulla',
      // the bogus name is a strict prefix of a real declaration, so it slips
      // through set-equality and file:line substring checks and MUST be
      // caught by the generated driver failing in the lake environment
      expect: /unknownIdentifier|Unknown identifier/,
      run: () => {
        const p = join(work, 'sab-bogus.html');
        writeFileSync(p, doc.replaceAll("'solvent_preserved'", "'solvent_preserve'"));
        return runGate({ html: p, work });
      },
    },
    {
      name: 'sha della ricevuta mutato',
      expect: /CHECK_RECEIPT\.sha non legato/,
      run: () => {
        const ex = extract(doc);
        const p = join(work, 'sab-sha.html');
        const flipped = (ex.sha[0] === '0' ? '1' : '0') + ex.sha.slice(1);
        writeFileSync(p, doc.replace(`sha: '${ex.sha}'`, `sha: '${flipped}'`));
        return runGate({ html: p, work });
      },
    },
    {
      name: 'sorgente Lean toccata',
      expect: /hash sorgente divergente/,
      run: () => {
        // scratch copy of the pinned sources only; the SAME production
        // hash-check path fails, before any lake run is attempted
        const root = join(work, 'srcroot');
        mkdirSync(join(root, 'lean', 'Reactivegas'), { recursive: true });
        const ex = extract(doc);
        for (const f of Object.keys(ex.sources))
          cpSync(join(REPO, f), join(root, f));
        const victim = join(root, 'lean/Reactivegas/Step.lean');
        writeFileSync(victim, readFileSync(victim, 'utf8') + '-- touched\n');
        return runGate({ html: HTML, sourcesRoot: root, work });
      },
    },
  ];
  for (const c of controls) {
    const r = c.run();
    if (r.ok) {
      console.error(`SELFTEST RED: controllo «${c.name}» ACCETTATO dal gate`);
      return 1;
    }
    const text = r.reasons.join('\n');
    if (!c.expect.test(text)) {
      console.error(`SELFTEST RED: «${c.name}» fallito per il motivo sbagliato:\n${text.slice(0, 300)}`);
      return 1;
    }
    console.log(`controllo negativo «${c.name}»: RED come atteso — ${text.split('\n')[0].slice(0, 110)}`);
  }
  const green = runGate({ work });
  if (!green.ok) {
    console.error('SELFTEST RED: il gate di produzione non torna GREEN:\n' + green.reasons.join('\n'));
    return 1;
  }
  console.log(`selftest GREEN: 3 controlli negativi RED per il motivo atteso; produzione GREEN ` +
    `(${green.rows} righe, ${green.cited} citazioni, sha ${green.sha.slice(0, 12)}…)`);
  return 0;
}

/* --- CLI ------------------------------------------------------------------- */

const work = mkdtempSync(join(tmpdir(), 'rg-claim-gate-'));
let code = 1;
try {
  if (process.argv.includes('--selftest')) {
    code = selftest(work);
  } else {
    const r = runGate({ work });
    if (r.ok) {
      console.log(`GREEN: ${r.rows} righe di manifesto, ${r.cited} citazioni verificate nel ` +
        `lake env; file:line risolti; hash sorgenti confermati; ricevuta legata (sha ${r.sha.slice(0, 12)}…)`);
      code = 0;
    } else {
      console.error(`RED: ${r.reasons.length} problemi`);
      r.reasons.forEach(x => console.error(' - ' + x));
      code = 1;
    }
  }
} finally {
  rmSync(work, { recursive: true, force: true });
}
process.exit(code);
