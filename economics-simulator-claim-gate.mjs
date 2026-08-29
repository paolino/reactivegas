#!/usr/bin/env node
/*
 * economics-simulator-claim-gate.mjs — the executable verifier of the claim
 * manifest embedded in economics-simulator.html.
 *
 * The simulator's CHECK_RECEIPT asserts that every Lean declaration cited by
 * its CLAIMS manifest elaborates in the repository's Lean sources, and that
 * every citation carries a DERIVED proof state. This gate is what performs
 * that verification. Fresh on every run it:
 *
 *   1. reads the committed economics-simulator.html;
 *   2. extracts every theorem/definition citation directly from the embedded
 *      CLAIMS manifest — never from a copied citation list;
 *   3. validates row shape (including NON PROVATO rows carrying no refs);
 *   4. verifies every cited file:line and every pinned source sha256 against
 *      the repository Lean files;
 *   5. generates a temporary Lean driver with `#check` AND `#print axioms`
 *      for the extracted distinct declaration set;
 *   6. runs it in the repository's actual lean/ lake environment;
 *   7. classifies every citation from the fresh axiom report — `provato`
 *      (no sorryAx) or `enunciato` (depends on sorryAx: stated, not proved) —
 *      and requires CHECK_RECEIPT.axioms to equal that fresh derivation;
 *      a citation the report cannot classify is RED, never assumed proved;
 *   8. hashes the fresh driver output and compares it to CHECK_RECEIPT.sha;
 *   9. requires CHECK_RECEIPT.decls to equal the extracted citation set;
 *  10. exits nonzero with a precise reason on any mismatch, zero on GREEN.
 *
 * Together with the page's rendering (which draws the three user-facing
 * states provato / enunciato, non dimostrato / NON PROVATO exclusively from
 * CHECK_RECEIPT.axioms) this makes it impossible for a sorry-backed citation
 * to render as proved while the gate is GREEN.
 *
 * Usage, from a clean checkout (any working directory):
 *   node economics-simulator-claim-gate.mjs                 # gate run
 *   node economics-simulator-claim-gate.mjs --selftest      # negative controls
 *   node economics-simulator-claim-gate.mjs --emit-receipt  # print the fresh
 *       sha and derived axioms map (for updating the embedded receipt after
 *       an intentional manifest change; the gate stays RED until they match)
 *
 * --selftest proves the gate can fail on every mandatory axis — bogus
 * economic citation, bogus vote citation, mutated receipt sha, touched
 * economic source, touched Vote source, a freshly-DERIVED sorry-backed
 * declaration flipped to provato in the receipt, and a disabled sorry
 * detector (env hook RG_GATE_SORRY_DETECTOR=off, caught by the always-on
 * tripwire) — each for its intended reason, then runs the unmodified
 * production gate GREEN. Temporary artifacts live in a fresh mkdtemp
 * directory; the repository stays clean.
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
  const rm = doc.match(/const CHECK_RECEIPT = \{[\s\S]*?sha: '([0-9a-f]{64})',[\s\S]*?decls: \[([\s\S]*?)\],[\s\S]*?axioms: \{([\s\S]*?)\},[\s\S]*?sources: \{([\s\S]*?)\},\n\};/);
  if (!rm) throw new Error('CHECK_RECEIPT non trovato nel documento');
  return {
    rows,
    cited: [...new Set(rows.filter(x => x.d).map(x => x.d))].sort(),
    sha: rm[1],
    decls: [...rm[2].matchAll(/'([A-Za-z_.]+)'/g)].map(x => x[1]).sort(),
    axioms: Object.fromEntries(
      [...rm[3].matchAll(/'([A-Za-z_.]+)':\s*'(provato|enunciato)'/g)].map(x => [x[1], x[2]])),
    sources: Object.fromEntries(
      [...rm[4].matchAll(/'([^']+)':\s*'([0-9a-f]{64})'/g)].map(x => [x[1], x[2]])),
  };
}

/*
 * Derive the proof state of every cited declaration from the fresh driver
 * output. `#print axioms D` prints exactly one of
 *   'D' does not depend on any axioms
 *   'D' depends on axioms: [a, b, ...]
 * The parse is name-anchored: a declaration whose report line is missing is
 * left unclassified (RED downstream), never defaulted. The env hook
 * RG_GATE_SORRY_DETECTOR=off simulates a broken sorry detector for the
 * selftest; the tripwire in runGate catches it mechanically.
 */
function deriveAxioms(cited, out) {
  const detectorOff = process.env.RG_GATE_SORRY_DETECTOR === 'off';
  const derived = {};
  const rawLine = {};
  for (const d of cited) {
    const esc = d.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
    const m = out.match(new RegExp(
      `'${esc}' (does not depend on any axioms|depends on axioms:[^\\n]*)`));
    if (!m) continue;
    rawLine[d] = m[0];
    const sorried = detectorOff ? false : m[1].includes('sorryAx');
    derived[d] = sorried ? 'enunciato' : 'provato';
  }
  return { derived, rawLine };
}

/*
 * Run the gate. opts:
 *   html        path to the artifact (default: committed HTML)
 *   sourcesRoot root for reading the pinned Lean sources (default: repo);
 *               the selftest's touched-source controls override this so the
 *               SAME production hash-check path fails, before any lake run
 *   lakeRepo    repo whose lean/ lake environment runs the driver
 *   work        scratch dir for the generated driver and its output
 *   emit        skip the sha/axioms comparison and return the fresh values
 * Returns { ok: true, axioms } or { ok: false, reasons: [...] }. Hash/shape/
 * line problems are collected and reported before the lake run; a hash
 * failure therefore never requires a shadow Lean build.
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
    // namespaced citations appear unqualified at their declaration site
    const localName = row.d.split('.').pop();
    if (!line.includes(localName))
      reasons.push(`${row.id}: ${row.f}:${row.l} non contiene ${localName} — «${line.trim().slice(0, 60)}»`);
  }
  if (reasons.length) return { ok: false, reasons };

  // generate the audit driver from the EXTRACTED set and run it via lake:
  // #check proves the citation elaborates, #print axioms yields the material
  // for the derived three-state classification
  const driverPath = join(work, 'claim-gate-driver.lean');
  writeFileSync(driverPath, [
    'import Reactivegas.Invariants',
    'import KelGroups.Invariants',
    'import KelGroups.Validate',
    'import KelGroups.Vote.Invariants',
    'import KelGroups.Vote.Validate',
    '',
    '-- generated by economics-simulator-claim-gate.mjs from the embedded manifest',
    ...ex.cited.flatMap(d => [`#check @${d}`, `#print axioms ${d}`]), ''].join('\n'));
  let out;
  try {
    out = execFileSync('nix',
      ['develop', lakeRepo, '-c', 'lake', 'env', 'lean', driverPath],
      { cwd: join(lakeRepo, 'lean'), encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] });
  } catch (e) {
    const all = String(e.stdout || '') + '\n' + String(e.stderr || '');
    const errLines = all.split('\n').filter(l => /error/i.test(l)).slice(0, 4);
    return { ok: false, reasons: ['il driver di audit generato fallisce nel lake env: ' +
      (errLines.length ? errLines.join(' | ') : all.slice(-400))] };
  }
  writeFileSync(join(work, 'claim-gate-output.txt'), out);

  // DERIVED three-state classification — never hand-assigned, never defaulted
  const { derived, rawLine } = deriveAxioms(ex.cited, out);
  for (const d of ex.cited) {
    if (!derived[d]) {
      reasons.push(`stato assiomi non classificabile per ${d} — il report fresco non lo nomina`);
      continue;
    }
    // always-on tripwire: a declaration classified provato whose fresh axiom
    // report mentions sorryAx means the sorry detector is disabled or broken
    if (derived[d] === 'provato' && rawLine[d].includes('sorryAx'))
      reasons.push(`rilevatore sorryAx disattivato o guasto: ${d} classificato provato con sorryAx nel report`);
  }
  if (reasons.length) return { ok: false, reasons };

  const outSha = sha256(out);
  if (opts.emit) return { ok: true, rows: ex.rows.length, cited: ex.cited.length,
    sha: outSha, axioms: derived };
  if (JSON.stringify(ex.axioms) !== JSON.stringify(derived)) {
    const diffs = [...new Set([...Object.keys(ex.axioms), ...Object.keys(derived)])]
      .filter(d => ex.axioms[d] !== derived[d])
      .map(d => `${d}: embedded=${ex.axioms[d] || 'assente'} derivato=${derived[d] || 'assente'}`);
    return { ok: false, reasons: ['CHECK_RECEIPT.axioms ≠ derivazione fresca — ' +
      diffs.slice(0, 6).join('; ')] };
  }
  if (outSha !== ex.sha)
    return { ok: false, reasons: [`CHECK_RECEIPT.sha non legato all'output fresco del driver — embedded=${ex.sha.slice(0, 12)}… fresh=${outSha.slice(0, 12)}…`] };
  const enun = Object.values(derived).filter(s => s === 'enunciato').length;
  return { ok: true, rows: ex.rows.length, cited: ex.cited.length, sha: outSha,
    axioms: derived, enun };
}

/* --- selftest: the mandatory negative axes, then production GREEN ---------- */

function selftest(work) {
  const doc = readFileSync(HTML, 'utf8');

  // run production FIRST: GREEN is required, and its fresh derivation is the
  // material for the derived-flip control (nothing hardcoded)
  const green = runGate({ work });
  if (!green.ok) {
    console.error('SELFTEST RED: il gate di produzione non torna GREEN:\n' + green.reasons.join('\n'));
    return 1;
  }
  const sorried = Object.entries(green.axioms).filter(([, s]) => s === 'enunciato').map(([d]) => d);
  if (!sorried.length) {
    console.error('SELFTEST RED: nessuna dichiarazione enunciata derivata — il controllo del ' +
      'flip non ha materiale (atteso: le invarianti del voto portano sorry)');
    return 1;
  }
  const flipTarget = sorried[0];
  console.log(`derivazione fresca: ${sorried.length} dichiarazioni enunciate; ` +
    `controllo flip su ${flipTarget}`);

  const controls = [
    {
      name: 'citazione economica fasulla',
      // strict prefix of a real declaration: slips through set-equality and
      // file:line substring checks, MUST die in the lake elaboration
      expect: /unknownIdentifier|[Uu]nknown identifier|[Uu]nknown constant/,
      run: () => {
        const p = join(work, 'sab-bogus.html');
        writeFileSync(p, doc.replaceAll("'solvent_preserved'", "'solvent_preserve'"));
        return runGate({ html: p, work });
      },
    },
    {
      name: 'citazione del voto fasulla',
      expect: /unknownIdentifier|[Uu]nknown identifier|[Uu]nknown constant/,
      run: () => {
        const p = join(work, 'sab-bogus-vote.html');
        writeFileSync(p, doc.replaceAll("'KelGroups.Vote.foldVote_wellFormed'",
          "'KelGroups.Vote.foldVote_wellForme'"));
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
      name: 'dichiarazione con sorry flippata a provato',
      // the target was DERIVED enunciato moments ago by the production run;
      // flipping only the embedded receipt must diverge from the fresh
      // derivation and go RED before the sha comparison can mask anything
      expect: /CHECK_RECEIPT\.axioms ≠ derivazione fresca/,
      run: () => {
        const p = join(work, 'sab-flip.html');
        const before = `'${flipTarget}': 'enunciato'`;
        if (!doc.includes(before)) return { ok: false,
          reasons: [`controllo mal costruito: ${before} assente dal documento`] };
        writeFileSync(p, doc.replace(before, `'${flipTarget}': 'provato'`));
        return runGate({ html: p, work });
      },
    },
    {
      name: 'rilevatore sorry disattivato (hook di test)',
      expect: /rilevatore sorryAx disattivato o guasto/,
      run: () => {
        process.env.RG_GATE_SORRY_DETECTOR = 'off';
        try { return runGate({ work }); }
        finally { delete process.env.RG_GATE_SORRY_DETECTOR; }
      },
    },
    {
      name: 'sorgente Lean economica toccata',
      expect: /hash sorgente divergente: lean\/Reactivegas\/Step\.lean/,
      run: () => touchedSourceControl(doc, work, 'lean/Reactivegas/Step.lean', 'srcroot-econ'),
    },
    {
      name: 'sorgente Lean del voto toccata',
      expect: /hash sorgente divergente: lean\/KelGroups\/Vote\/Invariants\.lean/,
      run: () => touchedSourceControl(doc, work, 'lean/KelGroups/Vote/Invariants.lean', 'srcroot-vote'),
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
  console.log(`selftest GREEN: ${controls.length} controlli negativi RED per il motivo atteso; ` +
    `produzione GREEN (${green.rows} righe, ${green.cited} citazioni, ` +
    `${green.enun} enunciate, sha ${green.sha.slice(0, 12)}…)`);
  return 0;
}

/* scratch copy of the pinned sources only; the SAME production hash-check
   path fails on the touched file, before any lake run is attempted */
function touchedSourceControl(doc, work, victimRel, tag) {
  const root = join(work, tag);
  const ex = extract(doc);
  for (const f of Object.keys(ex.sources)) {
    mkdirSync(dirname(join(root, f)), { recursive: true });
    cpSync(join(REPO, f), join(root, f));
  }
  const victim = join(root, victimRel);
  writeFileSync(victim, readFileSync(victim, 'utf8') + '-- touched\n');
  return runGate({ html: HTML, sourcesRoot: root, work });
}

/* --- CLI ------------------------------------------------------------------- */

const work = mkdtempSync(join(tmpdir(), 'rg-claim-gate-'));
let code = 1;
try {
  if (process.argv.includes('--selftest')) {
    code = selftest(work);
  } else if (process.argv.includes('--emit-receipt')) {
    const r = runGate({ work, emit: true });
    if (r.ok) {
      console.log(JSON.stringify({ sha: r.sha, axioms: r.axioms }, null, 2));
      code = 0;
    } else {
      console.error('RED (emit): ' + r.reasons.join('\n - '));
      code = 1;
    }
  } else {
    const r = runGate({ work });
    if (r.ok) {
      console.log(`GREEN: ${r.rows} righe di manifesto, ${r.cited} citazioni verificate nel ` +
        `lake env; stati derivati (${r.enun} enunciate, ${r.cited - r.enun} provate); ` +
        `file:line risolti; hash sorgenti confermati; ricevuta legata (sha ${r.sha.slice(0, 12)}…)`);
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
