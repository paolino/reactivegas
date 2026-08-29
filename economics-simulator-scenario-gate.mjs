#!/usr/bin/env node
/*
 * economics-simulator-scenario-gate.mjs — executable scenario suite over the
 * ONE machine core (economics-simulator-core.mjs), plus the shared-core
 * drift gate for the generated single-file page.
 *
 * A scenario is a frozen `reactivegas.trace` v1 envelope (with its optional
 * vote and base-channel streams and the combined seq) PLUS declarative
 * assertions — never a new trace format. Fresh on every run the gate:
 *
 *   1. imports the core MODULE directly (the same file whose slices are
 *      inlined byte-for-byte into the page — step 6 proves that);
 *   2. loads every scenario in economics-simulator-scenarios/ — an empty or
 *      all-skipped suite is RED;
 *   3. replays every envelope through the core verifiers: a malformed or
 *      non-v1 envelope, an ignored event (seq/steps mismatch), a refused
 *      step in an applied-only stream, or a poststate mismatch is RED;
 *   4. walks combined-seq governance (verifyGovernedSeq) and compares the
 *      outcome with the scenario's declared expectation — scenario
 *      01-elezioni-senza-delibera is the operator's exact unmarked-election
 *      sequence and must be REFUSED; the RG_SCENARIO_GOVERNANCE=off hook
 *      reintroduces the pre-fix model exactly, and --selftest proves the
 *      suite then goes RED (the assertion detects the pre-fix behavior);
 *   5. executes every declared assertion (an unknown kind, an empty
 *      assertion list, or an uncovered required kind is RED):
 *        no-vote-derived-without-evidence  governed walk ran and matched
 *        no-close-without-positive-permission
 *        no-negative-conto                 every prefix state, L7
 *        comune-tripwire                   this snapshot has no comune and
 *                                          no donate/backdonate execution;
 *                                          the day the core gains them this
 *                                          assertion REDs, forcing a
 *                                          conscious upgrade — never a
 *                                          silent vacuous pass;
 *   6. proves both surfaces consume the SAME core: runs
 *      economics-simulator-build.mjs --check (stale or forked inlined copy
 *      is RED) and executes the page's actual script in a vm, comparing its
 *      probe behavior against the imported module.
 *
 * Usage:
 *   node economics-simulator-scenario-gate.mjs             # production
 *   node economics-simulator-scenario-gate.mjs --selftest  # negative controls
 */

import { readFileSync, writeFileSync, readdirSync, mkdtempSync, mkdirSync,
  rmSync, cpSync } from 'node:fs';
import { createHash } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { tmpdir } from 'node:os';
import vm from 'node:vm';

const REPO = dirname(fileURLToPath(import.meta.url));
const HTML = join(REPO, 'economics-simulator.html');
const SCENARIOS = join(REPO, 'economics-simulator-scenarios');
const sha256 = b => createHash('sha256').update(b).digest('hex');

const core = await import(join(REPO, 'economics-simulator-core.mjs'));

const REQUIRED_KINDS = ['no-vote-derived-without-evidence',
  'no-close-without-positive-permission', 'no-negative-conto', 'comune-tripwire'];

/* --- assertion implementations (checks belong to the gate; data to the
       scenario files) ------------------------------------------------------ */

function assertNoNegativeConto(sc, ver) {
  ver.states.forEach((s, i) => {
    for (const [u, v] of s.conti)
      if (v < 0) throw new Error(`stato ${i}: conto ${u} negativo (${v})`);
  });
  return `${ver.states.length} stati, tutti i conti ≥ 0`;
}

function assertNoCloseWithoutPositive(sc) {
  const wrap = sc.wrap;
  let kgs = core.vtEmpty();
  const granted = new Set();
  let closes = 0;
  let ei = 0, ki = 0;
  for (const m of wrap.seq) {
    if (m === 'k') {
      const st = wrap.kel.steps[ki++];
      kgs = core.vtApply(kgs, st.signer, st.event).state;
    } else if (m === 'e') {
      const e = core.leanEventOf(wrap.trace.steps[ei++].event);
      if (e.tag === 'grantPermission') {
        const rec = kgs.closed.find(r => r.questionId === core.permQid(e.c));
        if (!rec || rec.verdict !== 'positive')
          throw new Error(`grantPermission su «${e.c}» senza verdetto positivo chiuso`);
        granted.add(e.c);
      }
      if (e.tag === 'closePurchase') {
        closes += 1;
        if (!granted.has(e.c))
          throw new Error(`closePurchase su «${e.c}» senza permesso da verdetto positivo`);
      }
    }
  }
  if (!closes) throw new Error('asserzione vacua: nessuna chiusura nello scenario');
  return `${closes} chiusure, ognuna preceduta dal permesso con verdetto positivo`;
}

function assertComuneTripwire(sc, ver) {
  // the tripwire watches the LIVE core, not just the frozen envelope: the
  // day the machine gains the comune (or executes donate/backdonate) this
  // assertion refuses to stay absence-based
  if ('comune' in core.initState(0))
    throw new Error('il core ha guadagnato il comune: aggiorna questa asserzione a una legge di conservazione (donazione, assorbimento di uscita, backdonation)');
  ver.states.forEach((s, i) => {
    if ('comune' in s) throw new Error(`stato ${i} con campo comune inatteso`);
  });
  for (const st of sc.wrap.trace.steps) {
    const e = core.leanEventOf(st.event);
    if (e.tag === 'donate' || e.tag === 'backdonate')
      throw new Error(`evento ${e.tag} eseguito ma l'asserzione comune è ancora assenza-based`);
  }
  return 'nessun comune in questo snapshot (tripwire vivo sul core)';
}

/* --- one scenario --------------------------------------------------------- */

function runScenario(sc) {
  const notes = [];
  if (!sc || typeof sc !== 'object' || !sc.name || !sc.wrap || !sc.expect)
    throw new Error('forma dello scenario non valida');
  if (!Array.isArray(sc.assertions) || !sc.assertions.length)
    throw new Error('scenario senza asserzioni');
  const wrap = sc.wrap;
  if (!wrap.trace || !wrap.kel || !wrap.base || typeof wrap.seq !== 'string')
    throw new Error('wrap senza i tre flussi + seq');

  // envelope verification: malformed/non-v1/poststate mismatch/refused → throw
  const ver = core.verifyTraceV1(wrap.trace, { appliedOnly: true });
  core.verifyKelTraceV1(wrap.kel, { appliedOnly: true });
  core.verifyBaseTraceV1(wrap.base, { appliedOnly: true });

  // ignored events: every step of every stream is walked exactly once
  const eC = [...wrap.seq].filter(m => m === 'e').length;
  const kC = [...wrap.seq].filter(m => m === 'k').length;
  const bC = [...wrap.seq].filter(m => m === 'b').length;
  if (eC !== wrap.trace.steps.length || kC !== wrap.kel.steps.length ||
      bC !== wrap.base.steps.length || eC + kC + bC !== wrap.seq.length)
    throw new Error('seq incoerente con i flussi: eventi ignorati o inventati');

  // governance walk vs declared expectation. RG_SCENARIO_GOVERNANCE=off is
  // the controlled reintroduction of the pre-fix model (no governance):
  // under it, a scenario expecting refusal MUST fail — see --selftest.
  const governanceOff = process.env.RG_SCENARIO_GOVERNANCE === 'off';
  let outcome = 'accepted', reason = null;
  if (!governanceOff) {
    try {
      core.verifyGovernedSeq({ env: wrap.trace, kelEnv: wrap.kel,
        baseEnv: wrap.base, seq: [...wrap.seq] });
    } catch (e) { outcome = 'refused'; reason = e.message; }
  }
  if (sc.expect.governed === 'refused') {
    if (outcome !== 'refused')
      throw new Error('atteso rifiuto del governo, ma la sequenza è stata accettata' +
        (governanceOff ? ' (governo disattivato: comportamento pre-fix reintrodotto)' : ''));
    if (sc.expect.refusalMatch && !reason.includes(sc.expect.refusalMatch))
      throw new Error(`rifiuto per il motivo sbagliato: ${reason}`);
    notes.push(`governo: rifiutata come atteso — ${reason}`);
  } else if (sc.expect.governed === 'accepted') {
    if (outcome !== 'accepted')
      throw new Error(`governo: rifiuto inatteso — ${reason}`);
    notes.push('governo: accettata come atteso');
  } else throw new Error('expect.governed sconosciuto: ' + sc.expect.governed);

  for (const kind of sc.assertions) {
    let note;
    if (kind === 'no-negative-conto') note = assertNoNegativeConto(sc, ver);
    else if (kind === 'no-close-without-positive-permission') note = assertNoCloseWithoutPositive(sc);
    else if (kind === 'comune-tripwire') note = assertComuneTripwire(sc, ver);
    else if (kind === 'no-vote-derived-without-evidence') {
      if (governanceOff)
        throw new Error('asserzione di governo richiesta ma il governo è disattivato');
      note = 'coperta dal cammino di governo qui sopra';
    } else throw new Error('asserzione sconosciuta: ' + kind);
    notes.push(`${kind}: ${note}`);
  }
  return { steps: wrap.seq.length, notes };
}

/* --- the suite + the shared-core gate -------------------------------------- */

function runSuite(opts) {
  const dir = opts.dir || SCENARIOS;
  const htmlPath = opts.html || HTML;
  const reasons = [];
  let files = [];
  try { files = readdirSync(dir).filter(f => f.endsWith('.json')).sort(); }
  catch (e) { return { ok: false, reasons: ['cartella scenari illeggibile: ' + dir] }; }
  if (!files.length) return { ok: false, reasons: ['suite vuota: nessuno scenario in ' + dir] };

  const covered = new Set();
  let ran = 0, totalSteps = 0;
  const lines = [];
  for (const f of files) {
    let sc;
    try { sc = JSON.parse(readFileSync(join(dir, f), 'utf8')); }
    catch (e) { reasons.push(`${f}: JSON illeggibile`); continue; }
    try {
      const r = runScenario(sc);
      ran += 1; totalSteps += r.steps;
      (sc.assertions || []).forEach(k => covered.add(k));
      lines.push(`  ${f}: ${r.steps} passi — ${r.notes.join(' · ')}`);
    } catch (e) {
      reasons.push(`${f}: ${e.message}`);
    }
  }
  if (!ran && !reasons.length) reasons.push('suite senza scenari eseguiti');
  for (const k of REQUIRED_KINDS)
    if (!covered.has(k)) reasons.push('genere di asserzione richiesto non coperto dalla suite: ' + k);

  // shared-core drift: the generated page must be byte-identical to the core
  try {
    execFileSync(process.execPath, [join(REPO, 'economics-simulator-build.mjs'),
      '--check', '--html', htmlPath], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] });
  } catch (e) {
    reasons.push('deriva del core condiviso: ' +
      (String(e.stderr || e.stdout || '')).trim().split('\n')[0]);
  }

  // the page demonstrably invokes the same core interface: execute its
  // actual script and compare probe behavior with the imported module
  if (!opts.skipVm) {
    try {
      const doc = readFileSync(htmlPath, 'utf8');
      const sm = doc.match(/<script>\n([\s\S]*?)\n<\/script>/);
      if (!sm) throw new Error('script di produzione non trovato');
      const stubHandler = {
        get(t, p) {
          if (p === Symbol.toPrimitive) return () => '';
          if (p === Symbol.iterator) return function* () {};
          if (p === 'hidden' || p === 'disabled') return false;
          return STUB;
        },
        set() { return true; }, apply() { return STUB; }, construct() { return STUB; },
      };
      const STUB = new Proxy(function () {}, stubHandler);
      const ctx = { location: { search: '' }, document: STUB, navigator: STUB,
        innerWidth: 1280, innerHeight: 800, performance: { now: () => 0 },
        requestAnimationFrame: () => 0, setTimeout: () => 0, clearTimeout: () => 0, console };
      ctx.window = ctx; ctx.globalThis = ctx;
      vm.createContext(ctx);
      vm.runInContext(sm[1], ctx, { filename: 'economics-simulator.html#script' });
      const RG = ctx.window.RG;
      if (!RG || typeof RG.attempt !== 'function')
        throw new Error('la pagina non espone il core');
      const probe = { tag: 'deposit', author: 0, user: 1, v: 7 };
      const s0 = core.initState(0);
      const viaPage = RG.attempt(core.attempt(s0, { tag: 'addUser', author: 0, target: 1 }).state, probe);
      const viaCore = core.attempt(core.attempt(s0, { tag: 'addUser', author: 0, target: 1 }).state, probe);
      if (core.canonState(viaPage.state) !== core.canonState(viaCore.state))
        throw new Error('la pagina e il modulo divergono sullo stesso evento');
      if (JSON.stringify(RG.EVENT_ROUTES) !== JSON.stringify(core.EVENT_ROUTES))
        throw new Error('instradamento divergente fra pagina e modulo');
    } catch (e) {
      reasons.push('interfaccia condivisa non dimostrata: ' + e.message);
    }
  }

  if (reasons.length) return { ok: false, reasons };
  return { ok: true, scenarios: ran, totalSteps, covered: [...covered].sort(), lines };
}

/* --- selftest -------------------------------------------------------------- */

function selftest(work) {
  const sabDir = tag => {
    const d = join(work, tag);
    mkdirSync(d, { recursive: true });
    for (const f of readdirSync(SCENARIOS).filter(x => x.endsWith('.json')))
      cpSync(join(SCENARIOS, f), join(d, f));
    return d;
  };
  const first = readdirSync(SCENARIOS).filter(f => f.endsWith('.json')).sort()[0];
  const controls = [
    {
      name: 'governo disattivato — reintroduzione esatta del comportamento pre-fix',
      expect: /atteso rifiuto del governo.*pre-fix/,
      run: () => {
        process.env.RG_SCENARIO_GOVERNANCE = 'off';
        try { return runSuite({}); }
        finally { delete process.env.RG_SCENARIO_GOVERNANCE; }
      },
    },
    {
      name: 'post-stato mutato in uno scenario',
      expect: /divergente|discontinuo/,
      run: () => {
        const d = sabDir('sab-state');
        const p = join(d, first);
        const sc = JSON.parse(readFileSync(p, 'utf8'));
        sc.wrap.trace.steps[0].result.state.users.push(99);
        writeFileSync(p, JSON.stringify(sc));
        return runSuite({ dir: d, skipVm: true });
      },
    },
    {
      name: 'suite vuota',
      expect: /suite vuota/,
      run: () => {
        const d = join(work, 'sab-empty');
        mkdirSync(d, { recursive: true });
        return runSuite({ dir: d, skipVm: true });
      },
    },
    {
      name: 'asserzioni omesse',
      expect: /senza asserzioni/,
      run: () => {
        const d = sabDir('sab-noassert');
        const p = join(d, first);
        const sc = JSON.parse(readFileSync(p, 'utf8'));
        sc.assertions = [];
        writeFileSync(p, JSON.stringify(sc));
        return runSuite({ dir: d, skipVm: true });
      },
    },
    {
      name: 'envelope non-v1',
      expect: /schema sconosciuto/,
      run: () => {
        const d = sabDir('sab-schema');
        const p = join(d, first);
        const sc = JSON.parse(readFileSync(p, 'utf8'));
        sc.wrap.trace.schema = 'x';
        writeFileSync(p, JSON.stringify(sc));
        return runSuite({ dir: d, skipVm: true });
      },
    },
    {
      name: 'evento ignorato (seq tronca)',
      expect: /seq incoerente/,
      run: () => {
        const d = sabDir('sab-seq');
        const p = join(d, first);
        const sc = JSON.parse(readFileSync(p, 'utf8'));
        sc.wrap.seq = sc.wrap.seq.slice(0, -1);
        writeFileSync(p, JSON.stringify(sc));
        return runSuite({ dir: d, skipVm: true });
      },
    },
    {
      name: 'artefatto generato stantio/biforcato',
      expect: /deriva del core condiviso/,
      run: () => {
        const doc = readFileSync(HTML, 'utf8');
        const m = doc.match(/\/\* @@CORE:machine@@ \*\/\n/);
        if (!m) return { ok: false, reasons: ['controllo mal costruito: fetta machine assente'] };
        const p = join(work, 'sab-fork.html');
        writeFileSync(p, doc.replace(m[0], m[0] + '// forked transcription\n'));
        return runSuite({ html: p, skipVm: true });
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
    console.log(`controllo negativo «${c.name}»: RED come atteso — ${text.split('\n')[0].slice(0, 120)}`);
  }
  const green = runSuite({});
  if (!green.ok) {
    console.error('SELFTEST RED: la suite di produzione non torna GREEN:\n' + green.reasons.join('\n'));
    return 1;
  }
  console.log(`selftest GREEN: ${controls.length} controlli negativi RED per il motivo atteso; ` +
    `produzione GREEN (${green.scenarios} scenari, ${green.totalSteps} passi)`);
  return 0;
}

/* --- CLI ------------------------------------------------------------------- */

const work = mkdtempSync(join(tmpdir(), 'rg-scenario-gate-'));
let code = 1;
try {
  if (process.argv.includes('--selftest')) {
    code = selftest(work);
  } else {
    const r = runSuite({});
    if (r.ok) {
      console.log(`GREEN: ${r.scenarios} scenari, ${r.totalSteps} passi replayati sul core ` +
        `condiviso; asserzioni coperte: ${r.covered.join(', ')}; pagina generata identica al ` +
        `core e stessa interfaccia dimostrata`);
      r.lines.forEach(l => console.log(l));
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
