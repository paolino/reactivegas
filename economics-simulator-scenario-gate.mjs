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
 *      non-v1 envelope, an ignored event (seq/steps mismatch), or a
 *      poststate mismatch is RED; a recorded step the production
 *      transition refuses is the sequence's refusal;
 *   4. walks the integrated governance (verifyGovernedIntegrated) and
 *      compares the outcome with the scenario's declared expectation —
 *      scenario 01-permesso-senza-delibera records a permission no bound
 *      closure authorizes and must be REFUSED by the transition, naming the
 *      missing authorization; --selftest imports a core whose grant does not
 *      spend a closure-derived authorization (the pre-#76 model) and proves
 *      the suite then goes RED;
 *   5. executes every declared assertion (an unknown kind, an empty
 *      assertion list, or an uncovered required kind is RED):
 *        no-vote-derived-without-evidence  governed walk ran and matched
 *        no-close-without-positive-permission
 *                                          every close preceded by a grant
 *                                          that spent a positive bound
 *                                          closure (#76) for its collection
 *        no-negative-conto                 every prefix state, L7
 *        comune-donation                   live donate witness: author
 *                                          cassa +v and unique reserved
 *                                          non-member comune +v; member
 *                                          conti unchanged
 *        comune-backdonation               through the integrated root from
 *                                          a funded comune: refused with no
 *                                          closure bound to w; after a bound
 *                                          positive closure every member +w,
 *                                          comune −n*w, casse unchanged,
 *                                          closure spent
 *        backdonate without closed-app     verifyGovernedIntegrated refuses a
 *        evidence                          backdonate step with empty vote
 *                                          evidence (honest NON PROVATO
 *                                          join; no invented vote id);
 *   6. proves both surfaces consume the SAME core: runs
 *      economics-simulator-build.mjs --check (stale or forked inlined copy
 *      is RED) and executes the page's actual script in a vm, comparing its
 *      probe behavior against the imported module;
 *   7. binds caller identity (F-01 class) on both surfaces: for every
 *      AppEvent constructor derived from lean/Reactivegas/Types.lean a
 *      payload author ≠ signer is refused author-mismatch by
 *      applyIntegrated, a recorded step carrying one is refused by
 *      verifyIntegratedV1 on its applied AND its refused branch and by the
 *      governance walk (the same step without it replays), and an equal
 *      author yields exactly the signer's step; --selftest imports a core
 *      with the refusal removed, and one whose refused branch bypasses
 *      applyIntegrated, and requires this check RED on each.
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

/* Teardown of a disposable resource must never determine the gate's verdict:
   scratch-dir cleanup is housekeeping and cannot veto the exit code. */
function rmQuiet(p) {
  try { rmSync(p, { recursive: true, force: true }); } catch { /* housekeeping */ }
}
const HTML = join(REPO, 'economics-simulator.html');
const SCENARIOS = join(REPO, 'economics-simulator-scenarios');
const sha256 = b => createHash('sha256').update(b).digest('hex');

const core = await import(join(REPO, 'economics-simulator-core.mjs'));

const REQUIRED_KINDS = ['no-vote-derived-without-evidence',
  'no-close-without-positive-permission', 'no-negative-conto',
  'comune-donation', 'comune-backdonation'];
const clone = x => JSON.parse(JSON.stringify(x));
const balOf = (m, k) => (m.find(([k2]) => k2 === k) || [null, 0])[1];
const totalOf = m => m.reduce((n, [, v]) => n + v, 0);
const ADMIN = { adminRole: { admin: 'publicAdmin' } };
const APP = { appRole: { name: 'socio' } };
const covMember = (k, admin) => [k, { key: k, email: k + '@toy.example', roles: admin ? [ADMIN] : [APP] }];
const donationView = () => ({ members: [covMember('anna', true), covMember('bruno', true), covMember('carla', false)] });
const donationBase = () => ({
  conti: [['carla', 100]],
  casse: [['anna', 100]],
  collections: [],
  votes: { openQuestions: [], closed: [] },
});
const COMUNE = 'comune';   // Reactivegas.comuneId

/* --- assertion implementations (checks belong to the gate; data to the
       scenario files) ------------------------------------------------------ */

function assertNoNegativeConto(sc, ver) {
  ver.states.forEach((s, i) => {
    for (const [k, v] of (s.payload ? s.payload.conti : s.conti))
      if (k !== 'comune' && v < 0) throw new Error(`stato ${i}: conto ${k} negativo (${v})`);
  });
  return `${ver.states.length} stati, tutti i conti ≥ 0`;
}

/* #76: the permission a close needs is granted only by spending a positive
   closure of a question bound to that collection — read from the verified
   states the replay produced (state i is the input of step i) */
function assertNoCloseWithoutPositive(sc, ver) {
  const wrap = sc.wrap;
  const positiveFor = (agg, c) => (agg.payload.live || []).filter(a =>
    a.target.permission && a.target.permission.c === c && a.verdict === 'positive').length;
  const granted = new Set();
  let closes = 0;
  for (const [i, st] of wrap.steps.entries()) {
    if (st.event.app === undefined) continue;
    const ae = st.event.app;
    if ('grantPermission' in ae) {
      const c = ae.grantPermission.c;
      if (positiveFor(ver.states[i], c) !== positiveFor(ver.states[i + 1], c) + 1)
        throw new Error('grantPermission senza la chiusura positiva legata spesa');
      granted.add(c);
    }
    if ('closePurchase' in ae) {
      closes += 1;
      if (!granted.has(ae.closePurchase.c))
        throw new Error('closePurchase senza permesso da verdetto positivo');
    }
  }
  if (!closes) throw new Error('asserzione vacua: nessuna chiusura nello scenario');
  return closes + ' chiusure, ognuna preceduta dal permesso con verdetto positivo';
}

function assertComuneDonation(mod) {
  if ('comune' in mod.emptyState())
    throw new Error('il comune non può essere un campo State: è un conto riservato in conti');
  const view = donationView();
  const memberKeys = view.members.map(([k]) => k);
  const before = donationBase();
  let r;
  try { r = mod.attempt(view, clone(before), { tag: 'donate', author: 'anna', v: 90 }); }
  catch (e) { throw new Error(`donate: ${e.message}`); }
  if (!r || r.ok !== true) throw new Error('donate: live witness refused');
  const after = r.state;
  const common = after.conti.filter(([k]) => !memberKeys.includes(k));
  if (balOf(after.casse, 'anna') - balOf(before.casse, 'anna') !== 90)
    throw new Error('donate: author cassa delta is not +90');
  if (totalOf(after.conti) - totalOf(before.conti) !== 90)
    throw new Error('donate: total conti delta is not +90');
  if (memberKeys.some(u => balOf(after.conti, u) !== balOf(before.conti, u)))
    throw new Error('donate: changed a member conto');
  if (common.length !== 1 || common[0][0] !== COMUNE || common[0][1] !== 90)
    throw new Error('donate: no unique reserved non-member comune conto at +90');
  if (JSON.stringify(after.collections) !== JSON.stringify(before.collections))
    throw new Error('donate: changed collections');
  return 'donate +90 su cassa autore e conto comune riservato, conti membri invariati';
}

/* #76: a backdonation pays only by spending the positive closure of a
   question bound to its share. Driven through the integrated root from a
   funded comune — never the economic step on TOY_AUTH alone: the same
   backdonate with no such closure must be refused, and with it, it pays. */
function assertComuneBackdonation(mod) {
  const view = donationView();
  const memberKeys = view.members.map(([k]) => k);
  const run = (gs, signer, ae, what) => {
    let det;
    try { det = mod.applyIntegrated(clone(gs), signer, { app: ae }); }
    catch (e) { throw new Error(`backdonate: ${what} threw: ${e.message}`); }
    if (!det) throw new Error(`backdonate: ${what} returned nothing`);
    return det;
  };
  const start = { members: view.members, pendingBase: [],
    appFold: { ...mod.emptyState(), ...donationBase() } };
  const funded = run(start, 'anna', { donate: { v: 90 } }, 'donate setup');
  if (funded.refused) throw new Error('backdonate: donate setup refused');
  if (!run(funded.gs, 'anna', { backdonate: { w: 10 } }, 'backdonate without closure').refused)
    throw new Error('backdonate: applied with no closure bound to its share (TOY_AUTH alone)');
  const target = { backdonation: { w: 10 } };
  const bound = run(funded.gs, 'anna',
    { openBound: { questionId: 'q:quota', kind: 'collective', target } }, 'openBound');
  if (bound.refused) throw new Error('backdonate: bound question refused');
  const closed = run(bound.gs, 'bruno', { cast: { questionId: 'q:quota', ballot: 'assent' } }, 'cast');
  if (closed.refused) throw new Error('backdonate: closing ballot refused');
  const r = run(closed.gs, 'anna', { backdonate: { w: 10 } }, 'backdonate');
  if (r.refused) throw new Error('backdonate: refused with its positive bound closure live');
  const before = closed.gs.appFold, after = r.gs.appFold;
  const common = before.conti.filter(([k]) => !memberKeys.includes(k));
  if (common.length !== 1)
    throw new Error('backdonate: funded comune is not a unique non-member conto');
  for (const u of memberKeys)
    if (balOf(after.conti, u) - balOf(before.conti, u) !== 10)
      throw new Error(`backdonate: member ${u} did not receive exactly +10`);
  if (balOf(after.conti, common[0][0]) - balOf(before.conti, common[0][0]) !== -30)
    throw new Error('backdonate: comune delta is not -(member-count * share)');
  if (JSON.stringify(after.casse) !== JSON.stringify(before.casse))
    throw new Error('backdonate: changed casse');
  if (JSON.stringify(after.collections) !== JSON.stringify(before.collections))
    throw new Error('backdonate: changed collections');
  if ((after.live || []).some(a => 'backdonation' in a.target))
    throw new Error('backdonate: its closure was not spent');
  return 'backdonate +10 a ogni membro, comune −30, casse invariate, spendendo la chiusura ' +
    'legata alla quota (senza: rifiutato)';
}

function assertBackdonateGovernanceBoundary(mod) {
  const empty = { members: [], pendingBase: [],
    payload: { conti: [], casse: [], collections: [], votes: { openQuestions: [], closed: [] } } };
  try {
    mod.verifyGovernedIntegrated({
      schema: 'reactivegas-integrated.trace', version: 1, initial: empty,
      steps: [{ input: empty, signer: 'anna', event: { app: { backdonate: { w: 1 } } },
        result: { tag: 'applied', aggregate: empty } }],
    });
  } catch (e) {
    if (/backdonate|verdetto|governo|rejected|rifiutato/.test(e.message))
      return `rifiuto: ${e.message}`;
    throw new Error(`backdonate governo: motivo sbagliato: ${e.message}`);
  }
  throw new Error('backdonate senza evidenza accettato dal governo');
}

/* --- caller identity (F-01 class): decode, validate, replay ---------------
   An integrated app event never carries its actor: Lean's AppEvent declares
   no author, and the signer travels apart from the event. For EVERY AppEvent
   constructor derived from lean/Reactivegas/Types.lean (never a list here):
   a payload author different from the signer is refused author-mismatch by
   the integrated step (validate); a recorded step carrying one is refused by
   the envelope verifier that the page's import and restore run first (decode,
   replay), while the same step without it replays (control); an author equal
   to the signer is stripped, so the step is exactly the signer's own — never
   substituted. Judged on the imported core module and on the page's own
   production script. */
function appEventConstructors() {
  const src = readFileSync(join(REPO, 'lean', 'Reactivegas', 'Types.lean'), 'utf8');
  const block = src.match(/inductive AppEvent where([\s\S]*?)deriving /);
  if (!block) throw new Error('F-01: AppEvent non trovato in lean/Reactivegas/Types.lean');
  const ctors = [...block[1].matchAll(/^\s*\|\s+([A-Za-z][A-Za-z0-9_]*)\b/gm)].map(m => m[1]);
  if (!ctors.length) throw new Error('F-01: inventario AppEvent vuoto');
  return ctors;
}

function assertCallerIdentity(mod, where) {
  const tags = appEventConstructors();
  const agg = () => ({ members: [covMember('anna', true), covMember('bruno', true)],
    pendingBase: [], appFold: mod.emptyState() });
  const bad = [];
  for (const tag of tags) {
    let det;
    try { det = mod.applyIntegrated(agg(), 'anna', { app: { [tag]: { author: 'bruno' } } }); }
    catch (e) { bad.push(`${tag}: eccezione ${e.message}`); continue; }
    if (!det || det.refused !== 'author-mismatch')
      bad.push(`${tag}: ${det && det.refused ? 'rifiutato ' + det.refused : 'applicato'}`);
  }
  if (bad.length)
    throw new Error(`F-01 ${where}: autore incorporato ≠ firmatario non rifiutato author-mismatch — ${bad.join('; ')}`);
  const honest = mod.applyIntegrated(agg(), 'anna', { app: { donate: { v: 10 } } });
  const equal = mod.applyIntegrated(agg(), 'anna', { app: { donate: { author: 'anna', v: 10 } } });
  if (honest.refused || equal.refused ||
      mod.canonAggregate(honest.gs) !== mod.canonAggregate(equal.gs))
    throw new Error(`F-01 ${where}: un autore uguale al firmatario non dà il passo del firmatario`);
  const boot = mod.bootAggregate();
  const applied = mod.applyIntegrated(boot, 'anna', { app: { donate: { v: 10 } } });
  const step = ev => ({ input: boot, signer: 'anna', event: { app: { donate: ev } },
    result: { tag: 'applied', aggregate: applied.gs } });
  const env = ev => ({ schema: 'reactivegas-integrated.trace', version: 1, initial: boot,
    steps: [step(ev)] });
  try { mod.verifyIntegratedV1(env({ v: 10 })); }
  catch (e) { throw new Error(`F-01 ${where}: controllo senza autore incorporato non replaya: ${e.message}`); }
  // every replay/verify path: the applied branch, the refused branch of the
  // conformance corpus (a recorded refusal cannot excuse a smuggled author),
  // and the governance walk the page's restore and import also run
  const refusedEnv = ev => ({ ...env(ev), steps: [{ ...step(ev),
    result: { tag: 'refused', error: { integrated: { app: { error: 'rejected' } } } } }] });
  const paths = [
    ['replay di un passo applicato', () => mod.verifyIntegratedV1(env({ author: 'bruno', v: 10 }))],
    ['replay di un passo rifiutato', () => mod.verifyIntegratedV1(refusedEnv({ author: 'bruno', v: 10 }),
      { withRefusals: true })],
    ['cammino di governo', () => mod.verifyGovernedIntegrated(env({ author: 'bruno', v: 10 }))],
  ];
  for (const [path, run] of paths) {
    let msg = null;
    try { run(); } catch (e) { msg = e.message; }
    if (!msg || !/author-mismatch/.test(msg))
      throw new Error(`F-01 ${where}: ${path} con autore incorporato ≠ firmatario ${msg ? 'rifiutato per altro: ' + msg : 'accettato'}`);
  }
  return tags.length;
}

/* --- one scenario --------------------------------------------------------- */

function runScenario(sc, mod = core) {
  const notes = [];
  if (!sc || typeof sc !== 'object' || !sc.name || !sc.wrap || !sc.expect)
    throw new Error('forma dello scenario non valida');
  if (!Array.isArray(sc.assertions) || !sc.assertions.length)
    throw new Error('scenario senza asserzioni');
  const wrap = sc.wrap;
  if (!wrap.schema || !Array.isArray(wrap.steps))
    throw new Error('wrap senza lo stream integrato');

  // envelope verification: malformed/non-v1/poststate mismatch → throw. A
  // recorded step the production transition refuses is the refusal of the
  // sequence (#76: an unbacked permission is refused by the root itself);
  // the states before it are the replayed prefix.
  let ver, outcome = 'accepted', reason = null;
  try {
    ver = mod.verifyIntegratedV1(wrap, { appliedOnly: true });
  } catch (e) {
    const m = /^integrato: passo (\d+): registrato applicato, la transizione rifiuta/.exec(e.message);
    if (!m) throw e;
    outcome = 'refused'; reason = e.message;
    ver = mod.verifyIntegratedV1({ ...wrap, steps: wrap.steps.slice(0, Number(m[1])) },
      { appliedOnly: true });
  }

  // governance walk vs declared expectation
  if (outcome === 'accepted') {
    try {
      mod.verifyGovernedIntegrated(wrap);
    } catch (e) { outcome = 'refused'; reason = e.message; }
  }
  if (sc.expect.governed === 'refused') {
    if (outcome !== 'refused')
      throw new Error('atteso rifiuto, ma la sequenza è stata accettata');
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
    else if (kind === 'no-close-without-positive-permission') note = assertNoCloseWithoutPositive(sc, ver);
    else if (kind === 'comune-donation') note = assertComuneDonation(core);
    else if (kind === 'comune-backdonation') note = assertComuneBackdonation(core);
    else if (kind === 'no-vote-derived-without-evidence')
      note = 'coperta dalla transizione e dal cammino di governo qui sopra'; else throw new Error('asserzione sconosciuta: ' + kind);
    notes.push(`${kind}: ${note}`);
  }
  return { steps: wrap.steps.length, notes };
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

  const mod = opts.core || core;
  const covered = new Set();
  let ran = 0, totalSteps = 0;
  const lines = [];
  for (const f of files) {
    let sc;
    try { sc = JSON.parse(readFileSync(join(dir, f), 'utf8')); }
    catch (e) { reasons.push(`${f}: JSON illeggibile`); continue; }
    try {
      const r = runScenario(sc, mod);
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

  if (core.EVENT_ROUTES.donate !== 'direct' || core.voteDerivedTag('donate'))
    reasons.push('donate non è instradato direct');
  if (core.EVENT_ROUTES.backdonate !== 'appDecided' || !core.voteDerivedTag('backdonate'))
    reasons.push('backdonate non è instradato appDecided');
  try {
    assertBackdonateGovernanceBoundary(core);
  } catch (e) {
    reasons.push(e.message);
  }
  let callerTags = 0;
  try { callerTags = assertCallerIdentity(core, 'modulo'); }
  catch (e) { reasons.push(e.message); }

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
      const v0 = { members: [
        ['anna', { key: 'anna', email: 'anna@toy.example', roles: [{ adminRole: { admin: 'publicAdmin' } }] }],
        ['bruno', { key: 'bruno', email: 'bruno@toy.example', roles: [{ appRole: { name: 'socio' } }] }]] };
      const s0 = core.emptyState();
      const e1 = { tag: 'openPurchase', author: 'anna', c: 7 };
      const viaPage = RG.attempt(v0, RG.attempt(v0, s0, e1).state, { tag: 'deposit', author: 'anna', user: 'bruno', v: 7 });
      const viaCore = core.attempt(v0, core.attempt(v0, s0, e1).state, { tag: 'deposit', author: 'anna', user: 'bruno', v: 7 });
      if (core.canonState(viaPage.state) !== core.canonState(viaCore.state))
        throw new Error('la pagina e il modulo divergono sullo stesso evento');
      if (JSON.stringify(RG.EVENT_ROUTES) !== JSON.stringify(core.EVENT_ROUTES))
        throw new Error('instradamento divergente fra pagina e modulo');
      assertCallerIdentity(RG, 'pagina');
    } catch (e) {
      reasons.push('interfaccia condivisa non dimostrata: ' + e.message);
    }
  }

  if (reasons.length) return { ok: false, reasons };
  return { ok: true, scenarios: ran, totalSteps, covered: [...covered].sort(), lines, callerTags };
}

/* --- selftest -------------------------------------------------------------- */

function selftest(work, mutantCore, bypassCore, unspentCore) {
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
      name: 'core il cui permesso non spende una chiusura legata — il modello pre-#76',
      expect: /permesso-senza-delibera\.json: atteso rifiuto, ma la sequenza è stata accettata/,
      run: () => runSuite({ core: unspentCore, skipVm: true }),
    },
    {
      name: 'post-stato mutato in uno scenario',
      expect: /divergente|discontinuo/,
      run: () => {
        const d = sabDir('sab-state');
        const p = join(d, first);
        const sc = JSON.parse(readFileSync(p, 'utf8'));
        sc.wrap.steps[0].result.aggregate.payload.conti.push(['zz', 99]);
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
        sc.wrap.schema = 'x';
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
    {
      name: 'legge di donazione violata (comune non accreditato)',
      expect: /donate: (author cassa delta is not \+90|no unique reserved non-member comune conto at \+90|total conti delta is not \+90)/,
      run: () => {
        try {
          assertComuneDonation({
            emptyState: core.emptyState,
            attempt: (v, s) => ({ ok: true, state: clone(s) }),
          });
          return { ok: true, reasons: [] };
        } catch (e) {
          return { ok: false, reasons: [e.message] };
        }
      },
    },
    {
      name: 'core senza il rifiuto author-mismatch (F-01)',
      expect: /F-01 modulo: autore incorporato ≠ firmatario non rifiutato/,
      run: () => {
        try { assertCallerIdentity(mutantCore, 'modulo'); return { ok: true, reasons: [] }; }
        catch (e) { return { ok: false, reasons: [e.message] }; }
      },
    },
    {
      name: 'ramo dei passi rifiutati che salta il punto di strozzatura (F-01)',
      expect: /F-01 modulo: replay di un passo rifiutato con autore incorporato ≠ firmatario accettato/,
      run: () => {
        try { assertCallerIdentity(bypassCore, 'modulo'); return { ok: true, reasons: [] }; }
        catch (e) { return { ok: false, reasons: [e.message] }; }
      },
    },
    {
      name: 'redistribuzione applicata sul solo TOY_AUTH (core senza la spesa)',
      expect: /backdonate: applied with no closure bound to its share/,
      run: () => {
        try { assertComuneBackdonation(unspentCore); return { ok: true, reasons: [] }; }
        catch (e) { return { ok: false, reasons: [e.message] }; }
      },
    },
    {
      name: 'backdonate senza evidenza accettato dal governo',
      expect: /backdonate senza evidenza accettato dal governo/,
      run: () => {
        try {
          assertBackdonateGovernanceBoundary({ verifyGovernedIntegrated() {} });
          return { ok: true, reasons: [] };
        } catch (e) {
          return { ok: false, reasons: [e.message] };
        }
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
    const source = readFileSync(join(REPO, 'economics-simulator-core.mjs'), 'utf8');
    const needle = "    if (smuggled !== undefined && smuggled !== signer)\n      return { refused: 'author-mismatch' };\n";
    if (source.split(needle).length !== 2)
      throw new Error('selftest: il mutante F-01 non si applica esattamente una volta');
    const mutantPath = join(work, 'economics-simulator-core-f01-mutant.mjs');
    writeFileSync(mutantPath, source.replace(needle, ''));
    const bypassNeedle = "      const det = applyIntegrated(gs, st.signer, st.event);\n      if (det.refused === 'author-mismatch')";
    if (source.split(bypassNeedle).length !== 2)
      throw new Error('selftest: il mutante di aggiramento F-01 non si applica esattamente una volta');
    const bypassPath = join(work, 'economics-simulator-core-f01-bypass.mjs');
    writeFileSync(bypassPath, source.replace(bypassNeedle,
      "      const det = { refused: 'rejected' };\n      if (det.refused === 'author-mismatch')"));
    const unspentNeedle = '    if (pulled === null) return { ok: false, failed: [NOLIVE(target, verdict)] };\n';
    if (source.split(unspentNeedle).length !== 2)
      throw new Error('selftest: il mutante pre-#76 non si applica esattamente una volta');
    const unspentPath = join(work, 'economics-simulator-core-unspent.mjs');
    writeFileSync(unspentPath, source.replace(unspentNeedle,
      '    if (pulled === null) return effect(s);\n'));
    code = selftest(work, await import(mutantPath), await import(bypassPath),
      await import(unspentPath));
  } else {
    const r = runSuite({});
    if (r.ok) {
      console.log(`GREEN: ${r.scenarios} scenari, ${r.totalSteps} passi replayati sul core ` +
        `condiviso; asserzioni coperte: ${r.covered.join(', ')}; pagina generata identica al ` +
        `core e stessa interfaccia dimostrata; F-01 identità del chiamante su ${r.callerTags} ` +
        `costruttori AppEvent derivati (modulo e pagina)`);
      r.lines.forEach(l => console.log(l));
      code = 0;
    } else {
      console.error(`RED: ${r.reasons.length} problemi`);
      r.reasons.forEach(x => console.error(' - ' + x));
      code = 1;
    }
  }
} finally {
  rmQuiet(work);
}
process.exit(code);
