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
 *      the repository Lean files — and requires the receipt's KelGroups pin
 *      set to exhaustively and exactly equal the authoritative source set
 *      discovered FRESH on every run as the TRACKED files (git ls-files) for
 *      lean/KelGroups.lean plus every *.lean recursively under
 *      lean/KelGroups/ — never a hardcoded list, never a filesystem walk
 *      (an untracked scratch file cannot enter the set): an omitted, added,
 *      or removed tracked source is RED until the receipt is intentionally
 *      updated;
 *   5. generates a temporary Lean driver with `#check` AND `#print axioms`
 *      for the extracted distinct declaration set;
 *   6. runs it in the repository's actual lean/ lake environment;
 *   7. classifies every citation from the fresh axiom report — `provato`
 *      (no sorryAx) or `enunciato` (depends on sorryAx: stated, not proved) —
 *      and requires CHECK_RECEIPT.axioms to equal that fresh derivation;
 *      a citation the report cannot classify is RED, never assumed proved;
 *   8. hashes the fresh driver output and compares it to CHECK_RECEIPT.sha;
 *   9. requires CHECK_RECEIPT.decls to equal the extracted citation set;
 *  10. verifies the ACCEPTED composition pin (immutable commit, exact tree):
 *      resolves it fresh, checks every pinned-commit citation's file:line
 *      inside the pinned source, derives the accepted routing/vote-derived
 *      tables by parsing the pinned classifiers (never a hand-copied list),
 *      requires the page's EVENT_ROUTES and per-constructor claim coverage
 *      to match them exhaustively, and re-derives the pinned proof states
 *      by ELABORATING the pinned module in a scratch worktree of the
 *      immutable commit (its own #print axioms directives report the
 *      axioms; its #guard witnesses fail the build if false);
 *  11. derives the Event constructor inventory from the accepted core pin's
 *      lean/Reactivegas/Types.lean (never EVENT_ROUTES/TAG_CLAIMS/EV),
 *      requires each cited source blob at the pin to equal origin/master,
 *      subtracts the exact dated #62 retirement manifest, and executes a
 *      valid witness through the exported real `attempt` for every remaining
 *      constructor — returning/refusing is not coverage; ok:true is required
 *      and `unknown event tag` is RED. A printed GREEN contains
 *      `machine=14/14 pinned=18 retired=4`;
 *  12. exits nonzero with a precise reason on any mismatch, zero on GREEN.
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
 * declaration flipped to provato in the receipt, a disabled sorry
 * detector (env hook RG_GATE_SORRY_DETECTOR=off, caught by the always-on
 * tripwire), and a freshly-DISCOVERED KelGroups pin removed from a scratch
 * receipt (the victim is derived from the tree, never hardcoded) — each for
 * its intended reason, then runs the unmodified production gate GREEN. Temporary artifacts live in a fresh mkdtemp
 * directory; the repository stays clean.
 */

import { readFileSync, writeFileSync, mkdtempSync, mkdirSync, rmSync, cpSync,
  existsSync } from 'node:fs';
import { createHash } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { tmpdir } from 'node:os';

const REPO = dirname(fileURLToPath(import.meta.url));
const HTML = join(REPO, 'economics-simulator.html');
const sha256 = b => createHash('sha256').update(b).digest('hex');

/* The ACCEPTED composition pin (NOTE-025/028, re-pinned to the MERGED
   commit by NOTE-029): an immutable commit, never a branch. Acceptance
   data, mirrored by the parent-owned ./gate.sh (v3); the embedded receipt
   must agree, the commit must resolve to exactly this tree, it must be
   REACHABLE FROM origin/master (an orphaned pin is RED even if locally
   resolvable), and the pinned module is re-elaborated fresh on every run. */
const ACCEPTED_COMPOSITION = {
  commit: 'c8c4dd8903cca817c814e9f84e9ff21ceba2de0c',
  tree: '641107474766534915f67651311b6bdcf1d1a574',
  module: 'lean/Reactivegas/Composition.lean',
};

/* Accepted #48 core pin: Event inventory is derived from the unique
   file in this freshness manifest — never from a parallel path constant,
   EVENT_ROUTES, TAG_CLAIMS, or EV. Pin-freshness compares each declared
   file's blob at the pin with origin/master. An empty, ambiguous, or
   repointed files list is RED. */
const MANIFEST_EVENT_FILE = 'lean/Reactivegas/Types.lean';
const ACCEPTED_CORE = {
  commit: '024dcc723fb70132c8085db2e39c7ba6d4e3a4c8',
  tree: '2914b7c5a69461b5c25678d06ae0c1393da2bfea',
  files: [MANIFEST_EVENT_FILE],
};
const DRIVER_IMPORTS = Object.freeze([
  'Reactivegas.Invariants',
  'KelGroups.Invariants',
  'KelGroups.Validate',
  'KelGroups.Vote.Invariants',
  'KelGroups.Vote.Validate',
]);
const RETIRED = ['addUser', 'electResponsabile', 'removeMember',
  'removeResponsabile'];
const RETIREMENT = Object.freeze({
  issue: '#62',
  requirement: 'R62-08',
  status: 'retired-by-#62',
  declared: '2026-08-30',
});
const CORE = resolve(REPO, 'economics-simulator-core.mjs');
const clone = x => JSON.parse(JSON.stringify(x));
const balOf = (m, k) => (m.find(([k2]) => k2 === k) || [null, 0])[1];
const totalOf = m => m.reduce((n, [, v]) => n + v, 0);
const coverageBase = () => ({
  users: [0, 1, 2],
  responsabili: [0, 1],
  conti: [[2, 100]],
  casse: [[0, 100]],
  collections: [],
});
const coverageCol = ({ accepted = [], pending = [], permitted = false } = {}) => ({
  ...coverageBase(),
  conti: [[2, 100 - accepted.reduce((n, p) => n + p.amount, 0)
                    - pending.reduce((n, p) => n + p.amount, 0)]],
  collections: [{ id: 7, referente: 0, permitted, accepted, pending }],
});

/* Parse the embedded CLAIMS manifest and CHECK_RECEIPT out of an HTML body. */
function extract(doc) {
  const mm = doc.match(/const CLAIMS = \{([\s\S]*?)\n\};/);
  if (!mm) throw new Error('manifesto CLAIMS non trovato nel documento');
  const rowRe = /'([a-z0-9-]+)':\s*\{ c: .*?k: '(teorema|definizione|NON PROVATO)', d: (null|'([A-Za-z_.]+)'), f: (null|'([^']+)'), l: (null|\d+)(?:, g: '([0-9a-f]{40})')? \}/g;
  const rows = [];
  let r;
  while ((r = rowRe.exec(mm[1])) !== null)
    rows.push({ id: r[1], k: r[2], d: r[4] || null, f: r[6] || null,
      l: r[7] === 'null' ? null : Number(r[7]), g: r[8] || null });
  if (!rows.length) throw new Error('nessuna riga estraibile dal manifesto');
  const rm = doc.match(/const CHECK_RECEIPT = \{[\s\S]*?sha: '([0-9a-f]{64})',[\s\S]*?decls: \[([\s\S]*?)\],[\s\S]*?composition: \{[\s\S]*?commit: '([0-9a-f]{40})',[\s\S]*?tree: '([0-9a-f]{40})',[\s\S]*?decls: \{([\s\S]*?)\},\n  \},[\s\S]*?axioms: \{([\s\S]*?)\},[\s\S]*?sources: \{([\s\S]*?)\},[\s\S]*?sourcePins: \{([\s\S]*?)\},\n\};/);
  if (!rm) throw new Error('CHECK_RECEIPT non trovato nel documento');
  const routesM = doc.match(/const EVENT_ROUTES = \{([\s\S]*?)\};/);
  if (!routesM) throw new Error('EVENT_ROUTES non trovato nel documento');
  const tagClaimsM = doc.match(/const TAG_CLAIMS = \{([\s\S]*?)\n\};/);
  if (!tagClaimsM) throw new Error('TAG_CLAIMS non trovato nel documento');
  const tagClaims = {};
  for (const t of tagClaimsM[1].matchAll(/(\w+): \[([^\]]*)\]/g))
    tagClaims[t[1]] = [...t[2].matchAll(/'([a-z0-9-]+)'/g)].map(x => x[1]);
  return {
    rows,
    cited: [...new Set(rows.filter(x => x.d && !x.g).map(x => x.d))].sort(),
    citedAtPin: [...new Set(rows.filter(x => x.d && x.g).map(x => x.d))].sort(),
    sha: rm[1],
    decls: [...rm[2].matchAll(/'([A-Za-z_.]+)'/g)].map(x => x[1]).sort(),
    composition: { commit: rm[3], tree: rm[4],
      decls: Object.fromEntries([...rm[5].matchAll(/'([A-Za-z_.]+)':\s*'(provato|enunciato)'/g)]
        .map(x => [x[1], x[2]])) },
    axioms: Object.fromEntries(
      [...rm[6].matchAll(/'([A-Za-z_.]+)':\s*'(provato|enunciato)'/g)].map(x => [x[1], x[2]])),
    sources: Object.fromEntries(
      [...rm[7].matchAll(/'([^']+)':\s*'([0-9a-f]{64})'/g)].map(x => [x[1], x[2]])),
    sourcePins: Object.fromEntries(
      [...rm[8].matchAll(/'([^']+)':\s*'([0-9a-f]{40})'/g)].map(x => [x[1], x[2]])),
    eventRoutes: Object.fromEntries(
      [...routesM[1].matchAll(/(\w+): '(\w+)'/g)].map(x => [x[1], x[2]])),
    tagClaims,
  };
}

/* Parse the two total classifiers out of the PINNED Composition source.
   This is the accepted routing derived fresh — never a hand-copied list. */
function parsePinnedClassifiers(src) {
  const grab = name => {
    const m = src.match(new RegExp(`def ${name} : Event → \\S+\\n([\\s\\S]*?)\\n\\n`));
    if (!m) throw new Error(`classificatore ${name} non trovato nella sorgente al pin`);
    const arms = [...m[1].matchAll(/\|\s*\.(\w+)[^=]*=>\s*\.?(\w+)/g)];
    if (!arms.length) throw new Error(`classificatore ${name}: nessun braccio estraibile`);
    return Object.fromEntries(arms.map(a => [a[1], a[2]]));
  };
  const route = grab('route');
  const voteDerived = grab('voteDerived');
  const ctors = Object.keys(route).sort();
  if (JSON.stringify(ctors) !== JSON.stringify(Object.keys(voteDerived).sort()))
    throw new Error('i due classificatori al pin coprono costruttori diversi');
  for (const c of ctors)
    if ((voteDerived[c] === 'true') !== (route[c] !== 'direct'))
      throw new Error(`classificatori al pin incoerenti su ${c} (voteDerived vs route)`);
  return { route, voteDerived, ctors };
}

/* Elaborate the pinned module in a scratch worktree of the immutable
   commit, fresh on every gate process: the module's own #print axioms
   directives yield the proof states and its #guard witnesses fail the
   build if false. The repo's lake cache primes the scratch as a pure
   optimization (lake re-hashes everything itself). Memoized per process:
   the input is an immutable commit. */
let pinElabMemo = null;
function elaboratePin(work) {
  if (pinElabMemo !== null) return pinElabMemo;
  const pinDir = join(work, 'pin-composition');
  execFileSync('git', ['-C', REPO, 'worktree', 'add', '--detach', pinDir,
    ACCEPTED_COMPOSITION.commit], { stdio: ['ignore', 'pipe', 'pipe'] });
  try {
    try { cpSync(join(REPO, 'lean', '.lake'), join(pinDir, 'lean', '.lake'),
      { recursive: true }); } catch (e) { /* cold cache: lake rebuilds */ }
    pinElabMemo = execFileSync('nix',
      ['develop', REPO, '-c', 'bash', '-c', 'cd lean && lake build Reactivegas.Composition 2>&1'],
      { cwd: pinDir, encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] });
  } finally {
    try { execFileSync('git', ['-C', REPO, 'worktree', 'remove', '--force', pinDir],
      { stdio: ['ignore', 'pipe', 'pipe'] }); } catch (e) { /* best effort */ }
  }
  return pinElabMemo;
}

/*
 * Discover the authoritative KelGroups source set FRESH on every run: the
 * TRACKED *.lean files (git ls-files) for lean/KelGroups.lean plus
 * everything recursively under lean/KelGroups/, from the real repository —
 * never a hardcoded list and never a filesystem walk, so an untracked
 * scratch file cannot enter the set while a tracked source added to or
 * removed from the tree changes it on the next run and the coverage
 * comparison in runGate goes RED until the receipt is intentionally
 * updated. Throws if git cannot enumerate (RED downstream, never an empty
 * silent pass).
 */
function discoverKelGroups(repo) {
  let out;
  try {
    out = execFileSync('git',
      ['-C', repo, 'ls-files', '--', 'lean/KelGroups.lean', 'lean/KelGroups'],
      { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] });
  } catch (e) {
    throw new Error('scoperta KelGroups fallita: git ls-files non eseguibile in ' + repo);
  }
  const set = out.split('\n').filter(f => f.endsWith('.lean')).sort();
  if (!set.length)
    throw new Error('scoperta KelGroups vuota: nessuna sorgente tracciata sotto ' + repo);
  return set;
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

function gitShow(revPath) {
  return execFileSync('git', ['-C', REPO, 'rev-parse', revPath],
    { encoding: 'utf8' }).trim();
}

function assertCitedFilesFresh(pin, files) {
  for (const file of files) {
    const pinnedBlob = gitShow(`${pin}:${file}`);
    const masterBlob = gitShow(`origin/master:${file}`);
    if (pinnedBlob !== masterBlob)
      throw new Error(`stale cited file ${file}: pin blob=${pinnedBlob} origin/master blob=${masterBlob}`);
  }
}

function eventSourceFromManifest(files = ACCEPTED_CORE.files) {
  if (!Array.isArray(files) || files.length !== 1)
    throw new Error('Event source manifest is not a unique derivation file: [' +
      (Array.isArray(files) ? files.join(',') : String(files)) + ']');
  return files[0];
}

function pinnedConstructors() {
  const gotTree = gitShow(`${ACCEPTED_CORE.commit}^{tree}`);
  if (gotTree !== ACCEPTED_CORE.tree)
    throw new Error(`accepted core tree mismatch: ${gotTree}`);
  execFileSync('git', ['-C', REPO, 'merge-base', '--is-ancestor',
    ACCEPTED_CORE.commit, 'origin/master']);
  assertCitedFilesFresh(ACCEPTED_CORE.commit, ACCEPTED_CORE.files);
  const eventSource = eventSourceFromManifest(ACCEPTED_CORE.files);
  const src = execFileSync('git', ['-C', REPO, 'show',
    `${ACCEPTED_CORE.commit}:${eventSource}`], { encoding: 'utf8' });
  const block = src.match(/inductive Event where([\s\S]*?)deriving DecidableEq, Repr/);
  if (!block) throw new Error('pinned Lean Event declaration not found');
  const ctors = [...block[1].matchAll(/^\s*\|\s+([A-Za-z][A-Za-z0-9_]*)\b/gm)]
    .map(m => m[1]);
  if (ctors.length !== 18 || new Set(ctors).size !== 18)
    throw new Error(`pinned Lean Event inventory is not exactly 18: ${ctors.join(',')}`);
  return ctors;
}

function validWitness(tag) {
  switch (tag) {
    case 'openPurchase': return [coverageBase(), { tag, author: 0, c: 7 }];
    case 'grantPermission': return [coverageCol(), { tag, author: 0, c: 7 }];
    case 'denyPermission': return [coverageCol(), { tag, author: 0, c: 7 }];
    case 'deposit': return [coverageBase(), { tag, author: 0, user: 2, v: 10 }];
    case 'withdraw': return [coverageBase(), { tag, author: 0, user: 2, v: 10 }];
    case 'transferCassa': return [coverageBase(), { tag, author: 0, from_: 1, v: 10 }];
    case 'donate': return [coverageBase(), { tag, author: 0, v: 90 }];
    case 'pledge': return [coverageCol(), { tag, author: 0, user: 2, c: 7, v: 10 }];
    case 'acceptPledge': return [coverageCol({ pending: [{ user: 2, amount: 10 }] }),
      { tag, author: 0, user: 2, c: 7 }];
    case 'refusePledge': return [coverageCol({ pending: [{ user: 2, amount: 10 }] }),
      { tag, author: 0, user: 2, c: 7 }];
    case 'correctPledge': return [coverageCol({ accepted: [{ user: 2, amount: 10 }] }),
      { tag, author: 0, user: 2, c: 7, v: 5 }];
    case 'closePurchase': return [coverageCol({ accepted: [{ user: 2, amount: 10 }], permitted: true }),
      { tag, author: 0, c: 7 }];
    case 'failPurchase': return [coverageCol({ accepted: [{ user: 2, amount: 10 }] }),
      { tag, author: 0, c: 7 }];
    default: return null;
  }
}

function requireRefused(attempt, state, event, label) {
  let result;
  try { result = attempt(clone(state), event); }
  catch (e) { return `${label}: threw instead of refusing: ${e.message}`; }
  if (!result || result.ok !== false) return `${label}: invalid event was not refused`;
  return null;
}

async function loadCore(corePath) {
  return import(`${pathToFileURL(corePath).href}?audit=${Date.now()}-${Math.random()}`);
}

async function checkMachineCoverage(corePath) {
  const reasons = [];
  let ctors;
  try { ctors = pinnedConstructors(); }
  catch (e) { return { ok: false, reasons: [e.message] }; }
  const active = ctors.filter(x => !RETIRED.includes(x));
  let mod;
  try { mod = await loadCore(corePath); }
  catch (e) { return { ok: false, reasons: ['core import failed: ' + e.message] }; }

  const manifest = mod.EVENT_RETIREMENTS;
  if (!manifest || typeof manifest !== 'object') {
    reasons.push('EVENT_RETIREMENTS manifest missing');
  } else {
    const keys = Object.keys(manifest).sort();
    if (JSON.stringify(keys) !== JSON.stringify(RETIRED))
      reasons.push(`retirement manifest keys differ: ${keys.join(',')}`);
    for (const tag of RETIRED) {
      const row = manifest[tag];
      for (const [k, v] of Object.entries(RETIREMENT))
        if (!row || row[k] !== v) reasons.push(`${tag}: retirement ${k} is not ${v}`);
    }
  }

  for (const tag of active) {
    if (tag === 'donate' || tag === 'backdonate') continue;
    const witness = validWitness(tag);
    if (!witness) { reasons.push(`${tag}: oracle witness missing`); continue; }
    try {
      const result = mod.attempt(clone(witness[0]), witness[1]);
      if (!result || result.ok !== true) reasons.push(`${tag}: live witness refused`);
    } catch (e) { reasons.push(`${tag}: live witness threw: ${e.message}`); }
  }

  let donated = null;
  try { donated = mod.attempt(coverageBase(), { tag: 'donate', author: 0, v: 90 }); }
  catch (e) { reasons.push(`donate: live witness threw: ${e.message}`); }
  if (donated && donated.ok === true) {
    const before = coverageBase();
    const after = donated.state;
    const common = after.conti.filter(([k]) => !after.users.includes(k));
    if (balOf(after.casse, 0) - balOf(before.casse, 0) !== 90)
      reasons.push('donate: author cassa delta is not +90');
    if (totalOf(after.conti) - totalOf(before.conti) !== 90)
      reasons.push('donate: total conti delta is not +90');
    if (before.users.some(u => balOf(after.conti, u) !== balOf(before.conti, u)))
      reasons.push('donate: changed a member conto');
    if (common.length !== 1 || common[0][1] !== 90)
      reasons.push('donate: no unique reserved non-member comune conto at +90');
    try {
      const back = mod.attempt(clone(after), { tag: 'backdonate', author: 0, w: 10 });
      if (!back || back.ok !== true) reasons.push('backdonate: live witness refused');
      else {
        for (const u of after.users)
          if (balOf(back.state.conti, u) - balOf(after.conti, u) !== 10)
            reasons.push(`backdonate: member ${u} did not receive exactly +10`);
        const commonKey = common[0][0];
        if (balOf(back.state.conti, commonKey) - balOf(after.conti, commonKey) !== -30)
          reasons.push('backdonate: comune delta is not -(member-count * share)');
        if (JSON.stringify(back.state.casse) !== JSON.stringify(after.casse))
          reasons.push('backdonate: changed casse');
      }
    } catch (e) { reasons.push(`backdonate: live witness threw: ${e.message}`); }
  } else if (donated && donated.ok !== true) {
    reasons.push('donate: live witness refused');
  }

  if (typeof mod.attempt === 'function') {
    for (const failure of [
      requireRefused(mod.attempt, coverageBase(), { tag: 'donate', author: 2, v: 10 },
        'donate non-responsabile'),
      requireRefused(mod.attempt, coverageBase(), { tag: 'donate', author: 0, v: 0 },
        'donate non-positive'),
      requireRefused(mod.attempt, coverageBase(), { tag: 'backdonate', author: 2, w: 1 },
        'backdonate non-responsabile'),
      requireRefused(mod.attempt, coverageBase(), { tag: 'backdonate', author: 0, w: 0 },
        'backdonate non-positive'),
      requireRefused(mod.attempt, coverageBase(), { tag: 'backdonate', author: 0, w: 1 },
        'backdonate insufficient comune'),
    ]) if (failure) reasons.push(failure);
  }

  if (reasons.length) return { ok: false, reasons, pinned: ctors.length,
    retired: RETIRED.length, executable: active.length };
  return { ok: true, reasons: [], pinned: ctors.length, retired: RETIRED.length,
    executable: active.length };
}

function stalePinSelftest() {
  const eventSource = eventSourceFromManifest(ACCEPTED_CORE.files);
  const masterBlob = gitShow(`origin/master:${eventSource}`);
  const history = execFileSync('git', ['-C', REPO, 'rev-list', `${ACCEPTED_CORE.commit}^`],
    { encoding: 'utf8' }).trim().split('\n');
  let stalePin = null;
  for (const candidate of history) {
    try {
      const blob = execFileSync('git', ['-C', REPO, 'rev-parse',
        `${candidate}:${eventSource}`],
        { encoding: 'utf8', stdio: ['ignore', 'pipe', 'ignore'] }).trim();
      if (blob !== masterBlob) { stalePin = candidate; break; }
    } catch { /* file did not yet exist */ }
  }
  if (!stalePin) throw new Error(`no historical stale pin found for ${eventSource}`);
  let staleMessage = '';
  try { assertCitedFilesFresh(stalePin, [eventSource]); }
  catch (e) { staleMessage = e.message; }
  if (!new RegExp(`stale cited file ${eventSource}: pin blob=[0-9a-f]{40} origin/master blob=[0-9a-f]{40}`)
    .test(staleMessage))
    throw new Error(`stale-pin control did not RED with file and both blobs: ${staleMessage}`);
}

const MACHINE_CONTROLS = 'removed-attempt-case,stale-event-pin,manifest-removed,manifest-ambiguous,manifest-repointed';

function requireUniqueManifestNeedle() {
  const src = readFileSync(fileURLToPath(import.meta.url), 'utf8');
  const needle = ['files: [', 'MANIFEST_EVENT_FILE],'].join('');
  if (src.split(needle).length !== 2)
    throw new Error('selftest mutation did not match exactly one live Event source manifest');
}

async function expectManifestRed(files, expect, name) {
  requireUniqueManifestNeedle();
  const orig = ACCEPTED_CORE.files;
  if (JSON.stringify(orig) !== JSON.stringify([MANIFEST_EVENT_FILE]))
    throw new Error(`${name}: live manifest is not the unique Types.lean entry`);
  ACCEPTED_CORE.files = files;
  try {
    const r = await checkMachineCoverage(CORE);
    const text = (r.reasons || []).join('\n');
    if (r.ok || !expect.test(text))
      throw new Error(`${name} did not RED as expected: ${text}`);
  } finally {
    ACCEPTED_CORE.files = orig;
  }
}

let leanModulesReadyFor = null;
function ensureLeanModules(lakeRepo) {
  if (leanModulesReadyFor === lakeRepo) return;
  if (!DRIVER_IMPORTS.length)
    throw new Error('generated driver import list is empty');
  execFileSync('nix',
    ['develop', lakeRepo, '-c', 'lake', 'build', ...DRIVER_IMPORTS],
    { cwd: join(lakeRepo, 'lean'), encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] });
  leanModulesReadyFor = lakeRepo;
}

async function removedAttemptCaseControl() {
  const dir = mkdtempSync(join(tmpdir(), 'rg-claim-machine-'));
  try {
    const source = readFileSync(CORE, 'utf8');
    const needle = "case 'deposit': {";
    if (source.split(needle).length !== 2)
      throw new Error('selftest mutation did not match exactly one live attempt case');
    const mutant = join(dir, 'economics-simulator-core-mutant.mjs');
    writeFileSync(mutant, source.replace(needle, "case 'deposit_REMOVED': {"));
    const r = await checkMachineCoverage(mutant);
    const message = (r.reasons || []).join('\n');
    if (r.ok || !/deposit: live witness threw: unknown event tag: deposit/.test(message))
      throw new Error(`removed-live-case control did not RED for machine reachability: ${message}`);
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
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
  const pinKeys = Object.keys(ex.sourcePins || {}).sort();
  const srcKeys = Object.keys(ex.sources).sort();
  if (JSON.stringify(pinKeys) !== JSON.stringify(srcKeys))
    reasons.push('sourcePins deve coprire esattamente CHECK_RECEIPT.sources');
  for (const row of ex.rows) {
    if (row.k === 'NON PROVATO') continue;
    if (!row.f) reasons.push(`${row.id}: collegamento citazione senza file`);
    if (!Number.isInteger(row.l) || row.l <= 0)
      reasons.push(`${row.id}: collegamento citazione senza riga`);
    const pin = row.g || (row.f && ex.sourcePins && ex.sourcePins[row.f]);
    if (!pin) reasons.push(`${row.id}: collegamento citazione senza pin`);
  }
  for (const [f, pin] of Object.entries(ex.sourcePins || {})) {
    if (!/^[0-9a-f]{40}$/.test(pin || '')) {
      reasons.push(`pin mancante/non SHA per ${f}`);
      continue;
    }
    try {
      execFileSync('git', ['-C', lakeRepo, 'merge-base', '--is-ancestor',
        pin, 'origin/master'], { stdio: ['ignore', 'pipe', 'pipe'] });
    } catch (e) {
      reasons.push(`pin non raggiungibile da origin/master: ${f}`);
      continue;
    }
    try {
      const body = execFileSync('git', ['-C', lakeRepo, 'show', `${pin}:${f}`]);
      if (sha256(body) !== ex.sources[f])
        reasons.push(`pin ${String(pin).slice(0, 10)} non risolve l'hash ricevuta per ${f}`);
    } catch (e) {
      reasons.push(`blob assente al pin per ${f}`);
    }
  }
  if (JSON.stringify(ex.decls) !== JSON.stringify(ex.cited))
    reasons.push('CHECK_RECEIPT.decls ≠ citazioni estratte — solo-ricevuta: [' +
      ex.decls.filter(d => !ex.cited.includes(d)) + '] solo-manifesto: [' +
      ex.cited.filter(d => !ex.decls.includes(d)) + ']');

  // exhaustive KelGroups coverage (NOTE-022/023): the receipt's KelGroups
  // pin set must exactly equal the TRACKED set discovered fresh from the
  // real repository (hash verification below still reads sourcesRoot, so
  // the touched-source controls keep exercising the production hash path)
  let discovered;
  try { discovered = discoverKelGroups(lakeRepo); }
  catch (e) { return { ok: false, reasons: [...reasons, e.message] }; }
  const pinnedKel = Object.keys(ex.sources)
    .filter(f => f === 'lean/KelGroups.lean' || f.startsWith('lean/KelGroups/')).sort();
  for (const f of discovered.filter(f => !pinnedKel.includes(f)))
    reasons.push('pin mancante per sorgente KelGroups scoperta: ' + f);
  for (const f of pinnedKel.filter(f => !discovered.includes(f)))
    reasons.push('pin per sorgente KelGroups non tracciata nell\'albero: ' + f);

  /* --- accepted composition pin (NOTE-025/028) --------------------------- */
  // resolve the receipt's pin fresh; unresolvable, moved, or drifted is RED
  let resolvedTree = null;
  try {
    execFileSync('git', ['-C', lakeRepo, 'cat-file', '-e', ex.composition.commit + '^{commit}'],
      { stdio: ['ignore', 'pipe', 'pipe'] });
    resolvedTree = execFileSync('git', ['-C', lakeRepo, 'rev-parse', ex.composition.commit + '^{tree}'],
      { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }).trim();
  } catch (e) {
    reasons.push('commit composizione non risolvibile: ' + ex.composition.commit);
  }
  if (resolvedTree !== null) {
    // stable reachability (NOTE-029 / gate v3): the pin must be an ancestor
    // of origin/master — an orphaned commit is rejected even when locally
    // resolvable, BEFORE any equality masking can hide the reason
    let reachable = false;
    try {
      execFileSync('git', ['-C', lakeRepo, 'merge-base', '--is-ancestor',
        ex.composition.commit, 'origin/master'], { stdio: ['ignore', 'pipe', 'pipe'] });
      reachable = true;
    } catch (e) { /* exit 1: not an ancestor */ }
    if (!reachable)
      reasons.push('pin composizione non raggiungibile da origin/master (commit orfano): ' +
        ex.composition.commit);
    if (resolvedTree !== ex.composition.tree)
      reasons.push(`albero del pin divergente dal dichiarato — dichiarato=${ex.composition.tree.slice(0, 12)}… risolto=${resolvedTree.slice(0, 12)}…`);
    if (ex.composition.commit !== ACCEPTED_COMPOSITION.commit ||
        ex.composition.tree !== ACCEPTED_COMPOSITION.tree)
      reasons.push('pin composizione ≠ composizione accettata');
  }
  let pinned = null;
  if (resolvedTree === ACCEPTED_COMPOSITION.tree &&
      ex.composition.commit === ACCEPTED_COMPOSITION.commit) {
    let pinnedSrc;
    try {
      pinnedSrc = execFileSync('git', ['-C', lakeRepo, 'show',
        ex.composition.commit + ':' + ACCEPTED_COMPOSITION.module],
        { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] });
    } catch (e) { reasons.push('modulo Composition assente al pin'); }
    if (pinnedSrc) {
      // pinned file:line for every pinned-commit citation
      const pinnedLines = pinnedSrc.split('\n');
      for (const row of ex.rows) {
        if (!row.g) continue;
        if (row.g !== ex.composition.commit)
          { reasons.push(`${row.id}: pin diverso dalla composizione della ricevuta`); continue; }
        if (row.f !== ACCEPTED_COMPOSITION.module)
          { reasons.push(`${row.id}: cita un modulo non accettato al pin`); continue; }
        const line = pinnedLines[row.l - 1] || '';
        const localName = row.d.split('.').pop();
        if (!line.includes(localName))
          reasons.push(`${row.id}: ${row.f}:${row.l}@pin non contiene ${localName} — «${line.trim().slice(0, 60)}»`);
      }
      // derive the accepted routing FRESH from the pinned classifiers
      try {
        pinned = parsePinnedClassifiers(pinnedSrc);
      } catch (e) { reasons.push(e.message); }
      if (!pinnedSrc.includes('#guard productionVerdictWitness'))
        reasons.push('testimone productionVerdictWitness senza #guard al pin');
      if (pinned) {
        // route-list drift: the page table must equal the derived table
        const pageRoutes = ex.eventRoutes;
        for (const c of pinned.ctors)
          if (pageRoutes[c] !== pinned.route[c])
            reasons.push(`instradamento divergente dal pin per ${c}: pagina=${pageRoutes[c] || 'assente'} pin=${pinned.route[c]}`);
        for (const c of Object.keys(pageRoutes))
          if (!pinned.route[c])
            reasons.push(`instradamento di pagina per costruttore non al pin: ${c}`);
        // exhaustive claim coverage, derived from the pinned classifier
        const rowIds = new Set(ex.rows.map(x => x.id));
        for (const c of pinned.ctors) {
          const ids = ex.tagClaims[c] || [];
          if (!ids.length) { reasons.push('costruttore senza righe di manifesto: ' + c); continue; }
          for (const id of ids)
            if (!rowIds.has(id)) reasons.push(`costruttore ${c}: riga inesistente ${id}`);
          if (pinned.route[c] !== 'direct') {
            const need = ['comp-routing', 'join-vote-econ',
              pinned.route[c] === 'baseEnacted' ? 'comp-base-threshold' : 'comp-app-verdict'];
            for (const nid of need)
              if (!ids.includes(nid))
                reasons.push(`costruttore ${c} (${pinned.route[c]}) senza riga ${nid}`);
          }
        }
      }
      // re-derive the pinned proof states by ELABORATING the pinned module
      let pinOut = null;
      try { pinOut = elaboratePin(work); }
      catch (e) {
        reasons.push('elaborazione del modulo al pin fallita: ' +
          (String(e.stdout || '') + String(e.stderr || '')).slice(-300));
      }
      if (pinOut !== null) {
        const pinDecls = Object.keys(ex.composition.decls).sort();
        if (JSON.stringify(pinDecls) !== JSON.stringify(ex.citedAtPin))
          reasons.push('composition.decls ≠ citazioni al pin estratte dal manifesto');
        const { derived: pinDerived, rawLine: pinRaw } = deriveAxioms(
          pinDecls.filter(d => !d.endsWith('Witness')), pinOut);
        for (const d of pinDecls) {
          let got;
          if (d.endsWith('Witness')) {
            // a #guard-ed witness: the build fails if it is false
            got = 'provato';
          } else if (!pinDerived[d]) {
            reasons.push(`stato al pin non classificabile per ${d} — il report fresco non lo nomina`);
            continue;
          } else {
            got = pinDerived[d];
            if (got === 'provato' && pinRaw[d].includes('sorryAx'))
              reasons.push(`rilevatore sorryAx disattivato o guasto (pin): ${d}`);
          }
          if (ex.composition.decls[d] !== got)
            reasons.push(`stato al pin divergente per ${d}: dichiarato=${ex.composition.decls[d]} derivato=${got}`);
        }
      }
    }
  }

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
    if (row.g) continue;   // pinned-commit citations verified above at the pin
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
    ...DRIVER_IMPORTS.map(m => 'import ' + m),
    '',
    '-- generated by economics-simulator-claim-gate.mjs from the embedded manifest',
    ...ex.cited.flatMap(d => [`#check @${d}`, `#print axioms ${d}`]), ''].join('\n'));
  let out;
  try { ensureLeanModules(lakeRepo); }
  catch (e) {
    const all = String(e.stdout || '') + '\n' + String(e.stderr || '');
    return { ok: false, reasons: ['bootstrap Lean modules for generated driver failed: ' +
      all.slice(-400)] };
  }
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
  const canonMap = m => JSON.stringify(Object.keys(m).sort().map(k => [k, m[k]]));
  if (canonMap(ex.axioms) !== canonMap(derived)) {
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
    axioms: derived, enun, kelPins: pinnedKel.length };
}

/* --- selftest: the mandatory negative axes, then production GREEN ---------- */

async function selftest(work) {
  const doc = readFileSync(HTML, 'utf8');

  const mc = await checkMachineCoverage(CORE);
  if (!mc.ok) {
    console.error('SELFTEST RED: copertura macchina non torna GREEN:\n' + mc.reasons.join('\n'));
    return 1;
  }
  try {
    await removedAttemptCaseControl();
    stalePinSelftest();
    await expectManifestRed([],
      /Event source manifest is not a unique derivation file: \[\]/,
      'manifest-removed');
    await expectManifestRed(
      [MANIFEST_EVENT_FILE, 'lean/Reactivegas/Step.lean'],
      /Event source manifest is not a unique derivation file: \[lean\/Reactivegas\/Types.lean,lean\/Reactivegas\/Step.lean\]/,
      'manifest-ambiguous');
    await expectManifestRed(['lean/Reactivegas/Step.lean'],
      /pinned Lean Event declaration not found/,
      'manifest-repointed');
  } catch (e) {
    console.error('SELFTEST RED: controllo macchina: ' + e.message);
    return 1;
  }
  console.log('machine-controls=' + MACHINE_CONTROLS);

  // production GREEN is required, and its fresh derivation is the
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

  // positive control: an UNTRACKED scratch source under lean/KelGroups/ must
  // not alter the authoritative set — discovery is git ls-files, not a
  // filesystem walk. The scratch file is created in the real tree, checked,
  // and removed in finally; validity is asserted (it must exist while the
  // discovery runs, or the control proved nothing).
  {
    const before = discoverKelGroups(REPO);
    const scratch = join(REPO, 'lean', 'KelGroups', 'ScratchUntrackedClaimGateSelftest.lean');
    let during;
    writeFileSync(scratch, '-- untracked scratch: must never enter the authoritative set\n');
    try {
      if (!existsSync(scratch)) throw new Error('controllo mal costruito: scratch assente');
      during = discoverKelGroups(REPO);
    } finally { rmSync(scratch, { force: true }); }
    if (JSON.stringify(before) !== JSON.stringify(during) ||
        during.some(f => f.includes('ScratchUntracked'))) {
      console.error('SELFTEST RED: una sorgente non tracciata è entrata nell\'insieme autoritativo');
      return 1;
    }
    console.log('controllo positivo «scratch non tracciata ignorata»: insieme autoritativo invariato ' +
      `(${during.length} sorgenti tracciate)`);
  }

  const controls = [
    {
      name: 'collegamento citazione senza pin',
      expect: /collegamento citazione senza pin/,
      run: () => {
        const ex = extract(doc);
        const f = 'lean/Reactivegas/Invariants.lean';
        const needle = `'${f}': '${ex.sourcePins[f]}',`;
        if (!doc.includes(needle) || doc.split(needle).length !== 2)
          return { ok: false, reasons: ['controllo mal costruito: pin sourcePins non unico'] };
        const p = join(work, 'sab-link-pin.html');
        writeFileSync(p, doc.replace(needle, ''));
        return runGate({ html: p, work });
      },
    },
    {
      name: 'collegamento citazione senza file',
      expect: /collegamento citazione senza file/,
      run: () => {
        const needle = "d: 'step_authorized', f: 'lean/Reactivegas/Invariants.lean', l: 452";
        if (!doc.includes(needle) || doc.split(needle).length !== 2)
          return { ok: false, reasons: ['controllo mal costruito: riga auth non unica'] };
        const p = join(work, 'sab-link-file.html');
        writeFileSync(p, doc.replace(needle,
          "d: 'step_authorized', f: null, l: 452"));
        return runGate({ html: p, work });
      },
    },
    {
      name: 'collegamento citazione senza riga',
      expect: /collegamento citazione senza riga/,
      run: () => {
        const needle = "d: 'step_authorized', f: 'lean/Reactivegas/Invariants.lean', l: 452";
        if (!doc.includes(needle) || doc.split(needle).length !== 2)
          return { ok: false, reasons: ['controllo mal costruito: riga auth non unica'] };
        const p = join(work, 'sab-link-line.html');
        writeFileSync(p, doc.replace(needle,
          "d: 'step_authorized', f: 'lean/Reactivegas/Invariants.lean', l: null"));
        return runGate({ html: p, work });
      },
    },
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
    {
      name: 'commit composizione non risolvibile',
      expect: /commit composizione non risolvibile/,
      run: () => {
        const p = join(work, 'sab-comp-unres.html');
        writeFileSync(p, doc.replace(`commit: '${ACCEPTED_COMPOSITION.commit}',`,
          `commit: '${'f'.repeat(40)}',`));
        return runGate({ html: p, work });
      },
    },
    {
      name: 'pin composizione spostato su un commit risolvibile',
      expect: /albero del pin divergente dal dichiarato/,
      run: () => {
        const head = execFileSync('git', ['-C', REPO, 'rev-parse', 'HEAD'],
          { encoding: 'utf8' }).trim();
        const p = join(work, 'sab-comp-moved.html');
        writeFileSync(p, doc.replace(`commit: '${ACCEPTED_COMPOSITION.commit}',`,
          `commit: '${head}',`));
        return runGate({ html: p, work });
      },
    },
    {
      name: 'pin orfano risolvibile ma non raggiungibile da origin/master',
      // the OLD pre-merge pin: locally resolvable with a CONSISTENT declared
      // tree, rejected specifically for stable reachability (NOTE-029)
      expect: /non raggiungibile da origin\/master \(commit orfano\): fcd4dc3037/,
      run: () => {
        const p = join(work, 'sab-comp-orphan.html');
        writeFileSync(p, doc
          .replace(`commit: '${ACCEPTED_COMPOSITION.commit}',`,
            "commit: 'fcd4dc3037c3621f2a8d5c452fe21c7a53443037',")
          .replace(`tree: '${ACCEPTED_COMPOSITION.tree}',`,
            "tree: 'dee9dfde87bff8e5c5e1b0e37655c19ee5d9b917',"));
        return runGate({ html: p, work });
      },
    },
    {
      name: 'stato del teorema al pin flippato',
      expect: /stato al pin divergente per Reactivegas\.Composition\.voteDerived_iff_not_direct/,
      run: () => {
        const needle = "'Reactivegas.Composition.voteDerived_iff_not_direct': 'provato',";
        if (!doc.includes(needle)) return { ok: false,
          reasons: ['controllo mal costruito: stato al pin non trovato nel documento'] };
        const p = join(work, 'sab-comp-status.html');
        writeFileSync(p, doc.replace(needle,
          "'Reactivegas.Composition.voteDerived_iff_not_direct': 'enunciato',"));
        return runGate({ html: p, work });
      },
    },
    {
      name: 'instradamento di pagina divergente dal pin',
      expect: /instradamento divergente dal pin per donate/,
      run: () => {
        const needle = "donate: 'direct'";
        if (!doc.includes(needle)) return { ok: false,
          reasons: ['controllo mal costruito: instradamento donate non trovato'] };
        const p = join(work, 'sab-route-drift.html');
        writeFileSync(p, doc.replace(needle, "donate: 'appDecided'"));
        return runGate({ html: p, work });
      },
    },
    {
      name: 'costruttore senza righe di copertura',
      expect: /costruttore senza righe di manifesto: backdonate/,
      run: () => {
        const ex = extract(doc);
        const needle = `backdonate: [${ex.tagClaims.backdonate.map(i => `'${i}'`).join(', ')}],`;
        if (!doc.includes(needle)) return { ok: false,
          reasons: ['controllo mal costruito: copertura backdonate non trovata'] };
        const p = join(work, 'sab-coverage.html');
        writeFileSync(p, doc.replace(needle, 'backdonate: [],'));
        return runGate({ html: p, work });
      },
    },
    {
      name: 'pin KelGroups omesso dalla ricevuta',
      // the victim is DISCOVERED fresh from the tree, never hardcoded: the
      // first source the production coverage walk finds has its pin removed
      // from a scratch receipt, and the SAME production check must name it
      expect: /pin mancante per sorgente KelGroups scoperta/,
      run: () => {
        const victims = discoverKelGroups(REPO);
        const ex = extract(doc);
        const victim = victims.find(f => ex.sources[f]);
        if (!victim) return { ok: false,
          reasons: ['controllo mal costruito: nessuna sorgente KelGroups scoperta e pinnata'] };
        const needle = `'${victim}': '${ex.sources[victim]}',`;
        if (!doc.includes(needle)) return { ok: false,
          reasons: [`controllo mal costruito: pin ${victim} non trovato nel documento`] };
        const p = join(work, 'sab-missing-pin.html');
        writeFileSync(p, doc.replace(needle, ''));
        return runGate({ html: p, work });
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
  console.log(`selftest GREEN: ${controls.length} controlli negativi RED per il motivo atteso; ` +
    `produzione GREEN (${green.rows} righe, ${green.cited} citazioni, ` +
    `${green.enun} enunciate, sha ${green.sha.slice(0, 12)}…); ` +
    `machine-controls=${MACHINE_CONTROLS}`);
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
    code = await selftest(work);
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
    const mc = await checkMachineCoverage(CORE);
    if (!mc.ok) {
      console.error(`RED: ${mc.reasons.length} problemi`);
      mc.reasons.forEach(x => console.error(' - ' + x));
      code = 1;
    } else {
      const r = runGate({ work });
      if (r.ok) {
        console.log(`GREEN: ${r.rows} righe di manifesto, ${r.cited} citazioni verificate nel ` +
          `lake env; stati derivati (${r.enun} enunciate, ${r.cited - r.enun} provate); ` +
          `file:line risolti; hash sorgenti confermati; copertura KelGroups esaustiva ` +
          `(${r.kelPins} sorgenti scoperte = pinnate); pin composizione ` +
          `${ACCEPTED_COMPOSITION.commit.slice(0, 10)}… verificato (albero esatto, righe al pin, ` +
          `instradamento derivato, copertura dei costruttori, stati elaborati freschi); ` +
          `ricevuta legata (sha ${r.sha.slice(0, 12)}…); ` +
          `machine=${mc.executable}/${mc.executable} pinned=${mc.pinned} retired=${mc.retired}`);
        code = 0;
      } else {
        console.error(`RED: ${r.reasons.length} problemi`);
        r.reasons.forEach(x => console.error(' - ' + x));
        code = 1;
      }
    }
  }
} finally {
  rmSync(work, { recursive: true, force: true });
}
process.exit(code);
