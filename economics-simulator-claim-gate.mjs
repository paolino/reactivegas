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
 *      requires each cited source blob at the pin to equal HEAD's,
 *      subtracts the exact dated #62 retirement manifest, and executes a
 *      valid witness through the exported real `attempt` for every remaining
 *      constructor — returning/refusing is not coverage; ok:true is required
 *      and `unknown event tag` is RED. It then DISCOVERS every event
 *      vocabulary at the pin (each inductive under lean/Reactivegas and
 *      lean/KelGroups named …Event, …Command, …Proposal or …Mutation): a
 *      routed one (AppEvent with the #81 vote set, VoteEvent,
 *      IntegratedEvent, DirectCommand, Reactivegas.Proposal) needs, per
 *      parsed constructor, a live witness through `applyIntegrated` that is
 *      applied AND shows the Lean effect; the rest carry a named
 *      non-presented reason; anything else, an empty extent or a stale
 *      table entry is RED. No count is written anywhere;
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
 * directory; the repository stays clean. Last, in a throwaway worktree of
 * HEAD it commits Lean edits and judges that worktree as the checkout under
 * test, one control per rule judged against HEAD (see leanBranchControls):
 * a cited Lean file edited with re-made receipts passes, without them REDs
 * naming the file; edited pinned modules and vocabularies RED against HEAD;
 * orphaned source and core pins RED on reachability.
 */

import { readFileSync, writeFileSync, mkdtempSync, mkdirSync, rmSync, cpSync,
  existsSync } from 'node:fs';
import { createHash } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { tmpdir } from 'node:os';

const REPO = dirname(fileURLToPath(import.meta.url));

/* Teardown of a disposable resource must never determine the gate's verdict:
   scratch-dir cleanup is housekeeping and cannot veto the exit code. */
function rmQuiet(p) {
  try { rmSync(p, { recursive: true, force: true }); } catch { /* housekeeping */ }
}
const HTML = join(REPO, 'economics-simulator.html');
const sha256 = b => createHash('sha256').update(b).digest('hex');

/* The ACCEPTED composition pin: an immutable commit, never a branch — the
   master merge base of the simulator branch (2bd9a20, #81 + #92), where
   every source the receipt pins is pinned too. The embedded receipt
   must agree, the commit must resolve to exactly this tree, it must be
   an ancestor of HEAD (an orphaned pin is RED even if locally
   resolvable), and the pinned module is re-elaborated fresh on every run.

   Every pin rule here is judged against HEAD of the checkout under test —
   the branch commit locally, the PR merge commit in CI, the master tip on
   master — never against origin/master: a branch that changes a cited Lean
   file and re-makes its receipts against its own Lean must be able to pass,
   and one that changes it without re-making them must not. */
const ACCEPTED_COMPOSITION = {
  commit: '2bd9a2080a692f8a832968e76ecbd270898f2aa2',
  tree: '1ca66428464b2897f1f1a41b9b347b9ca0da5eee',
  module: 'lean/Reactivegas/Composition.lean',
};

/* Accepted #48 core pin: Event inventory is derived from the unique
   file in this freshness manifest — never from a parallel path constant,
   EVENT_ROUTES, TAG_CLAIMS, or EV. Pin-freshness compares each declared
   file's blob at the pin with its blob at HEAD. An empty, ambiguous, or
   repointed files list is RED. */
const MANIFEST_EVENT_FILE = 'lean/Reactivegas/Types.lean';
const ACCEPTED_CORE = {
  commit: '2bd9a2080a692f8a832968e76ecbd270898f2aa2',
  tree: '1ca66428464b2897f1f1a41b9b347b9ca0da5eee',
  files: [MANIFEST_EVENT_FILE],
};
const DRIVER_IMPORTS = Object.freeze([
  'Reactivegas.Invariants',
  'KelGroups.Invariants',
  'KelGroups.Validate',
  'KelGroups.Vote.Invariants',
  'KelGroups.Vote.Validate',
]);
/* The retirement manifest is GONE: #62 merged, the four constructors are out
   of the pinned Lean itself — the inventory needs no dated subtraction and
   the transcription carries no exemption any more. */
const CORE = resolve(REPO, 'economics-simulator-core.mjs');
const clone = x => JSON.parse(JSON.stringify(x));
const balOf = (m, k) => (m.find(([k2]) => k2 === k) || [null, 0])[1];
const totalOf = m => m.reduce((n, [, v]) => n + v, 0);
const ADMIN = { adminRole: { admin: 'publicAdmin' } };
const APP = { appRole: { name: 'socio' } };
const covMember = (k, admin) => [k, { key: k, email: k + '@toy.example', roles: admin ? [ADMIN] : [APP] }];
const coverageView = () => ({ members: [covMember('anna', true), covMember('bruno', true), covMember('carla', false)] });
const COMUNE = 'comune';   // Reactivegas.comuneId
const coverageCol = ({ accepted = [], pending = [], permitted = false } = {}) => ({
  conti: [['carla', 100 - accepted.reduce((n, p) => n + p.amount, 0)
                    - pending.reduce((n, p) => n + p.amount, 0)]],
  casse: [], collections: [{ id: 7, referente: 'bruno', permitted, accepted, pending }],
  votes: { openQuestions: [], closed: [] },
});

/* Parse the embedded CLAIMS manifest/* Parse the embedded CLAIMS manifest and CHECK_RECEIPT out of an HTML body. */
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

function gitShow(revPath, repo = REPO) {
  return execFileSync('git', ['-C', repo, 'rev-parse', revPath],
    { encoding: 'utf8' }).trim();
}

function assertCitedFilesFresh(pin, files, repo = REPO) {
  for (const file of files) {
    const pinnedBlob = gitShow(`${pin}:${file}`, repo);
    const headBlob = gitShow(`HEAD:${file}`, repo);
    if (pinnedBlob !== headBlob)
      throw new Error(`stale cited file ${file}: pin blob=${pinnedBlob} HEAD blob=${headBlob}`);
  }
}

/* INV-8: the cited composition module joins the freshness manifest — its
   blob at the pin must equal its blob at HEAD, so the silent drift that
   slipped through #62 REDs from now on. */
function assertCompositionModuleFresh(pin, module, repo = REPO) {
  const pinnedBlob = gitShow(pin + ':' + module, repo);
  const headBlob = gitShow('HEAD:' + module, repo);
  if (pinnedBlob !== headBlob)
    throw new Error('modulo composizione obsoleto al pin: pin=' + pinnedBlob +
      ' HEAD=' + headBlob);
}

function eventSourceFromManifest(files = ACCEPTED_CORE.files) {
  if (!Array.isArray(files) || files.length !== 1)
    throw new Error('Event source manifest is not a unique derivation file: [' +
      (Array.isArray(files) ? files.join(',') : String(files)) + ']');
  return files[0];
}

function pinnedConstructors(repo = REPO) {
  const gotTree = gitShow(ACCEPTED_CORE.commit + "^{tree}", repo);
  if (gotTree !== ACCEPTED_CORE.tree)
    throw new Error("accepted core tree mismatch: " + gotTree);
  try {
    execFileSync('git', ['-C', repo, 'merge-base', '--is-ancestor',
      ACCEPTED_CORE.commit, 'HEAD'], { stdio: ['ignore', 'pipe', 'pipe'] });
  } catch (e) {
    throw new Error('pin core non raggiungibile da HEAD (commit orfano): ' + ACCEPTED_CORE.commit);
  }
  assertCitedFilesFresh(ACCEPTED_CORE.commit, ACCEPTED_CORE.files, repo);
  const eventSource = eventSourceFromManifest(ACCEPTED_CORE.files);
  const src = execFileSync('git', ['-C', repo, 'show',
    ACCEPTED_CORE.commit + ':' + eventSource], { encoding: 'utf8' });
  const block = src.match(/inductive Event where([\s\S]*?)deriving DecidableEq, Repr/);
  if (!block) throw new Error('pinned Lean Event declaration not found');
  const ctors = [...block[1].matchAll(/^\s*\|\s+([A-Za-z][A-Za-z0-9_]*)\b/gm)]
    .map(m => m[1]);
  if (!ctors.length) throw new Error('pinned Lean Event inventory is empty');
  if (new Set(ctors).size !== ctors.length)
    throw new Error('pinned Lean Event inventory has duplicates: ' + ctors.join(','));
  return ctors;
}

/* Every pinned Lean event vocabulary the simulator could route, DISCOVERED
   at the accepted pin: each inductive under lean/Reactivegas and
   lean/KelGroups whose name ends in Event, Command, Proposal or Mutation —
   never a list of names here. Each discovered inductive is either ROUTED,
   and then every constructor parsed from its declaration needs a live
   witness through the integrated root (`applyIntegrated`) that produces the
   Lean effect, or NOT PRESENTED with a named reason. A discovered inductive
   in neither table, a table entry no longer discovered, an empty extent, a
   constructor without a witness or a witness for no constructor is RED.
   The legacy Event inventory keeps its own `attempt` witnesses above. */
const VOCAB_ROOTS = ['lean/Reactivegas', 'lean/KelGroups'];
const VOCAB_RE = /^inductive ([A-Za-z]*(?:Event|Command|Proposal|Mutation))\b[^\n]*\bwhere\b/gm;

function parseVocabularies(file, src) {
  const out = [];
  for (const m of src.matchAll(VOCAB_RE)) {
    const rest = src.slice(m.index + m[0].length);
    const end = rest.search(/^(deriving|\S)/m);
    const body = end < 0 ? rest : rest.slice(0, end);
    const ctors = [...body.matchAll(/^\s*\|\s+([A-Za-z][A-Za-z0-9_]*)\b/gm)].map(c => c[1]);
    out.push({ key: `${file}#${m[1]}`, name: m[1], file, ctors });
  }
  return out;
}

function pinnedVocabularies(extra, repo = REPO) {
  const files = execFileSync('git', ['-C', repo, 'ls-tree', '-r', '--name-only',
    ACCEPTED_CORE.commit, '--', ...VOCAB_ROOTS], { encoding: 'utf8' })
    .split('\n').filter(f => f.endsWith('.lean')).sort();
  if (!files.length) throw new Error('vocabulary discovery: no Lean source at the pin');
  const vocab = [];
  for (const file of files) {
    const src = execFileSync('git', ['-C', repo, 'show', ACCEPTED_CORE.commit + ':' + file],
      { encoding: 'utf8' });
    const found = parseVocabularies(file, src);
    if (found.length) assertCitedFilesFresh(ACCEPTED_CORE.commit, [file], repo);
    vocab.push(...found);
  }
  for (const [file, src] of Object.entries(extra || {})) vocab.push(...parseVocabularies(file, src));
  if (!vocab.length) throw new Error('vocabulary discovery: empty extent');
  return vocab;
}

const voteOpen = (v, qid) => (v.openQuestions.find(([k]) => k === qid) || [])[1] || null;
const covAdmin = k => covMember(k, true);
const memberKeysOf = gs => gs.members.map(([k]) => k);

/* one live witness per routed constructor: { gs, signer, ev, holds(det) },
   applied by the integrated root, its Lean effect observable on the result */
function voteWitness(mod, tag) {
  const agg = votes => ({ members: [covAdmin('anna'), covAdmin('bruno'), covAdmin('carla')],
    pendingBase: [], appFold: { ...mod.emptyState(), votes } });
  const q = { kind: 'collective', proposer: 'anna', assents: [], dissents: [] };
  const withQ = () => agg({ openQuestions: [['q:w', clone(q)]], closed: [] });
  switch (tag) {
    case 'openQuestion': return { gs: agg({ openQuestions: [], closed: [] }), signer: 'anna',
      ev: { app: { openQuestion: { questionId: 'q:w', kind: 'collective' } } },
      holds: det => JSON.stringify(voteOpen(det.gs.appFold.votes, 'q:w')) === JSON.stringify(q) };
    case 'cast': return { gs: withQ(), signer: 'bruno',
      ev: { app: { cast: { questionId: 'q:w', ballot: 'assent' } } },
      holds: det => { const w = voteOpen(det.gs.appFold.votes, 'q:w');
        return !!w && w.assents.includes('bruno'); } };
    case 'renounce': return { gs: withQ(), signer: 'anna',
      ev: { app: { renounce: { questionId: 'q:w' } } },
      holds: det => !voteOpen(det.gs.appFold.votes, 'q:w') &&
        JSON.stringify(det.gs.appFold.votes.closed) === JSON.stringify(
          [{ questionId: 'q:w', question: q, verdict: 'negative', cause: 'renounced' }]) };
    // #76: the question opens with its signer as proposer and its target
    // bound before any ballot; the same bind of an open question is refused
    case 'openBound': {
      const target = { backdonation: { w: 5 } };
      return { gs: agg({ openQuestions: [], closed: [] }), signer: 'anna',
        ev: { app: { openBound: { questionId: 'q:w', kind: 'collective', target } } },
        holds: det => JSON.stringify(voteOpen(det.gs.appFold.votes, 'q:w')) === JSON.stringify(q) &&
          JSON.stringify(det.gs.appFold.bindings) === JSON.stringify([['q:w', target]]),
        without: { gs: withQ(), signer: 'anna',
          ev: { app: { openBound: { questionId: 'q:w', kind: 'collective', target } } } } };
    }
    default: return null;
  }
}

/* #76: the app-decided constructors spend one closure-derived authorization
   naming their exact target under the verdict they need. Their witness first
   mints it through the integrated root — anna opens a question bound to the
   target, bruno's ballot closes it (two responsabili, θ = 1) — and the same
   event on the state without that closure must be refused. */
const APP_DECIDED_AUTH = {
  grantPermission: a => ({ target: { permission: { c: a.c } }, verdict: 'positive' }),
  denyPermission: a => ({ target: { permission: { c: a.c } }, verdict: 'negative' }),
  backdonate: a => ({ target: { backdonation: { w: a.w } }, verdict: 'positive' }),
};
function mintedFor(mod, gs, { target, verdict }) {
  const open = mod.applyIntegrated(clone(gs), 'anna',
    { app: { openBound: { questionId: 'q:auth', kind: 'collective', target } } });
  if (!open || open.refused) return null;
  const shut = mod.applyIntegrated(open.gs, 'bruno', { app: { cast: { questionId: 'q:auth',
    ballot: verdict === 'positive' ? 'assent' : 'dissent' } } });
  return shut && !shut.refused ? shut.gs : null;
}

/* an economic AppEvent through the integrated app route lands exactly where
   the legacy transition `attempt` lands on the same signed event (an
   app-decided one also spends its authorization, and only with it applies) */
function economicAppWitness(mod, tag) {
  let w = validWitness(tag);
  if (!w && tag === 'backdonate') {
    const donated = mod.attempt(coverageView(), { conti: [], casse: [], collections: [],
      votes: { openQuestions: [], closed: [] } }, { tag: 'donate', author: 'anna', v: 90 });
    if (!donated || !donated.ok) return null;
    w = [coverageView(), donated.state, { tag, author: 'anna', w: 10 }];
  }
  if (!w) return null;
  const [view, state, event] = w;
  const { tag: _t, author, ...args } = event;
  const plain = { members: view.members, pendingBase: [],
    appFold: { ...clone(state), bindings: [], live: [] } };
  const auth = APP_DECIDED_AUTH[tag] ? APP_DECIDED_AUTH[tag](args) : null;
  const gs = auth ? mintedFor(mod, plain, auth) : plain;
  if (!gs) return null;
  return { gs, signer: author, ev: { app: { [tag]: args } },
    holds: det => {
      const direct = mod.attempt(view, clone(state), event);
      return !!direct && direct.ok === true &&
        mod.canonState(det.gs.appFold) === mod.canonState(direct.state) &&
        (!auth || !(det.gs.appFold.live || []).some(a =>
          JSON.stringify(a.target) === JSON.stringify(auth.target)));
    },
    without: auth ? { gs: plain, signer: author, ev: { app: { [tag]: args } } } : null };
}

function baseWitness(mod, tag) {
  const boot = () => mod.bootAggregate();
  const two = () => ({ ...boot(), members: [...boot().members, covMember('bruno', false)] });
  const four = () => ({ ...boot(), members: [covAdmin('anna'), covAdmin('bruno'), covAdmin('carla'),
    covMember('dora', false)], pendingBase: [['depart:dora',
      { proposal: { departure: 'dora' }, proposer: 'anna', approvals: ['anna'] }]] });
  switch (tag) {
    case 'admitMember': case 'direct': return { gs: boot(), signer: 'anna',
      ev: { direct: { admitMember: { key: 'bruno', email: 'bruno@toy.example', roles: [] } } },
      holds: det => memberKeysOf(det.gs).includes('bruno') &&
        JSON.stringify(det.change) === JSON.stringify({ memberAdmitted: 'bruno' }) };
    case 'departure': case 'propose': return { gs: two(), signer: 'anna',
      ev: { propose: { proposal: { departure: 'bruno' } } },
      holds: det => !memberKeysOf(det.gs).includes('bruno') &&
        JSON.stringify(det.change) === JSON.stringify({ memberRemoved: 'bruno' }) };
    case 'changeRoles': return { gs: two(), signer: 'anna',
      ev: { propose: { proposal: { changeRoles: { key: 'bruno',
        roles: [{ adminRole: { admin: 'publicAdmin' } }] } } } },
      holds: det => mod.isAdminView('bruno', { members: det.gs.members }) &&
        JSON.stringify(det.change) === JSON.stringify({ rolesChanged: 'bruno' }) };
    case 'approve': return { gs: four(), signer: 'bruno',
      ev: { approve: { proposalId: 'depart:dora' } },
      holds: det => !memberKeysOf(det.gs).includes('dora') && det.gs.pendingBase.length === 0 &&
        JSON.stringify(det.change) === JSON.stringify({ memberRemoved: 'dora' }) };
    case 'app': return { gs: boot(), signer: 'anna', ev: { app: { donate: { v: 10 } } },
      holds: det => mod.canonState(det.gs.appFold) !== mod.canonState(boot().appFold) &&
        !det.change };
    default: return null;
  }
}

const ROUTED_VOCABULARIES = {
  'lean/Reactivegas/Types.lean#AppEvent': (mod, c) => voteWitness(mod, c) || economicAppWitness(mod, c),
  'lean/KelGroups/Vote/Event.lean#VoteEvent': voteWitness,
  'lean/KelGroups/Integration.lean#IntegratedEvent': baseWitness,
  'lean/KelGroups/Event.lean#DirectCommand': baseWitness,
  'lean/Reactivegas/Types.lean#Proposal': baseWitness,
  // the legacy economic Event: covered by the attempt witnesses above
  'lean/Reactivegas/Types.lean#Event': null,
};
const NOT_PRESENTED_VOCABULARIES = {
  'lean/KelGroups/Event.lean#Proposal':
    'historical substrate proposal (introduceMember) kept as #54 evidence; Reactivegas.apply takes Reactivegas.Proposal, so it cannot reach the simulator',
  'lean/KelGroups/Event.lean#BaseEvent':
    'pre-integration substrate fold vocabulary over KelGroups.Proposal; the production root carries propose/approve inside IntegratedEvent',
  'lean/KelGroups/Event.lean#GroupEvent':
    'generic pre-integration group fold event; superseded on the production root by IntegratedEvent',
  'lean/KelGroups/Event.lean#BaseMutation':
    'substrate effect computed from Reactivegas.Proposal by proposalMutation; never a signed event',
};

function checkVocabularyCoverage(mod, vocab, routed = ROUTED_VOCABULARIES,
    notPresented = NOT_PRESENTED_VOCABULARIES) {
  const reasons = [];
  let witnessed = 0;
  const keys = new Set(vocab.map(v => v.key));
  for (const k of [...Object.keys(routed), ...Object.keys(notPresented)])
    if (!keys.has(k)) reasons.push(`vocabulary ${k}: listed but not discovered at the pin`);
  for (const v of vocab) {
    if (!v.ctors.length) { reasons.push(`vocabulary ${v.key}: no constructor parsed`); continue; }
    if (v.key in notPresented) continue;
    if (!(v.key in routed)) {
      reasons.push(`vocabulary ${v.key}: neither routed nor named non-presented`);
      continue;
    }
    const witnessOf = routed[v.key];
    if (witnessOf === null) continue;
    for (const c of v.ctors) {
      const w = witnessOf(mod, c);
      if (!w) { reasons.push(`${v.name} ${c}: no core handler witness`); continue; }
      try {
        const det = mod.applyIntegrated(clone(w.gs), w.signer, w.ev);
        if (!det || det.refused) reasons.push(`${v.name} ${c}: live witness refused (${det && det.refused})`);
        else if (!w.holds(det)) reasons.push(`${v.name} ${c}: core handler does not produce the Lean effect`);
        else if (w.without) {
          const off = mod.applyIntegrated(clone(w.without.gs), w.without.signer, w.without.ev);
          if (!off || !off.refused)
            reasons.push(`${v.name} ${c}: applied without the closure-derived authorization Lean requires`);
          else witnessed++;
        } else witnessed++;
      } catch (e) { reasons.push(`${v.name} ${c}: live witness threw: ${e.message}`); }
    }
  }
  return { reasons, witnessed, vocabularies: vocab.length };
}

function validWitness(tag) {
  const base = () => [coverageView(), { conti: [['carla', 100]], casse: [], collections: [],
    votes: { openQuestions: [], closed: [] } }];
  const col = opts => ({ conti: [['carla', 100]], casse: [],
    collections: [{ id: 7, referente: 'bruno', permitted: !!(opts && opts.permitted),
      accepted: (opts && opts.accepted) || [], pending: (opts && opts.pending) || [] }],
    votes: { openQuestions: [], closed: [] } });
  switch (tag) {
    case 'openPurchase': return [coverageView(), base()[1], { tag, author: 'anna', c: 7 }];
    case 'grantPermission': return [coverageView(), col(), { tag, author: 'anna', c: 7 }];
    case 'denyPermission': return [coverageView(), col(), { tag, author: 'anna', c: 7 }];
    case 'deposit': return [coverageView(), base()[1], { tag, author: 'anna', user: 'carla', v: 10 }];
    case 'withdraw': return [coverageView(), base()[1], { tag, author: 'anna', user: 'carla', v: 10 }];
    case 'transferCassa': return [coverageView(), base()[1], { tag, author: 'anna', from_: 'bruno', v: 10 }];
    case 'donate': return [coverageView(), base()[1], { tag, author: 'anna', v: 90 }];
    case 'pledge': return [coverageView(), col(), { tag, author: 'anna', user: 'carla', c: 7, v: 10 }];
    case 'acceptPledge': return [coverageView(), col({ pending: [{ user: 'carla', amount: 10 }] }),
      { tag, author: 'bruno', user: 'carla', c: 7 }];
    case 'refusePledge': return [coverageView(), col({ pending: [{ user: 'carla', amount: 10 }] }),
      { tag, author: 'bruno', user: 'carla', c: 7 }];
    case 'correctPledge': return [coverageView(), col({ accepted: [{ user: 'carla', amount: 10 }] }),
      { tag, author: 'bruno', user: 'carla', c: 7, v: 5 }];
    case 'closePurchase': return [coverageView(), col({ accepted: [{ user: 'carla', amount: 10 }], permitted: true }),
      { tag, author: 'bruno', c: 7 }];
    case 'failPurchase': return [coverageView(), col({ accepted: [{ user: 'carla', amount: 10 }], permitted: true }),
      { tag, author: 'bruno', c: 7 }];
    default: return null;
  }
}

function requireRefused(attempt, view, state, event, label) {
  let result;
  try { result = attempt(view, clone(state), event); }
  catch (e) { return `${label}: threw instead of refusing: ${e.message}`; }
  if (!result || result.ok !== false) return `${label}: invalid event was not refused`;
  return null;
}

async function loadCore(corePath) {
  return import(`${pathToFileURL(corePath).href}?audit=${Date.now()}-${Math.random()}`);
}

async function checkMachineCoverage(corePath, repo = REPO) {
  const reasons = [];
  let ctors;
  try { ctors = pinnedConstructors(repo); }
  catch (e) { return { ok: false, reasons: [e.message] }; }
  const active = ctors;   // no dated subtraction: the pin itself is post-#62
  let mod;
  try { mod = await loadCore(corePath); }
  catch (e) { return { ok: false, reasons: ['core import failed: ' + e.message] }; }

  // EVENT_RETIREMENTS is gone from the transcription: the exemption died
  // with the constructors it excused
  for (const tag of active) {
    if (tag === 'donate' || tag === 'backdonate') continue;
    const witness = validWitness(tag);
    if (!witness) { reasons.push(`${tag}: oracle witness missing`); continue; }
    try {
      const result = mod.attempt(witness[0], clone(witness[1]), witness[2]);
      if (!result || result.ok !== true) reasons.push(`${tag}: live witness refused`);
    } catch (e) { reasons.push(`${tag}: live witness threw: ${e.message}`); }
  }

  let donated = null;
  const base0 = { conti: [], casse: [], collections: [], votes: { openQuestions: [], closed: [] } };
  try { donated = mod.attempt(coverageView(), base0, { tag: 'donate', author: 'anna', v: 90 }); }
  catch (e) { reasons.push(`donate: live witness threw: ${e.message}`); }
  if (donated && donated.ok === true) {
    const before = base0;
    const after = donated.state;
    const memberKeys0 = coverageView().members.map(([k]) => k);
    const common = after.conti.filter(([k]) => !memberKeys0.includes(k));
    if (balOf(after.casse, 'anna') - balOf(before.casse, 'anna') !== 90)
      reasons.push('donate: author cassa delta is not +90');
    if (totalOf(after.conti) - totalOf(before.conti) !== 90)
      reasons.push('donate: total conti delta is not +90');
    if (memberKeys0.some(u => u !== COMUNE && balOf(after.conti, u) !== balOf(before.conti, u)))
      reasons.push('donate: changed a member conto');
    if (common.length !== 1 || common[0][0] !== COMUNE || common[0][1] !== 90)
      reasons.push('donate: no unique reserved non-member comune conto at +90');
    // #76: a backdonate pays only by spending the positive closure bound to
    // its share, minted through the integrated root from the funded comune;
    // the same backdonate with no such closure is refused
    try {
      const funded = { members: coverageView().members, pendingBase: [],
        appFold: { ...clone(after), bindings: [], live: [] } };
      const unclosed = mod.applyIntegrated(clone(funded), 'anna', { app: { backdonate: { w: 10 } } });
      if (!unclosed || !unclosed.refused)
        reasons.push('backdonate: applied with no closure bound to its share');
      const minted = mintedFor(mod, funded, APP_DECIDED_AUTH.backdonate({ w: 10 }));
      const back = minted && mod.applyIntegrated(clone(minted), 'anna', { app: { backdonate: { w: 10 } } });
      if (!back || back.refused) reasons.push('backdonate: live witness refused');
      else {
        const pre = minted.appFold, post = back.gs.appFold;
        for (const u of memberKeys0)
          if (balOf(post.conti, u) - balOf(pre.conti, u) !== 10)
            reasons.push(`backdonate: member ${u} did not receive exactly +10`);
        if (balOf(post.conti, COMUNE) - balOf(pre.conti, COMUNE) !== -30)
          reasons.push('backdonate: comune delta is not -(member-count * share)');
        if (JSON.stringify(post.casse) !== JSON.stringify(pre.casse))
          reasons.push('backdonate: changed casse');
        if ((post.live || []).some(a => 'backdonation' in a.target))
          reasons.push('backdonate: its closure was not spent');
      }
    } catch (e) { reasons.push(`backdonate: live witness threw: ${e.message}`); }
  } else if (donated && donated.ok !== true) {
    reasons.push('donate: live witness refused');
  }

  if (typeof mod.attempt === 'function') {
    for (const failure of [
      requireRefused(mod.attempt, coverageView(), base0, { tag: 'donate', author: 'carla', v: 10 },
        'donate non-responsabile'),
      requireRefused(mod.attempt, coverageView(), base0, { tag: 'donate', author: 'anna', v: 0 },
        'donate non-positive'),
      requireRefused(mod.attempt, coverageView(), base0, { tag: 'backdonate', author: 'carla', w: 1 },
        'backdonate non-responsabile'),
      requireRefused(mod.attempt, coverageView(), base0, { tag: 'backdonate', author: 'anna', w: 0 },
        'backdonate non-positive'),
      requireRefused(mod.attempt, coverageView(), base0, { tag: 'backdonate', author: 'anna', w: 1 },
        'backdonate insufficient comune'),
    ]) if (failure) reasons.push(failure);
  }

  let vc = { reasons: [], witnessed: 0, vocabularies: 0 };
  try { vc = checkVocabularyCoverage(mod, pinnedVocabularies(undefined, repo)); }
  catch (e) { reasons.push(e.message); }
  reasons.push(...vc.reasons);

  const tally = { pinned: ctors.length, retired: 0, executable: active.length,
    vocabularies: vc.vocabularies, witnessed: vc.witnessed };
  if (reasons.length) return { ok: false, reasons, ...tally };
  return { ok: true, reasons: [], ...tally };
}

function stalePinSelftest() {
  const eventSource = eventSourceFromManifest(ACCEPTED_CORE.files);
  const headBlob = gitShow(`HEAD:${eventSource}`);
  const history = execFileSync('git', ['-C', REPO, 'rev-list', `${ACCEPTED_CORE.commit}^`],
    { encoding: 'utf8' }).trim().split('\n');
  let stalePin = null;
  for (const candidate of history) {
    try {
      const blob = execFileSync('git', ['-C', REPO, 'rev-parse',
        `${candidate}:${eventSource}`],
        { encoding: 'utf8', stdio: ['ignore', 'pipe', 'ignore'] }).trim();
      if (blob !== headBlob) { stalePin = candidate; break; }
    } catch { /* file did not yet exist */ }
  }
  if (!stalePin) throw new Error(`no historical stale pin found for ${eventSource}`);
  let staleMessage = '';
  try { assertCitedFilesFresh(stalePin, [eventSource]); }
  catch (e) { staleMessage = e.message; }
  if (!new RegExp(`stale cited file ${eventSource}: pin blob=[0-9a-f]{40} HEAD blob=[0-9a-f]{40}`)
    .test(staleMessage))
    throw new Error(`stale-pin control did not RED with file and both blobs: ${staleMessage}`);
}

/* INV-8 negative control: the composition freshness assertion must be able
   to fail for its own stated reason. The witness is the pin this slice
   replaced — c8c4dd89's Composition.lean blob is the drift that went through
   #62 in silence. Point the production assertion at it and require the exact
   staleness RED. */
function staleCompositionPinSelftest() {
  const driftPin = 'c8c4dd8903cca817c814e9f84e9ff21ceba2de0c';
  const module = ACCEPTED_COMPOSITION.module;
  const pinnedBlob = gitShow(driftPin + ':' + module);
  const headBlob = gitShow('HEAD:' + module);
  if (pinnedBlob === headBlob)
    throw new Error('drift witness is stale itself: c8c4dd89 blob equals HEAD');
  let message = '';
  try { assertCompositionModuleFresh(driftPin, module); }
  catch (e) { message = e.message; }
  if (message !== 'modulo composizione obsoleto al pin: pin=' + pinnedBlob +
      ' HEAD=' + headBlob)
    throw new Error('stale-composition control did not RED with pin and HEAD blobs: ' + message);
}

const MACHINE_CONTROLS = 'removed-attempt-case,removed-vote-handler,unhandled-vote-constructor,unlisted-vocabulary,stale-event-pin,stale-composition-pin,manifest-removed,manifest-ambiguous,manifest-repointed';

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

/* a removed vote handler (the renounce effect no longer closes) must RED the
   derived vote coverage through the same live-witness path */
async function removedVoteHandlerControl() {
  const dir = mkdtempSync(join(tmpdir(), 'rg-claim-vote-'));
  try {
    const source = readFileSync(CORE, 'utf8');
    const needle = 'if (q) effected = { openQuestions: vtErase(questionId, gs.openQuestions),';
    if (source.split(needle).length !== 2)
      throw new Error('selftest mutation did not match exactly one live renounce handler');
    const mutant = join(dir, 'economics-simulator-core-mutant.mjs');
    writeFileSync(mutant, source.replace(needle, needle.replace('if (q)', 'if (false)')));
    const r = await checkMachineCoverage(mutant);
    const message = (r.reasons || []).join('\n');
    if (r.ok || !/VoteEvent renounce: core handler does not produce the Lean effect/.test(message))
      throw new Error(`removed-vote-handler control did not RED: ${message}`);
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
}

/* a vocabulary grown in the pinned source must RED whatever its spelling:
   a constructor the core does not handle, and an event inductive neither
   routed nor named non-presented */
async function unhandledVocabularyControl() {
  const mod = await loadCore(CORE);
  const voteFile = 'lean/KelGroups/Vote/Event.lean';
  const src = execFileSync('git', ['-C', REPO, 'show', ACCEPTED_CORE.commit + ':' + voteFile],
    { encoding: 'utf8' });
  const grown = src.replace(/inductive VoteEvent where\n/, 'inductive VoteEvent where\n  | zzUnhandled\n');
  if (grown === src) throw new Error('selftest mutation did not grow the VoteEvent inventory');
  const vocab = pinnedVocabularies().filter(v => v.key !== voteFile + '#VoteEvent')
    .concat(parseVocabularies(voteFile, grown));
  const m1 = checkVocabularyCoverage(mod, vocab).reasons.join('\n');
  if (!/VoteEvent zzUnhandled: no core handler witness/.test(m1))
    throw new Error(`unhandled-vote-constructor control did not RED: ${m1}`);
  const stray = pinnedVocabularies({ 'lean/KelGroups/ZzStray.lean':
    'inductive ZzEvent where\n  | zz\nderiving Repr\n' });
  const m2 = checkVocabularyCoverage(mod, stray).reasons.join('\n');
  if (!/vocabulary lean\/KelGroups\/ZzStray.lean#ZzEvent: neither routed nor named non-presented/.test(m2))
    throw new Error(`unlisted-vocabulary control did not RED: ${m2}`);
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
        pin, 'HEAD'], { stdio: ['ignore', 'pipe', 'pipe'] });
    } catch (e) {
      reasons.push(`pin non raggiungibile da HEAD: ${f}`);
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
    // of HEAD — an orphaned commit is rejected even when locally
    // resolvable, BEFORE any equality masking can hide the reason
    let reachable = false;
    try {
      execFileSync('git', ['-C', lakeRepo, 'merge-base', '--is-ancestor',
        ex.composition.commit, 'HEAD'], { stdio: ['ignore', 'pipe', 'pipe'] });
      reachable = true;
    } catch (e) { /* exit 1: not an ancestor */ }
    if (!reachable)
      reasons.push('pin composizione non raggiungibile da HEAD (commit orfano): ' +
        ex.composition.commit);
    if (resolvedTree !== ex.composition.tree)
      reasons.push(`albero del pin divergente dal dichiarato — dichiarato=${ex.composition.tree.slice(0, 12)}… risolto=${resolvedTree.slice(0, 12)}…`);
    if (ex.composition.commit !== ACCEPTED_COMPOSITION.commit ||
        ex.composition.tree !== ACCEPTED_COMPOSITION.tree)
      reasons.push('pin composizione ≠ composizione accettata');
    // INV-8: the cited module joins the freshness manifest — its blob at the
    // pin must equal its blob at HEAD, so the same silent staleness that let
    // the composition pin drift through #62 REDs from now on
    try {
      assertCompositionModuleFresh(ex.composition.commit, ACCEPTED_COMPOSITION.module, lakeRepo);
    } catch (e) {
      reasons.push(e.message);
    }
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

/* a parentless commit of the accepted composition tree, referenced by
   nothing: resolvable, tree-consistent, never an ancestor of HEAD */
let orphanMemo = null;
function orphanPin() {
  if (orphanMemo) return orphanMemo;
  orphanMemo = execFileSync('git', ['-C', REPO, 'commit-tree', ACCEPTED_COMPOSITION.tree,
    '-m', 'claim-gate selftest: orphan composition pin'], { encoding: 'utf8',
    env: { ...process.env, GIT_AUTHOR_NAME: 'claim-gate', GIT_AUTHOR_EMAIL: 'claim-gate@invalid',
      GIT_COMMITTER_NAME: 'claim-gate', GIT_COMMITTER_EMAIL: 'claim-gate@invalid',
      GIT_AUTHOR_DATE: '2000-01-01T00:00:00Z', GIT_COMMITTER_DATE: '2000-01-01T00:00:00Z' } }).trim();
  return orphanMemo;
}

async function selftest(work) {
  const doc = readFileSync(HTML, 'utf8');

  const mc = await checkMachineCoverage(CORE);
  if (!mc.ok) {
    console.error('SELFTEST RED: copertura macchina non torna GREEN:\n' + mc.reasons.join('\n'));
    return 1;
  }
  try {
    await removedAttemptCaseControl();
    await removedVoteHandlerControl();
    await unhandledVocabularyControl();
    stalePinSelftest();
    staleCompositionPinSelftest();
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
  // every citation at the merged pin is sorry-free today; the flip control
  // therefore flips a PROVATO row to enunciato and expects the same
  // derivation-divergence RED a hand-flipped sorry row produces
  const proved = Object.entries(green.axioms).filter(([, s]) => s === 'provato').map(([d]) => d);
  if (!proved.length) {
    console.error('SELFTEST RED: nessuna dichiarazione provata derivata — il controllo del flip non ha materiale');
    return 1;
  }
  const flipTarget = proved[0];
  console.log(`derivazione fresca: ${proved.length} dichiarazioni provate, 0 enunciate (scarico #48/#65); ` +
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
        const needle = "d: 'step_authorized', f: 'lean/Reactivegas/Invariants.lean', l: 561";
        if (!doc.includes(needle) || doc.split(needle).length !== 2)
          return { ok: false, reasons: ['controllo mal costruito: riga auth non unica'] };
        const p = join(work, 'sab-link-file.html');
        writeFileSync(p, doc.replace(needle,
          "d: 'step_authorized', f: null, l: 561"));
        return runGate({ html: p, work });
      },
    },
    {
      name: 'collegamento citazione senza riga',
      expect: /collegamento citazione senza riga/,
      run: () => {
        const needle = "d: 'step_authorized', f: 'lean/Reactivegas/Invariants.lean', l: 561";
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
        const before = `'${flipTarget}': 'provato'`;
        if (!doc.includes(before)) return { ok: false,
          reasons: [`controllo mal costruito: ${before} assente dal documento`] };
        writeFileSync(p, doc.replace(before, `'${flipTarget}': 'enunciato'`));
        return runGate({ html: p, work });
      },
    },
    {
      // SENSITIVITY: the production classifier must distinguish a
      // sorry-backed report from a sorry-free one, in BOTH directions. With
      // every citation sorry-free at the merged pin, the control feeds a
      // FABRICATED sorryAx report for a real cited declaration through the
      // production deriveAxioms: detector on must classify enunciato,
      // detector off (the broken-detector simulation) must classify provato.
      // Either corruption of the detector (always-false, always-true, hook
      // removed) breaks one direction and fails this control; an unreachable
      // fixture fails it too — never a silent pass.
      name: 'rilevatore sorry: sensibilità on=enunciato off=provato',
      expect: /sensibilità del rilevatore confermata/,
      run: () => {
        const target = Object.entries(green.axioms).find(([, s]) => s === 'provato');
        if (!target) return { ok: false, reasons: ['sensibilità del rilevatore non costruibile: nessuna dichiarazione provata da fabbricare'] };
        const [decl] = target;
        const fake = `'${decl}' depends on axioms: [sorryAx, propext]`;
        process.env.RG_GATE_SORRY_DETECTOR = 'on';
        let on, off;
        try {
          on = deriveAxioms([decl], fake);
          process.env.RG_GATE_SORRY_DETECTOR = 'off';
          off = deriveAxioms([decl], fake);
        } finally { delete process.env.RG_GATE_SORRY_DETECTOR; }
        if (on.derived[decl] === 'enunciato' && off.derived[decl] === 'provato' &&
            off.rawLine[decl].includes('sorryAx'))
          return { ok: false, reasons: [`sensibilità del rilevatore confermata: ${decl} enunciato col rilevatore attivo, provato con sorryAx nel report a rilevatore spento`] };
        return { ok: false, reasons: [`sensibilità del rilevatore ASSENTE: on=${JSON.stringify(on.derived[decl])} off=${JSON.stringify(off.derived[decl])}`] };
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
      name: 'pin orfano risolvibile ma non raggiungibile da HEAD',
      // an orphan made here (git commit-tree of the accepted tree, no parent,
      // no ref): resolvable with a CONSISTENT declared tree in any checkout,
      // so only stable reachability can reject it — no commit that exists in
      // one clone and not another
      expect: () => new RegExp('non raggiungibile da HEAD \\(commit orfano\\): ' +
        orphanPin().slice(0, 10)),
      run: () => {
        const p = join(work, 'sab-comp-orphan.html');
        writeFileSync(p, doc
          .replace(`commit: '${ACCEPTED_COMPOSITION.commit}',`, `commit: '${orphanPin()}',`));
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
    const want = typeof c.expect === 'function' ? c.expect() : c.expect;
    if (!want.test(text)) {
      console.error(`SELFTEST RED: «${c.name}» fallito per il motivo sbagliato:\n${text.slice(0, 300)}`);
      return 1;
    }
    console.log(`controllo negativo «${c.name}»: RED come atteso — ${text.split('\n')[0].slice(0, 110)}`);
  }
  const branch = await leanBranchControls(work);
  if (branch.length) {
    console.error('SELFTEST RED: controlli del ramo Lean:\n' + branch.join('\n'));
    return 1;
  }
  console.log(`selftest GREEN: ${controls.length} controlli negativi RED per il motivo atteso; ` +
    `produzione GREEN (${green.rows} righe, ${green.cited} citazioni, ` +
    `${green.enun} enunciate, sha ${green.sha.slice(0, 12)}…); ` +
    `machine-controls=${MACHINE_CONTROLS}`);
  return 0;
}

/* --- a Lean-changing branch, judged as the checkout under test ------------ */

/* The cited source the branch controls edit: cited by a manifest row at the
   checkout (not at the composition pin), outside the core manifest, the
   composition module and every event vocabulary — so the receipt's own
   sources/sourcePins are all a branch has to re-make for it. */
const BRANCH_VICTIM = 'lean/Reactivegas/Invariants.lean';
const SELFTEST_IDENT = {
  GIT_AUTHOR_NAME: 'claim-gate', GIT_AUTHOR_EMAIL: 'claim-gate@invalid',
  GIT_COMMITTER_NAME: 'claim-gate', GIT_COMMITTER_EMAIL: 'claim-gate@invalid',
  GIT_AUTHOR_DATE: '2000-01-01T00:00:00Z', GIT_COMMITTER_DATE: '2000-01-01T00:00:00Z',
};

function scratchCommit(dir, paths, message) {
  const git = args => execFileSync('git', ['-C', dir, ...args], { encoding: 'utf8',
    stdio: ['ignore', 'pipe', 'pipe'], env: { ...process.env, ...SELFTEST_IDENT } }).trim();
  git(['add', '--', ...paths]);
  git(['commit', '--quiet', '--no-verify', '-m', message]);
  return git(['rev-parse', 'HEAD']);
}

/* The full claim gate (machine coverage + runGate) with `dir` as the
   checkout under test: its HEAD, its working tree, its lake environment. */
async function gateAt(dir, work) {
  const mc = await checkMachineCoverage(join(dir, 'economics-simulator-core.mjs'), dir);
  const r = runGate({ html: join(dir, 'economics-simulator.html'), sourcesRoot: dir,
    lakeRepo: dir, work });
  return { ok: mc.ok && r.ok, reasons: [...(mc.ok ? [] : mc.reasons), ...(r.ok ? [] : r.reasons)] };
}

/*
 * In a throwaway worktree of HEAD (shared objects and refs, the tracked tree
 * never written) the controls commit Lean edits and judge that worktree as
 * the checkout under test. Each rule the gate applies against HEAD gets a
 * commit only the worktree's HEAD reaches, so a rule still reading
 * origin/master, or this repository's own HEAD, fails here:
 *   - a cited Lean file edited without re-made receipts: RED naming it;
 *   - the core Event source and the composition module edited: RED on both
 *     pin blobs differing from the blobs at HEAD;
 *   - an event vocabulary source edited: RED on its pin blob vs HEAD;
 *   - the receipt's sources/sourcePins re-made against the edit commit: the
 *     whole gate passes, the pin being an ancestor of the worktree's HEAD;
 *   - a source pin moved to a parentless commit of the same tree: RED on
 *     reachability alone;
 *   - the receipt's composition commit moved to the worktree's HEAD: only the
 *     tree mismatch REDs, never reachability;
 *   - the accepted core pin moved to the worktree's HEAD: the core inventory
 *     derives; moved to a parentless commit of that tree: RED on reachability.
 * Returns the list of failed controls (empty when all hold).
 */
async function leanBranchControls(work) {
  const dir = join(work, 'lean-branch');
  const failures = [];
  const red = (name, text) =>
    console.log(`controllo negativo «${name}»: RED come atteso — ${text.split('\n')[0].slice(0, 110)}`);
  execFileSync('git', ['-C', REPO, 'worktree', 'add', '--quiet', '--detach', dir, 'HEAD'],
    { stdio: ['ignore', 'pipe', 'pipe'] });
  try {
    try { cpSync(join(REPO, 'lean', '.lake'), join(dir, 'lean', '.lake'),
      { recursive: true }); } catch { /* cold cache: lake rebuilds */ }
    const git = args => execFileSync('git', ['-C', dir, ...args],
      { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }).trim();
    const appendTo = (f, line) =>
      writeFileSync(join(dir, f), readFileSync(join(dir, f), 'utf8') + '\n' + line + '\n');
    const orphanOf = rev => execFileSync('git', ['-C', dir, 'commit-tree', git(['rev-parse', rev + '^{tree}']),
      '-m', 'claim-gate selftest: orphan pin'],
      { encoding: 'utf8', env: { ...process.env, ...SELFTEST_IDENT } }).trim();
    const htmlPath = join(dir, 'economics-simulator.html');
    const doc = readFileSync(htmlPath, 'utf8');
    const ex = extract(doc);
    const victim = BRANCH_VICTIM;
    const victimPath = join(dir, victim);
    const src = readFileSync(victimPath, 'utf8');
    if (!ex.sources[victim] || !ex.sourcePins[victim] ||
        !ex.rows.some(r => r.f === victim && !r.g) ||
        ACCEPTED_CORE.files.includes(victim) || victim === ACCEPTED_COMPOSITION.module ||
        parseVocabularies(victim, src).length)
      return [`controllo mal costruito: ${victim} non è una sorgente citata fuori da core, composizione e vocabolari`];
    const coreFile = eventSourceFromManifest(ACCEPTED_CORE.files);
    const compFile = ACCEPTED_COMPOSITION.module;
    const vocabFile = pinnedVocabularies(undefined, REPO).map(v => v.file)
      .find(f => !ACCEPTED_CORE.files.includes(f));
    if (!vocabFile) return ['controllo mal costruito: nessun vocabolario scoperto fuori dal manifesto core'];

    appendTo(victim, '-- claim-gate selftest: a Lean-changing branch');
    const edit = scratchCommit(dir, [victim], 'claim-gate selftest: edit a cited Lean file');

    const stale = await gateAt(dir, work);
    const staleText = stale.reasons.join('\n');
    if (stale.ok || !staleText.includes('hash sorgente divergente: ' + victim))
      failures.push(`ramo Lean senza ricevute rifatte non RED sul file ${victim}: ${staleText.slice(0, 300)}`);
    else red('ramo Lean senza ricevute rifatte', staleText);

    // the core Event source and the composition module edited on the branch:
    // their blobs at the accepted pins no longer equal the blobs at HEAD
    for (const f of [coreFile, compFile]) appendTo(f, '-- claim-gate selftest: a pinned module edited on the branch');
    scratchCommit(dir, [coreFile, compFile], 'claim-gate selftest: edit the pinned modules');
    const wantCore = `stale cited file ${coreFile}: pin blob=${git(['rev-parse', ACCEPTED_CORE.commit + ':' + coreFile])} HEAD blob=${git(['rev-parse', 'HEAD:' + coreFile])}`;
    const wantComp = `modulo composizione obsoleto al pin: pin=${git(['rev-parse', ex.composition.commit + ':' + compFile])} HEAD=${git(['rev-parse', 'HEAD:' + compFile])}`;
    const drifted = await gateAt(dir, work);
    const driftText = drifted.reasons.join('\n');
    if (drifted.ok || !driftText.includes(wantCore) || !driftText.includes(wantComp))
      failures.push(`moduli al pin modificati sul ramo non RED contro HEAD — atteso «${wantCore}» e «${wantComp}»: ${driftText.slice(0, 400)}`);
    else red('moduli al pin modificati sul ramo', wantCore);
    git(['checkout', '--quiet', '--detach', edit]);

    // an event vocabulary source edited on the branch: the vocabulary
    // discovery's freshness check judges its pin blob against HEAD
    appendTo(vocabFile, '-- claim-gate selftest: a vocabulary edited on the branch');
    scratchCommit(dir, [vocabFile], 'claim-gate selftest: edit a vocabulary source');
    const wantVocab = `stale cited file ${vocabFile}: pin blob=${git(['rev-parse', ACCEPTED_CORE.commit + ':' + vocabFile])} HEAD blob=${git(['rev-parse', 'HEAD:' + vocabFile])}`;
    const vmc = await checkMachineCoverage(join(dir, 'economics-simulator-core.mjs'), dir);
    const vocabText = (vmc.reasons || []).join('\n');
    if (vmc.ok || !vocabText.includes(wantVocab))
      failures.push(`vocabolario modificato sul ramo non RED contro HEAD — atteso «${wantVocab}»: ${vocabText.slice(0, 300)}`);
    else red('vocabolario modificato sul ramo', wantVocab);
    git(['checkout', '--quiet', '--detach', edit]);

    const srcNeedle = `'${victim}': '${ex.sources[victim]}',`;
    const pinNeedle = `'${victim}': '${ex.sourcePins[victim]}',`;
    if (doc.split(srcNeedle).length !== 2 || doc.split(pinNeedle).length !== 2)
      return [...failures, `controllo mal costruito: voce di ricevuta per ${victim} non unica`];
    writeFileSync(htmlPath, doc.replace(srcNeedle, `'${victim}': '${sha256(readFileSync(victimPath))}',`)
      .replace(pinNeedle, `'${victim}': '${edit}',`));
    const remadeHead = scratchCommit(dir, ['economics-simulator.html'], 'claim-gate selftest: re-make the receipt');
    const remadeDoc = readFileSync(htmlPath, 'utf8');

    const remade = await gateAt(dir, work);
    if (!remade.ok)
      failures.push(`ramo Lean con ricevute rifatte RESPINTO: ${remade.reasons.join('\n').slice(0, 400)}`);
    else console.log(`controllo positivo «ramo Lean con ricevute rifatte»: GREEN — ${victim} pinnato a ${edit.slice(0, 10)}…`);

    const sourceOrphan = orphanOf('HEAD');
    const orphanHtml = join(work, 'sab-source-orphan.html');
    writeFileSync(orphanHtml, remadeDoc.replace(`'${victim}': '${edit}',`, `'${victim}': '${sourceOrphan}',`));
    const orphaned = runGate({ html: orphanHtml, sourcesRoot: dir, lakeRepo: dir, work });
    const orphanText = (orphaned.reasons || []).join('\n');
    if (orphaned.ok || !orphanText.includes('pin non raggiungibile da HEAD: ' + victim))
      failures.push(`pin sorgente orfano non RED per raggiungibilità: ${orphanText.slice(0, 300)}`);
    else red('pin sorgente orfano', orphanText);

    // the receipt's composition commit moved to the worktree's HEAD: an
    // ancestor of the checkout under test, so only its tree may RED
    const compNeedle = `commit: '${ACCEPTED_COMPOSITION.commit}',`;
    if (remadeDoc.split(compNeedle).length !== 2)
      return [...failures, 'controllo mal costruito: commit composizione non unico nella ricevuta'];
    const compHtml = join(work, 'sab-comp-branch.html');
    writeFileSync(compHtml, remadeDoc.replace(compNeedle, `commit: '${remadeHead}',`));
    const moved = runGate({ html: compHtml, sourcesRoot: dir, lakeRepo: dir, work });
    const movedText = (moved.reasons || []).join('\n');
    if (moved.ok || !movedText.includes('albero del pin divergente dal dichiarato') ||
        movedText.includes('pin composizione non raggiungibile'))
      failures.push(`pin composizione sul ramo giudicato fuori da HEAD: ${movedText.slice(0, 300)}`);
    else red('pin composizione sul ramo (solo albero)', movedText);

    // the accepted core pin moved to the worktree's HEAD derives the
    // inventory; moved to a parentless commit of that tree it REDs
    const savedCore = { commit: ACCEPTED_CORE.commit, tree: ACCEPTED_CORE.tree };
    const coreOrphan = orphanOf('HEAD');
    try {
      ACCEPTED_CORE.commit = remadeHead;
      ACCEPTED_CORE.tree = git(['rev-parse', 'HEAD^{tree}']);
      try {
        const ctors = pinnedConstructors(dir);
        if (!ctors.length) failures.push('pin core sul ramo: inventario vuoto');
        else console.log(`controllo positivo «pin core sul ramo»: GREEN — ${ctors.length} costruttori derivati`);
      } catch (e) { failures.push(`pin core sul ramo RESPINTO: ${e.message.slice(0, 300)}`); }
      ACCEPTED_CORE.commit = coreOrphan;
      let coreText = '';
      try { pinnedConstructors(dir); } catch (e) { coreText = e.message; }
      if (!coreText.includes('pin core non raggiungibile da HEAD (commit orfano): ' + coreOrphan))
        failures.push(`pin core orfano non RED per raggiungibilità: ${coreText.slice(0, 300)}`);
      else red('pin core orfano', coreText);
    } finally {
      ACCEPTED_CORE.commit = savedCore.commit;
      ACCEPTED_CORE.tree = savedCore.tree;
    }
    return failures;
  } finally {
    try { execFileSync('git', ['-C', REPO, 'worktree', 'remove', '--force', dir],
      { stdio: ['ignore', 'pipe', 'pipe'] }); } catch { /* best effort */ }
    rmQuiet(dir);
  }
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
    const r = runGate({ work });
    if (!mc.ok || !r.ok) {
      const all = [...(mc.ok ? [] : mc.reasons), ...(r.ok ? [] : r.reasons)];
      console.error(`RED: ${all.length} problemi`);
      all.forEach(x => console.error(' - ' + x));
      code = 1;
    } else {
        console.log(`GREEN: ${r.rows} righe di manifesto, ${r.cited} citazioni verificate nel ` +
          `lake env; stati derivati (${r.enun} enunciate, ${r.cited - r.enun} provate); ` +
          `file:line risolti; hash sorgenti confermati; copertura KelGroups esaustiva ` +
          `(${r.kelPins} sorgenti scoperte = pinnate); pin composizione ` +
          `${ACCEPTED_COMPOSITION.commit.slice(0, 10)}… verificato (albero esatto, righe al pin, ` +
          `instradamento derivato, copertura dei costruttori, stati elaborati freschi); ` +
          `ricevuta legata (sha ${r.sha.slice(0, 12)}…); ` +
          `machine=${mc.executable}/${mc.executable} pinned=${mc.pinned} retired=0 ` +
          `vocabularies=${mc.vocabularies} witnessed=${mc.witnessed}`);
        code = 0;
    }
  }
} finally {
  rmQuiet(work);
}
process.exit(code);
