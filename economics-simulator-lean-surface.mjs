/*
 * economics-simulator-lean-surface.mjs — the one discovery of the Lean event
 * surface the simulator can route.
 *
 * Every pinned event vocabulary is DISCOVERED at the core pin: each inductive
 * under lean/Reactivegas and lean/KelGroups whose name ends in Event, Command,
 * Proposal or Mutation, with its constructors parsed from the declaration —
 * never a list of names. The claim gate (witnesses per constructor) and the
 * scenario generator (productions per constructor) both import this module, so
 * the extent they judge is the same by construction.
 *
 * The core pin is derived from the history of the checkout under test: the
 * last commit reachable from HEAD that changes a path under the vocabulary
 * roots. A truncated history would name its boundary commit as that change, so
 * it is refused. Reads git history only.
 *
 * RG_SURFACE_EXTRA (a JSON object { file: leanSource }) adds sources to the
 * extent, for the controls that show every consumer follows this module.
 */

import { execFileSync } from 'node:child_process';
import { dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const REPO = dirname(fileURLToPath(import.meta.url));

export const VOCAB_ROOTS = ['lean/Reactivegas', 'lean/KelGroups'];

export function gitShow(revPath, repo = REPO) {
  return execFileSync('git', ['-C', repo, 'rev-parse', revPath],
    { encoding: 'utf8' }).trim();
}

export function deriveCorePin(repo, head) {
  const git = args => execFileSync('git', ['-C', repo, ...args],
    { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }).trim();
  const refuse = cause => new Error(`core pin: cannot derive from the history of ${head}: ${cause}`);
  try { git(['rev-parse', '--verify', '--quiet', head + '^{commit}']); }
  catch { throw refuse('unresolvable revision'); }
  if (git(['rev-parse', '--is-shallow-repository']) === 'true')
    throw refuse('truncated history (shallow checkout)');
  const commit = git(['log', '-1', '--format=%H', head, '--', ...VOCAB_ROOTS]);
  if (!commit) throw refuse('no commit changes ' + VOCAB_ROOTS.join(', '));
  return { commit, tree: git(['rev-parse', commit + '^{tree}']) };
}

export function assertCorePinReachable(commit, repo = REPO) {
  try {
    execFileSync('git', ['-C', repo, 'merge-base', '--is-ancestor',
      commit, 'HEAD'], { stdio: ['ignore', 'pipe', 'pipe'] });
  } catch (e) {
    throw new Error('pin core non raggiungibile da HEAD (commit orfano): ' + commit);
  }
}

export function assertCitedFilesFresh(pin, files, repo = REPO) {
  for (const file of files) {
    const pinnedBlob = gitShow(`${pin}:${file}`, repo);
    const headBlob = gitShow(`HEAD:${file}`, repo);
    if (pinnedBlob !== headBlob)
      throw new Error(`stale cited file ${file}: pin blob=${pinnedBlob} HEAD blob=${headBlob}`);
  }
}

const VOCAB_RE = /^inductive ([A-Za-z]*(?:Event|Command|Proposal|Mutation))\b[^\n]*\bwhere\b/gm;

export function parseVocabularies(file, src) {
  const out = [];
  for (const m of src.matchAll(VOCAB_RE)) {
    const rest = src.slice(m.index + m[0].length);
    const end = rest.search(/^(deriving|\S)/m);
    const body = end < 0 ? rest : rest.slice(0, end);
    const constructors = [...body.matchAll(/^\s*\|\s+([A-Za-z][A-Za-z0-9_]*)\b/gm)].map(c => c[1]);
    out.push({ key: `${file}#${m[1]}`, vocabulary: m[1], file, constructors });
  }
  return out;
}

/* discoverVocabularies(repo, pin) -> [{ key, vocabulary, file, constructors }]:
   the Lean source at the pin, each file holding a vocabulary checked fresh
   against HEAD */
export function discoverVocabularies(repo = REPO, pin = deriveCorePin(repo, 'HEAD').commit) {
  const files = execFileSync('git', ['-C', repo, 'ls-tree', '-r', '--name-only',
    pin, '--', ...VOCAB_ROOTS], { encoding: 'utf8' })
    .split('\n').filter(f => f.endsWith('.lean')).sort();
  if (!files.length) throw new Error('vocabulary discovery: no Lean source at the pin');
  const vocab = [];
  for (const file of files) {
    const src = execFileSync('git', ['-C', repo, 'show', pin + ':' + file],
      { encoding: 'utf8' });
    const found = parseVocabularies(file, src);
    if (found.length) assertCitedFilesFresh(pin, [file], repo);
    vocab.push(...found);
  }
  for (const [file, src] of Object.entries(JSON.parse(process.env.RG_SURFACE_EXTRA || '{}')))
    vocab.push(...parseVocabularies(file, src));
  if (!vocab.length) throw new Error('vocabulary discovery: empty extent');
  return vocab;
}

/* Discovered vocabularies the simulator never presents, each with its reason:
   the extent a consumer reports instead of skipping. */
export const NOT_PRESENTED_VOCABULARIES = {
  'lean/KelGroups/Event.lean#Proposal':
    'historical substrate proposal (introduceMember) kept as #54 evidence; Reactivegas.apply takes Reactivegas.Proposal, so it cannot reach the simulator',
  'lean/KelGroups/Event.lean#BaseEvent':
    'pre-integration substrate fold vocabulary over KelGroups.Proposal; the production root carries propose/approve inside IntegratedEvent',
  'lean/KelGroups/Event.lean#GroupEvent':
    'generic pre-integration group fold event; superseded on the production root by IntegratedEvent',
  'lean/KelGroups/Event.lean#BaseMutation':
    'substrate effect computed from Reactivegas.Proposal by proposalMutation; never a signed event',
};
