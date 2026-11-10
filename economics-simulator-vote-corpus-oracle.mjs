/*
 * economics-simulator-vote-corpus-oracle.mjs — replay of the Lean vote corpus
 * (lean/corpus/vote.json) through the simulator's integrated root, with the
 * threshold table of its sidecar (lean/corpus/vote.context.json) as the only
 * policy.
 *
 *   validateContext(contextBytes, corpusBytes)  pre-replay checks of the sidecar
 *   tableThreshold(table)                       n -> threshold, aborting outside
 *                                               the table's domain at the query
 *   replayCorpus(corpus, theta, core)           one core call per step, compared
 *                                               with what Lean recorded
 *   verifyCorpus(corpusBytes, contextBytes)     the two, in order
 *
 * An outcome is one of four tagged values: PRE-REPLAY REFUSAL (the sidecar was
 * refused before any step), RUNTIME ABORT (a threshold was queried outside the
 * table's domain), CONFORMANCE MISMATCH (a step disagreed) or PASS. The sidecar's
 * label, source and generatedFrom are provenance and are never read.
 */

import { createHash } from 'node:crypto';
import * as defaultCore from './economics-simulator-core.mjs';

export const TAGS = { refusal: 'PRE-REPLAY REFUSAL', abort: 'RUNTIME ABORT',
  mismatch: 'CONFORMANCE MISMATCH', pass: 'PASS' };

const SCHEMA = 'reactivegas.replay-context';
const VERSION = 1;
const AUTH_SEMANTICS = ['always-refuse'];

/* a threshold queried at an n the table does not cover */
export class DomainAbort extends Error {
  constructor(n, domain) {
    super(`threshold queried at n=${n}, outside the table's domain {0..${domain}}`);
    this.n = n;
    this.domain = domain;
  }
}

/* an event the corpus encodes in a form the transcription has no reading for */
class EncodingError extends Error {}

const sha256 = b => createHash('sha256').update(b).digest('hex');
const refuse = (rules, detail) => ({ refusal: { rules, detail } });
const INTEGER_TOKEN = /^(0|[1-9][0-9]*)$/;

/* the sidecar before replay: schema, version, the digest of the corpus bytes,
   the canonical table rules 1-5 (every violated rule is named) and the
   backdonateAuth semantics token. Numbers keep their source text, so
   a float spelled as an integer value is still a float. */
export function validateContext(contextBytes, corpusBytes) {
  let ctx;
  try {
    ctx = JSON.parse(Buffer.from(contextBytes).toString('utf8'),
      (_, v, c) => typeof v === 'number' ? { number: v, source: c.source } : v);
  } catch (e) { return refuse(['json'], 'the sidecar is not JSON: ' + e.message); }
  const plain = v => (v !== null && typeof v === 'object' && 'number' in v && 'source' in v) ? v.number : v;
  const at = (o, ...path) => path.reduce((x, k) => (x !== null && typeof x === 'object') ? x[k] : undefined, o);
  if (at(ctx, 'schema') !== SCHEMA) return refuse(['schema'], `unrecognised schema ${JSON.stringify(plain(at(ctx, 'schema')))}`);
  if (plain(at(ctx, 'version')) !== VERSION) return refuse(['version'], `unrecognised version ${JSON.stringify(plain(at(ctx, 'version')))}`);
  const stated = at(ctx, 'corpus', 'contentSha256'), actual = sha256(Buffer.from(corpusBytes));
  if (stated !== actual) return refuse(['digest'], `corpus.contentSha256 ${stated} is not the SHA-256 of the corpus bytes ${actual}`);
  const table = at(ctx, 'policies', 'threshold', 'table');
  if (!Array.isArray(table)) return refuse(['table-rule-1'], 'policies.threshold.table is not a list of [key, value] pairs');
  const exact = x => x !== null && typeof x === 'object' && INTEGER_TOKEN.test(x.source) && Number.isSafeInteger(x.number);
  const rules = [];
  const pairs = table.every(p => Array.isArray(p) && p.length === 2);
  if (!pairs || !table.every(([k, v]) => exact(k) && exact(v))) rules.push('table-rule-1');
  if (pairs) {
    const keys = table.map(([k]) => plain(k)).filter(Number.isInteger);
    if (!keys.every((k, i) => i === 0 || keys[i - 1] < k)) rules.push('table-rule-2');
    const set = new Set(keys);
    if (!set.size || !Array.from({ length: Math.max(...set) + 1 }, (_, i) => i).every(i => set.has(i)))
      rules.push('table-rule-3');
    if (set.size !== keys.length) rules.push('table-rule-4');
  }
  if (!table.length) rules.push('table-rule-5');
  if (rules.length) return refuse(rules, 'the threshold table breaks the replay-context table rules');
  const semantics = at(ctx, 'policies', 'backdonateAuth', 'semantics');
  if (!AUTH_SEMANTICS.includes(semantics))
    return refuse(['semantics'], `unknown backdonateAuth.semantics ${JSON.stringify(plain(semantics))}`);
  return { context: { table: table.map(([k, v]) => [plain(k), plain(v)]), semantics } };
}

/* the table as a threshold function. The domain is {0..N}; an n outside it
   aborts where it is asked, and every n asked inside it is logged. */
export function tableThreshold(table) {
  const values = new Map(table);
  const domain = table.length - 1;
  const theta = n => {
    if (!values.has(n)) throw new DomainAbort(n, domain);
    theta.queries.push(n);
    return values.get(n);
  };
  theta.queries = [];
  return theta;
}

/* corpus event -> the integrated root's event; the only translation is encoding */
function coreEvent(e) {
  if (e.tag === 'direct') return { direct: e.command };
  if (e.tag === 'approve') return { approve: { proposalId: e.proposalId } };
  if (e.tag === 'app') return { app: e.event };
  if (e.tag === 'propose') {
    const p = e.proposal;
    if ('departure' in p) return { propose: { proposal: { departure: p.departure.key } } };
    if ('changeRoles' in p) return { propose: { proposal: p } };
  }
  throw new EncodingError('event not readable by the transcription: ' + JSON.stringify(e).slice(0, 120));
}

const sorted = v => JSON.stringify(v, (_, x) => x !== null && typeof x === 'object' && !Array.isArray(x)
  ? Object.fromEntries(Object.keys(x).sort().map(k => [k, x[k]])) : x);

/* the pending base mutations in the corpus's own encoding */
const pendingOf = pending => pending.map(([id, e]) => [id,
  { approvals: e.approvals, mutation: 'departure' in e.proposal
      ? { removeMember: { key: e.proposal.departure } } : e.proposal,
    proposer: e.proposer }]);

const changeOf = c => (c === null || c === undefined) ? null
  : Object.fromEntries(Object.entries(c).map(([k, v]) => [k, typeof v === 'string' ? { key: v } : v]));

/* the vote validator's error name, when the refusal came from it */
const voteErrorOf = det => {
  for (const f of det.failed || []) {
    const m = /validateVoteEvent = \.error \.(\w+)/.exec(f.code);
    if (m) return m[1];
  }
  return null;
};

export function replayCorpus(corpus, theta, core = defaultCore) {
  const queries = [];
  const mismatch = (step, field, expected, actual) =>
    ({ tag: TAGS.mismatch, step, field, expected, actual });
  const init = corpus.initial;
  if ((init.pendingBase || []).length || (init.pendingProposals || []).length)
    return mismatch(-1, 'initial', [], init.pendingBase);
  let gs = { members: init.members, pendingBase: [], appFold: init.appFold };
  for (let i = 0; i < corpus.steps.length; i++) {
    const s = corpus.steps[i];
    const asked = theta.queries ? theta.queries.length : 0;
    let det;
    try { det = core.applyIntegrated(gs, s.signer, coreEvent(s.event), theta); }
    catch (e) {
      if (e instanceof DomainAbort)
        return { tag: TAGS.abort, n: e.n, domain: e.domain, step: i, checkedPrefix: i };
      if (e instanceof EncodingError) return mismatch(i, 'event', s.event, e.message);
      throw e;
    }
    if (theta.queries) for (const n of theta.queries.slice(asked)) queries.push({ step: i, n });
    const accepted = !det.refused;
    if (accepted !== s.accepted) return mismatch(i, 'accepted', s.accepted, accepted);
    const next = accepted ? det.gs : gs;
    if (core.canonAggregate(next) !== core.canonAggregate(s.state) ||
        sorted(pendingOf(next.pendingBase)) !== sorted(s.state.pendingBase) ||
        (s.state.pendingProposals || []).length)
      return mismatch(i, 'state', core.canonAggregate(s.state), core.canonAggregate(next));
    if (sorted(changeOf(accepted ? det.change : null)) !== sorted(changeOf(s.change)))
      return mismatch(i, 'change', s.change, accepted ? det.change : null);
    const verr = accepted ? null : voteErrorOf(det);
    if (verr !== s.voteError) return mismatch(i, 'voteError', s.voteError, verr);
    gs = next;
  }
  return { tag: TAGS.pass, steps: corpus.steps.length, queries };
}

export function verifyCorpus(corpusBytes, contextBytes, core = defaultCore) {
  const v = validateContext(contextBytes, corpusBytes);
  if (v.refusal) return { tag: TAGS.refusal, ...v.refusal };
  return replayCorpus(JSON.parse(Buffer.from(corpusBytes).toString('utf8')),
    tableThreshold(v.context.table), core);
}
