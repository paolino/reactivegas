/*
 * economics-simulator-generator-shrink.mjs — the search that cuts a violating
 * history down. It knows nothing of the core, the laws or the events: it is
 * handed `accepts(steps)`, the generator gate's own replay through the
 * production core, and keeps a smaller candidate only when `accepts` does.
 *
 * Moves, in order, until none applies:
 *   1. remove a chunk of steps before the last one (halves down to one);
 *   2. replace an integer argument by a smaller one (1, half, one less).
 * The last step is never removed: it is the step that violates. The search is
 * deterministic — no PRNG, a fixed move order.
 */

const clone = x => JSON.parse(JSON.stringify(x));

/* every integer greater than one inside an event, as a path of keys */
function leaves(value, path = []) {
  if (Number.isInteger(value)) return value > 1 ? [path] : [];
  if (value && typeof value === 'object')
    return Object.entries(value).flatMap(([k, v]) => leaves(v, [...path, k]));
  return [];
}
const at = (value, path) => path.reduce((x, k) => x[k], value);
function withLeaf(steps, index, path, to) {
  const out = clone(steps);
  const parent = at(out[index].ev, path.slice(0, -1));
  parent[path[path.length - 1]] = to;
  return out;
}

function removeChunks(steps, accepts) {
  let cur = steps;
  for (let size = Math.max(1, Math.floor((cur.length - 1) / 2)); ; size = Math.max(1, Math.floor(size / 2))) {
    for (let from = 0; from + size <= cur.length - 1;) {
      const cand = cur.slice(0, from).concat(cur.slice(from + size));
      if (accepts(cand)) cur = cand; else from += size;
    }
    if (size === 1) return cur;
  }
}

function simplifyArguments(steps, accepts) {
  let cur = steps;
  for (let i = 0; i < cur.length; i++)
    for (const path of leaves(cur[i].ev)) {
      for (let moved = true; moved;) {
        moved = false;
        const v = at(cur[i].ev, path);
        for (const to of [...new Set([1, Math.floor(v / 2), v - 1])].filter(x => x >= 1 && x < v)) {
          const cand = withLeaf(cur, i, path, to);
          if (accepts(cand)) { cur = cand; moved = true; break; }
        }
      }
    }
  return cur;
}

export function shrinkSteps(steps, accepts) {
  let cur = steps;
  for (;;) {
    const next = simplifyArguments(removeChunks(cur, accepts), accepts);
    if (JSON.stringify(next) === JSON.stringify(cur)) return cur;
    cur = next;
  }
}

/* the first single move that still passes `accepts`, as a sentence, or null:
   dropping one step before the last, or an integer argument set to one. Judged
   apart from the moves the search makes, so a search that skips a move cannot
   pass its own check. */
export function firstImprovement(steps, accepts) {
  for (let i = 0; i < steps.length - 1; i++)
    if (accepts(steps.slice(0, i).concat(steps.slice(i + 1)))) return `removing step ${i} keeps the violation`;
  for (let i = 0; i < steps.length; i++)
    for (const path of leaves(steps[i].ev))
      if (accepts(withLeaf(steps, i, path, 1))) return `argument ${path.join('.')} of step ${i} can be 1`;
  return null;
}
