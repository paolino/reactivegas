# Plan — #86 exporter successor

Single OWNER slice S86 (LIGHT ineligible: CI-wiring entailment, wrapper-value
semantics, refusal-without-write, and additive-only scope need semantic
judgment no pure gate entails). `draft=NONE`. Ticket owner `muse`, commit
owner `muse` (operator-suspended alternation), auditor `codex`|`grok` FULL
per candidate (never `muse`/`glm`/`claude`).

## Fixed decisions

Start from inherited `9c8756a` (descendant of `4a6cd87` via `bc44998`), new
branch `feat/86-exporter-successor`. Repairs ride as uncommitted work on that
base; emitter bytes stay identical (spec fence) so the corpus files +
manifest do not move. Format + gate + apparatus change; content does not.

Order: (1) freeze mandate + immutable gate with per-row RED on the unrepaired
tree (A–E RED, 4 inherited GREEN expected); (2) dispatch commit owner with
the frozen packet; (3) submission → fresh FULL auditor; (4) one
findings-driven repair max, then fresh FULL re-audit; (5) accept → stamp →
`just ci` GREEN → draft PR (provisional body, no merge/comments).

## Source horizon and fence

Owned (additive unless named): `lean/Reactivegas/CorpusExport.lean`,
`lean/corpus/*`, `lean/lakefile.lean`, corpus recipes in `justfile`,
`.github/workflows/ci.yaml` additively, dev-shell tool decl under `nix/`,
`handoffs/CORPUS-COVERAGE.md` (ticket root, routed to #71), task stamps here.

Forbidden: any Lean theorem/guard/`step`/`stepEvent`/`appFold`/`baseHook`/
state type/`Trace`/`reactivegas.trace/v1`; existing corpus content,
`seedView`, `corpusInitial`, `seedAuth`; `docs/en/design/` (#71);
`paolino/kelgroups` implementation. Crossing → stop + `questions/Q-NNN`.

## Slice S86 — five repairs + four re-established rows

Delivers R86-A…R86-F and tasks T8600…T8606. Commit owner binds traces/steps
+ view/initial/both-auths to live defs (bounded call-site claim), fixes
`check`-one-path fallthrough (usage exit 1, no writes), wires CI verifier
additively, declares `jq`, corrects coverage handoff with dated history.
Auditor scope: entire unaccepted exporter vs `4a6cd87`, all 9 rows open.

Frozen gate rows: `G86-A-CI-PATH`, `G86-B-JQ-DECLARED`, `G86-C-CONTEXT-BOUND`,
`G86-D-ARITY-REFUSES-NOWRITE`, `G86-E-COVERAGE-CURRENT`,
`G74-CALLS-EXISTING`, `G74-ENVELOPE-CLOSED`, `G74-VERIFY-FAILS-CLOSED`,
`G74-ADDITIVE-ONLY`.
