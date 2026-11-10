# Spec — #75 integrated vote corpus through the production root

Class: **changes behaviour** (new frozen corpus, new replay context and new
conformance claims). No model semantics change. Topology: OWNER.
Base: `origin/master` `222ad9a`. Re-freeze after #69 lands (merge order #69 → #75).

## User stories

- **US-vote-corpus.** A replayer outside Lean gets one frozen file whose signed
  vote events went through `Reactivegas.apply`'s `.app` route over
  `GroupState State`, with the post-state of every step.
- **US-replay-context.** The same replayer gets a sidecar naming the threshold
  as a finite table, so it can replay without Lean and can tell a bad context,
  a too-small domain and a real disagreement apart.
- **US-honest-inputs.** A reviewer can trust that the corpus was generated from
  committed inputs only, and that an input the projection forgot is caught.

## Requirements (stable names)

- **vote-route-observable (V1).** At least one accepted step is `.app` carrying
  a vote-routed `AppEvent`, applied by `Reactivegas.apply` over `GroupState State`.
- **closure-with-economics (V2).** At least one step's post-state carries a new
  closure record *and* an economic effect of that closure (authorization minted
  and spent, or refund on negative closure) in the same file.
- **franchise-closure-while-active (V3).** A `franchiseChange` closure happens
  while the payload is economically active (nonzero balances or an open
  collection with accepted pledges).
- **journey-lifted.** Every `VOTE_TRACES_V1` journey item listed in the issue
  appears as steps through the production root; the proposer's renounce now
  closes negative (V-5), not a no-op.
- **lifecycle-rows.** Proposer renounce → closed `.negative` cause `renounced`;
  proposer departure → closed `.negative` cause `proposerDeparted`; both with
  their records in the stored state.
- **coverage-over-discovered-extent.** Every constructor of `VoteEvent`,
  `VoteError`, `ClosureCause` and `Verdict`, plus `AppEvent.openBound`, appears
  in the corpus. The constructor lists are read from the inductive declarations
  in the Lean environment, never hand-written. Adding a constructor to any of
  these fails the gate.
- **vote-error-identity-observed.** At the production root every vote refusal
  is `StepError.rejected`; the identity is not observable there. Each refused
  vote step therefore records the `VoteError` that
  `KelGroups.Vote.applyVoteEventChecked` returns on the exact arguments
  `voteApply` receives for that step. A refused step with no vote-machine error
  (e.g. `openBound` not bindable) records none. No `VoteError` or `StepError`
  change.
- **frozen-and-verified.** `just lean-corpus-verify` re-emits the third file and
  its sidecar to a temp dir, `cmp`s both, `sha256sum -c` over the extended
  manifest, live-binds the steps element for element, and checks exact key sets.
- **byte-control-in-gate.** Inside the verify recipe: mutate one byte of the
  committed vote corpus copy → non-zero; restore byte-identical → zero.
- **replay-context (R3.1).** Sidecar `lean/corpus/vote.context.json`, schema
  and bytes per R3.1 §5. Threshold `table` is the sole replay authority;
  `label`/`source`/`generatedFrom` have no operative force.
- **three-outcomes.** The replayer reports PRE-REPLAY REFUSAL, RUNTIME ABORT
  (domain, at the point of query, with the queried `n`) and CONFORMANCE MISMATCH
  distinguishably (distinct exit codes and tagged output). An abort makes no
  full-conformance claim; a prefix report is labelled as a prefix.
- **policy-rederived.** The gate evaluates the named live declaration at every
  domain point through a path independent of the sidecar writer and compares
  with the sidecar *bytes*. Provenance fields are never evidence.
- **policy-witness-precondition.** Before any policy control, the gate runs
  both `legacyThreshold` and `zeroThreshold` tables over the corpus and exhibits
  the first step whose outcome differs, at a queried `n >= 1`. Absent → the
  gate fails naming the corpus inadequate.
- **context-controls.** R3.1 §7 rows 1–9 in the gate, each failing row shown
  failing; row 4 truncates to a valid prefix below the witnessed `n` and must
  pass validation then abort at exactly `n`.
- **source-input-digest.** `generatedFrom.sourceInputDigest` per the encoding in
  `data-model.md`; recomputed independently by the gate; projected paths must
  equal their committed bytes or the gate fails.
- **added-input-caught.** Generation from a tree holding only the projection
  (clean environment) reproduces the committed bytes; a control that adds a
  generation input outside the projection, with the projection path set
  unchanged, makes the gate fail; restoring makes it pass.

## Rejections and non-goals

No semantics change, no new `VoteError`/`ClosureCause`/`StepError` constructor.
`economic.json`/`integrated.json` bytes unchanged and they get no sidecar here
(residual). No Haskell vote replayer (none exists; #70 is downstream). Not the
composition. No shipped threshold default: the corpus binds `legacyThreshold`
as test input only (ruling 2026-10-01). `docs/en/design` untouched (#71).
