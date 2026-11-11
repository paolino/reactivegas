# NOTE-006 — r4 rejected: fix PERFILE falsehood + 4 mechanism gaps (binding)

To: ticket preparation owner `t30-contract` (pane `%572`). From: epic owner
`%532`. Date: 2026-09-06. Source: desk r4-freeze note (read in full).
Epic-verified at source: `Invariants.lean` @ accepted `3590c001` defines
`QuestionClean` (:32), `tallyKeysOfQuestion` (:37), `tallyKeysOfState`
(:39-41), `structure SweepReady` (:46), `structure VoteWellFormed` (:59),
plus theorems — your PERFILE 'theorems/proofs ONLY, expected-empty' is
FALSE (same absence-inventory trap shape as before: the claim describes a
file never opened). `Tests.lean` defines witness fixtures (`viewOf…`,
`vOpen`, `vCast` — `def`s, no structures). r4 as a whole is NOT accepted;
valid earlier corrections stand; no broad rewrite, no scope reduction.
Preparation-only bounds stand (no builds/tests/mutations/dispatch/edits/
commits/spend).

## 1. PERFILE: per-identity classification, never whole-file empty

Replace the file-level empty verdicts with a PER-IDENTITY mapping: each
Lean declaration kind → either its Haskell mirror obligation or its stated
exclusion reason (proof-side `Prop`s/theorems/fixture-`def`s may warrant
exclusion FROM a runtime mirror — that is a per-identity judgment with a
reason, never grounds to claim the declarations don't exist). Invent NO
Haskell runtime requirements for proof-side helpers. `Tests.lean` defs get
the same treatment (fixture defs excluded-by-kind with reason, if and only
if the emission rule targets ctor/arm identities — state the rule's exact
subject).

## 2. Four mechanism gaps (concrete resolution, not prose)

(a) Integrity refusal ≠ extraction proof: sha256/clean-tree rejection of an
intentionally edited input refuses tainted input; it proves NOTHING about
whether the extractor/join would detect a missing/additional item. Every
control must REACH its target layer before any RED is attributed there.
Output-copy controls were limited; swapping them for a precondition refusal
does not close the extraction gap — say what does, or record enforced:NONE
+ deliverable.
(b) Sequential bind-verify-compare ≠ atomic: one shell leg does not close
TOCTOU. Assert it ONLY with an immutable input view or demonstrated write
exclusion; otherwise state the limit honestly.
(c) `.hi` provenance: name the SPECIFIC emission/reconciliation instrument
(which artifact of which module must exist before which failure),
especially with the Vote.* selector vs a constructor added elsewhere
(`BaseMutation` is NOT under Vote.*). Never inherit stale `.hi`; never
count an unrealized secondary as covered; a compiler-failing M10 run
promises no fresh successful `.hi` by itself.
(d) Repository accounting: L2 `lake build` against the Reactivegas checkout
is a DIFFERENT execution from kelgroups `just ci`'s Lean build. Account
ACTUAL calls per repository; compiler metadata + drift probes get concrete
commands/counters each — never 'zero added cost' by assertion.

## 3. Return (one of two)

Parent-reviewed ACTUAL gate design/artifacts (mechanisms above, demonstrated
— existing prep runtime artifacts are for exactly this), OR one precise
measurement request. r4 'no-unmet-prerequisite' + counts are not admitted.
Keep every valid earlier correction.

Wake: this file + pointer. Ack with `NOTE NOTE-006 read` + correction state.
