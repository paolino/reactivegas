# Lean clarity record

What the Lean under `lean/` does not let a reader decide on its own, where a doc
comment, a design page or a ruling disagrees with a definition, and how a future
clarity measurement must be run. Every entry cites the artifact it is read from;
nothing here is a reconstruction.

## The historical clarity experiment: VOID

The clarity measurement owed by issue #66 asks what a fresh reader can decide
from the Lean alone. No such measurement exists for the current model, and none
can be reconstructed: the simulator that would have been its reader was built
with explanation from the authors, not from the Lean alone (issue #66: "Our
simulator was built with explanation, not from the Lean alone, so
`LEAN-CLARITY.md` cannot be reconstructed honestly"). Its outcomes therefore say
nothing about what the Lean conveys, and they are not recorded here as results.

The void covers that historical experiment only. It does not forbid a future
measurement (protocol below) and it does not prevent recording ambiguities that
are already known (next section).

## Known ambiguities

Each row: what is said, what the definition does, and where both are read.

| # | prose | definition | evidence |
|---|---|---|---|
| A-1 | `Reactivegas.TraceTests` says the guard sample "really does cover all eighteen identities", that the reconciliation "stays true whichever way the 8/10 split moves", and that "All eighteen are listed". | `GuardId` has fourteen constructors, one per `Event` constructor; the four membership/role constructors left `Event` with T6222 (`Reactivegas/Types.lean`: "The fourteen surviving economic constructors"). No eighteen-identity vocabulary or 8/10 split exists in the model. | `lean/Reactivegas/TraceTests.lean` doc comments of `checkGuardOfAgrees`, `checkEmittedClaimsReconcile`, `permittedNames`; `lean/Reactivegas/Trace.lean` `inductive GuardId`; the compiled census (`scripts/lean-mutants/run --census`) lists the fourteen `GuardId` constructors. |
| A-2 | `Reactivegas/Predicates.lean` names `docs/en/design/state-machine.md` as the prose record of its laws. | The page exists, but it links the Lean sources at `tree/main/lean/Reactivegas` while the repository's default branch is `master` (no `main` branch on `origin`), and it states the laws for "15 events" (its lines 63, 158, 184) where `Event` has fourteen constructors. The page belongs to #71. | `lean/Reactivegas/Predicates.lean` module doc; `docs/en/design/state-machine.md` lines 9, 63, 158, 184; `git symbolic-ref refs/remotes/origin/HEAD` → `origin/master`. |
| A-3 | `KelGroups/Vote/Fold.lean` said "`renounce` is carried in the vocabulary and is a no-op in this slice", against ruling V-5 (a proposer's renounce closes the question negatively) and ruling S-12 (a renounce by anyone other than the proposer is refused and changes no state). **Resolved by #81.** | The module doc now states the V-5 closure, and the definitions agree with both rulings: `effectedState` closes a renounced question with `verdict := .negative`, `cause := .renounced`, keeping the record; `validateVoteEvent` refuses a renounce by a responsabile who is not the proposer with `VoteError.notProposer` and a cast on a permission question by anyone but its designee with `VoteError.notDesignee`; `closeProposerQuestions`, run by `baseHook` on `memberRemoved`, closes a departing proposer's questions with cause `proposerDeparted`. Nothing on this row is left undecided by the Lean. | `lean/KelGroups/Vote/Fold.lean` module doc, `effectedState`, `closeProposerQuestions`; `lean/KelGroups/Vote/Validate.lean` module doc and `validateVoteEvent`; `lean/Reactivegas/Step.lean` `baseHook`; the census emitter sets (`scripts/lean-mutants/run --census`: both constructors emitted by `KelGroups.Vote.validateVoteEvent` alone), and their guard mutants in `lean/KELGROUPS-MUTANTS.md`. |
| A-4 | The doc comment above `seedDenyPermissionRefunds` (`Reactivegas/Trace.lean`) said its refused withdrawal "carries an identity with no accepted inversion, so the corpus also exercises an `UNPROVED` claim row". | `step_withdraw_inv` is an accepted inversion (`Reactivegas/Invariants.lean`); the emitted corpus binds that refusal to it (`"guard":{"declaration":"step_withdraw_inv","id":"withdraw"}`) and holds no `UNPROVED` row. The comment is corrected in the same change as this record. | `lean/Reactivegas/Trace.lean` above `seedDenyPermissionRefunds`; `lean/Reactivegas/Invariants.lean` `theorem step_withdraw_inv`; `lean/corpus/economic.json`. |

## Protocol for a future isolated reader

A clarity measurement counts only when the reader cannot have learned the model
from anywhere but the Lean.

1. **Inputs, frozen.** The reader receives a tree of the `lean/` directory at one
   commit (hash recorded) and nothing else: no `docs/`, no issues, no rulings, no
   simulator, no conversation with an author. Doc comments are part of the Lean and
   stay in.
2. **Questions before reading.** The questions (one per user story or ruling under
   test, each with the decision it asks for) are written and hashed before the
   reader starts; they never contain the expected answer.
3. **Isolated reader.** A fresh context with no access to the repository history,
   the authors, or earlier readers' answers. Its answers are written, per question,
   as: the decision; the Lean declarations it rests on; or `UNDECIDABLE` with what
   is missing.
4. **Comparison afterwards.** Only once the answers are frozen are they compared
   with the rulings. Each disagreement becomes a row in the table above (decision,
   Lean clause, ruling, evidence), never an edit to the answers.
5. **Void on contamination.** If any input outside step 1 reached the reader, the
   run is recorded here as VOID with that reason, like the historical one.
