# Tasks — #54

Task IDs are stable. Only the ticket owner checks a box, and only after a fresh
independent audit has passed the exact candidate.

## Slice 1 — faithful vote machine (AUTHORIZED)

- [x] T5401 Wire `lean/KelGroups` into `lean/lakefile.lean` as an additional
      default target, and add the `lean/KelGroups.lean` root importing every
      submodule. (R-1, R-4)
- [x] T5402 `KelGroups.Types` — member, role, admin, role definitions, group
      configuration. (R-5, data-model Roles/Member/Group configuration)
- [x] T5403 `KelGroups.Event` — the three proposal payloads and the event
      vocabulary. (data-model Proposal payloads/Events)
- [x] T5404 `KelGroups.State` — group state, pending proposals, admin counting,
      the `(n+1)/2` threshold, auth mode. (R-9, R-10)
- [x] T5405 `KelGroups.Fold` — propose, approve, enactment attempt, the three
      enactment payloads, application-event folding. (R-6..R-8, R-11..R-17, R-24)
- [x] T5406 `KelGroups.Validate` — bootstrap and normal proposal validation,
      approval validation, application-event validation, first-error fidelity.
      (R-18..R-23)
- [x] T5407 `KelGroups.Invariants` — VI-1..VI-5 proved; `majority_table` and
      `majority_not_strict_on_even` proved. (R-27, R-28)
- [x] T5408 `KelGroups.Invariants` — VI-6 and VI-7 delivered as executed
      counterexample witnesses, explicitly **not** as theorems.
- [x] T5409 `KelGroups.Tests` — every point test enumerated in
      `functions-model.md`, non-vacuous under `lake build`. (R-29)
- [x] T5410 A **tracked** dependency-direction checker, invoked from the `lean`
      recipe in `justfile` so that both `just ci` and the existing GitHub CI
      step run it on every build. (R-2b)
- [x] T5411 `docs/en/design/kelgroups-vote-machine.md` — the reviewed fidelity
      matrix, plus its `mkdocs.yml` nav entry. (R-25, R-26)

## Slice 2 — structural composition (BLOCKED)

Blocked on P-1 (#48 definitions accepted, both consumer signatures frozen) and
P-2 (EP-DENY ruled). Do not start.

- [ ] T5412 Evaluate verdict-carrying permission events; record the design
      decision, or the concrete impracticality justifying the relational
      fallback, **before** implementation. (R-31)
- [ ] T5413 Compose under `lean/Reactivegas/`, replacing the unilateral
      `isResponsabile` permission guard. (R-30, R-32)
- [ ] T5414 Expose enacted verdict identity, question, and provenance. (R-33)
- [ ] T5415 Enumerate purchase approval and voted comune backdonation as
      consumers of the one verdict interface. (R-34)
- [ ] T5416 Status wording `enforced: PROVED-IN-MODEL` with the later-port
      caveat, in documentation and theorem metadata. (R-35)

## Vote-coverage run — Slice A: the tally machine (AUTHORIZED)

- [ ] T5420 `lean/KelGroups.lean` imports the new `KelGroups.Vote.*` root set,
      and a deliberate elaboration error under `lean/KelGroups/Vote/` is shown
      to make `just lean` red. (R-40, R-43)
- [ ] T5421 `KelGroups.Vote.Types` — three-way verdict, ballot, question kind,
      closure cause, the threshold-policy type and its two named instances.
      (R-46, R-47, R-49, R-62)
- [ ] T5422 `KelGroups.Vote.State` — state, franchise, tallies, and `verdictOf`
      as the single verdict site taking the threshold explicitly. (R-46, R-50)
- [ ] T5423 `KelGroups.Vote.Event` + `KelGroups.Vote.Validate` — the event
      vocabulary and the distinct admissibility errors, franchise included.
      (R-44, R-45)
- [ ] T5424 `KelGroups.Vote.Fold` — ballot placement, the unconditional
      recompute-and-close sweep, and closure as an append rather than a
      delete. (R-51, R-55, R-56)
- [ ] T5425 `KelGroups.Vote.Invariants` — `ballots_nodup_disjoint`,
      `open_questions_are_open`, `questions_partition`, `no_expiry`,
      `foldVote_wellFormed`, all over the production fold. (R-52, R-54, R-57,
      R-61, R-68)
- [ ] T5426 `KelGroups.Vote.Tests` — the V-2 witnesses (tie passes under the
      legacy policy; zero threshold passes with no ballot) and the V-3 witness
      (a question closes positive because a responsabile left), each executed
      through `foldVote`. (R-48, R-53, R-69)
- [ ] T5427 Controls proved able to fail for Slice A: no dissent path; a voter
      in both tallies; silent deletion; recompute only on ballots; an expiry
      field. (R-70)
- [ ] T5428 Dependency-direction coverage of `lean/KelGroups/Vote/`
      demonstrated, and zero `sorry`/custom axioms recorded as gate evidence.
      (R-41, R-42, R-71)

## Vote-coverage run — Slice B: the paths that are not a tally (AUTHORIZED)

- [ ] T5430 Permission questions: named designee in the kind, designee-only
      admissibility with its distinct error. (R-62, R-63)
- [ ] T5431 `permission_ignores_threshold` — a permission verdict is
      independent of the threshold argument and the franchise size. (R-64)
- [ ] T5432 Proposer renunciation, proposer-only, for both question kinds.
      (R-58, R-65)
- [ ] T5433 Closure on proposer departure — loss of responsabile standing and
      loss of membership — with `closure_of_departure_is_negative`. (R-59,
      R-60)
- [ ] T5434 Direct member admission and `admission_opens_no_question`, with the
      executed immediacy witness. (R-66, R-67)
- [ ] T5435 Controls proved able to fail for Slice B: a permission decided by
      majority; admission routed through a question; a departure closure
      carrying a non-negative verdict. (R-70)

## Vote-coverage run — Slice C: record honesty (AUTHORIZED)

- [ ] T5440 Correct the stale `EP-DENY` sentence in
      `docs/en/design/kelgroups-vote-machine.md` and point it at the required
      machine, leaving every fidelity claim about kelgroups `368b596`
      unchanged. Fence extension reported upward. (spec §"Ruling absorbed")
