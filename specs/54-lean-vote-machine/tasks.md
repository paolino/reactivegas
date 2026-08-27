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
