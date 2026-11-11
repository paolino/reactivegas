# Modules model — #68 (A-001 reading)

Responsibilities and dependency direction for the slice. Only changed
decisions; no imports, bodies, or algorithms.

- `KelGroups.State` — owns the two pending stores (`PendingProposal`
  historical, `PendingBase` integrated/production), `majority` (UNTOUCHED),
  lookups. No behavior change expected (docs only if WellFormed moves file).
- `KelGroups.Fold` — owns the HISTORICAL transition (`applyProposeDetailed`,
  `applyApproveDetailed`, `tryEnactDetailed`, `foldGroup`). Change: propose
  opens empty. The raw fold stays validation-free; refusal lives in
  `Validate` (boundary distinction preserved per A-001).
- `KelGroups.Validate` — owns admissibility. Change: `validateApproval` and
  `validateBaseApproval` refuse self-approval above n=1 under a NEW error
  variant (never `alreadyApproved`); sole-admin self-approval passes.
- `KelGroups.Integration` — owns the PRODUCTION boundary
  (`applyIntegratedEvent`, `tryEnactBase`, `foldIntegrated`). Change: propose
  opens empty into `pendingBase`; enactment reads current canonical
  membership. Sealed hook path unchanged.
- `KelGroups.Invariants` — owns the proof surface. Change: restated
  `PendingWellFormed`, proposer-theorem replacement pair, preservation proofs
  on both paths, threshold evidence intact in meaning.
- `KelGroups.Tests` — owns worker `#guard` pins. Change: proposer-credit
  expectations rewritten to empty-open; new guards mirroring the oracle.
- `Reactivegas.Invariants` — owns wrappers/corpus emitters. Change: adopt the
  restatement; emitters only if approvals shape anion observations.
- `specs/68-proposer-assent/witness-t68.lean` — TICKET-OWNER oracle, fenced,
  unimported, explicitly run. Not a lib module; never a dependency of
  anything above.

Direction: State ← {Fold, Validate}; {Fold, Validate} ← Integration;
all ← Invariants; {Invariants, Validate} ← Tests. Oracle depends on build
artifacts, nothing depends on the oracle. No new modules; no promotion (the
refusal is a new error VARIANT, not a new component).
