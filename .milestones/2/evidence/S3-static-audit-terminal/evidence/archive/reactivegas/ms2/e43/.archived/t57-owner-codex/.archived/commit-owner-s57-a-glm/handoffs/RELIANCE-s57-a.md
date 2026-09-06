# Reliance declaration — #57 S57-A structural validation

Commit owner: commit-owner-s57-a-glm (glm, pane %168). Base: bb3ac41a.
Scope of reliance: what the S57-A change *depends on* being true in code it
does not own. This is not a survey of the Vote modules.

INV-57-REL-SUBSTRATE
invariant: the KelGroups.Types substrate equations used by the vote machine
  (hasAdmin, Member roles, assocLookup/assocInsert/assocErase/assocAdjust,
  setInsert, LawfulBEq Key) keep their current transparent definitions, so the
  re-proved assoc/tally lemmas in Vote.Invariants remain sound.
severity: BLOCKING
enforced: gate leg `Slice-1 modules blob-identical to ccdda830` plus the
  focused build re-elaborating every re-proved lemma; no mutation-specific
  control in this slice (substrate is outside the fence by design).

INV-57-REL-EVENTSURFACE
invariant: `VoteEvent` is the closed six-constructor surface and the
  authorization boundary enumerates it with no wildcard, so a new constructor
  cannot silently acquire an authorization default.
severity: BLOCKING
enforced: gate `check-event-boundary.sh` constructor scan (wildcard-absent +
  every constructor named) and the seeded surface-extension control
  (`falsify-surface.sh`, reason=authorization-boundary-missing-constructor).

INV-57-REL-SIGNATURES
invariant: `validateVoteEvent`, `applyVoteEvent`, `effectedState`,
  `sweepClosures`, `foldVote`, `foldFrom` keep their public signatures; the
  gate's required-theorem and instrument legs elaborate against exactly those.
severity: BLOCKING
enforced: functions-model fixes them; focused build + frozen gate compile all
  instruments against the candidate. A signature drift fails mechanically.

INV-57-REL-BOOTSTRAP
invariant: from `emptyVoteState` a first responsabile can be seeded: the
  empty-franchise admit branch admits the seeder, and every production trace
  in Tests/gate instruments reaches a nonempty franchise before any R-45
  oracle fires.
severity: BLOCKING
enforced: `witnessTraceValid` over every Tests trace and the r45/no-expiry
  instrument preflight traces on the candidate. On the pre-slice base:
  enforced: NONE (the empty-franchise branch is new in this slice).

INV-57-REL-TOOLCHAIN
invariant: the Nix shell provides Lean 4.25.0 and lake honours it; the
  `lean/lean-toolchain` 4.27 pin is inert (Q-002/A-002, #54 evidence).
severity: BLOCKING (receipt identity, not model semantics)
enforced: gate leg 1 (toolchain identity, `version 4.25.0`). Reconciliation
  itself is the named milestone-wide follow-up, outside this slice.

INV-57-REL-ERRORIDENTITY
invariant: `VoteError.notResponsabile` is the single rejection error of the
  universal class, and the existing `BEq (Except VoteError Unit)` instance
  compares error identity, so every point oracle pins the exact error.
severity: ADVISORY
enforced: Tests point guards for all six constructors pin
  `Except.error VoteError.notResponsabile` exactly; a coarser error would
  redden them.

Count: 6 rows. Enforced on candidate: 5. Enforced: NONE (pre-slice only): 1
(INV-57-REL-BOOTSTRAP, becomes enforced by the candidate's own executed
traces). No declared reliance has been found false on inspection; if any row
above turns out false in the tree, that is a contract challenge, not a silent
workaround.
