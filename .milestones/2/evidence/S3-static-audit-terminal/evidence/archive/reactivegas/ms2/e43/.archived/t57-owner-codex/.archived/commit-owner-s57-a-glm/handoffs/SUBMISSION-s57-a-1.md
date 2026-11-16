# Submission receipt — S57-A submission 1

- Candidate: `400f5b2829eeae27faeb0994ba8cfcc03c37dd3d` (parent RED
  `da3ebdb3319079f7f08af8d785c9a38ef64c7f38`, base `bb3ac41a1456c50b1bba7dafd522c174461b42ea`)
- Changed paths: exactly `lean/KelGroups/Vote/{Validate,Fold,Invariants,Tests}.lean`
  (gate: `source fence changed_paths=4`); candidate total 242+/278− (ceiling 900)
- RED diff sha256 `fa299779ed9e170c9a938303a47d6e6e51c0d16f266753ad3345380a99849e47`
  (handoffs/red-s57-a.diff + .manifest, verify-commit-handoff OK)
- GREEN delta sha256 `6af7a6198104f6e162236ce28b03df4c93110764b28ba02fcd6d34a9aa72d4f4`
  (handoffs/green-s57-a.diff + .manifest, verify-commit-handoff OK)
- Toolchain: Lean 4.25.0 on every receipt; inert `lean/lean-toolchain` 4.27 pin untouched

## Invariant → proof/evidence map

| Invariant | Proof / control | RED evidence | GREEN evidence | Result |
|---|---|---|---|---|
| INV-57-BOUNDARY | `applyVoteEvent` single validation decision before effect and sweep; `inadmissible_is_noop` | `evidence/red-focused-v4.log` `4d4de71e…` (unsolved `sweepClosures θ gs = gs`) | gate `R-45 preflight ok`; instrument `arbitrary-surface-noop` green (`applyVoteEvent == arbitraryPreState` on a stale-tally state) | PASS |
| INV-57-NOOP | `inadmissible_is_noop` — arbitrary `gs`, exact `.error error` premise, no `VoteWellFormed`; axioms `[propext]` | same log (theorem unprovable at base) | focused build exit 0; gate axiom print clean | PASS |
| INV-57-AUTH | `nonresponsabile_event_noop` (universal, no event-kind premise) + Tests six-constructor stranger rejection/inertness guards + explicit member/role no-ops | same log (`⊢ False` for `renounce`/`admitMember`/`removeMember`/`setRoles`) | instruments `r45-production` + `arbitrary-surface-noop` green | PASS |
| INV-57-EXHAUSTIVE | `check-event-boundary.sh`: 6 constructors enumerated, wildcard absent; seeded surface-extension control | frozen `gate-negative-controls-v2.log` `110367e3…` 10/10 (superseded only as to gate version; instruments unchanged) | gate v2 run: `exhaustive … wildcard=absent`; `mutant red structural-bypass reason=BYPASS` | PASS |
| INV-57-NOEXPIRY (ADVISORY-BUT-REQUIRED) | `PreservesQuestionSemantics` (semantic: target ballots, franchise, proposer standing) + `no_expiry` restated on it; Tests decide-witnesses positive and negative | red: identifier absent at base (`red-focused-v4.log`) | gate `green instrument no-expiry-member`; Tests `PreservesQuestionSemantics` decide + three ¬-discrimination witnesses | PASS |
| INV-54-PARTITION | `questions_partition` re-proven on repaired fold | — | gate v2 `mutant red inherited-partition reason=PARTITION` | PASS (fresh) |
| INV-54-DISJOINT | `ballots_nodup_disjoint` | — | gate v2 `mutant red inherited-disjoint reason=DISJOINT` | PASS (fresh) |
| INV-54-NOSTALE | `open_questions_are_open` | — | gate v2 `mutant red inherited-nostale reason=NOSTALE` | PASS (fresh) |
| INV-54-FRANCHISE | `franchise_of_tallies` + `unfranchised_cast_noop` (now without well-formedness premise) | — | gate v2 `mutant red inherited-franchise reason=FRANCHISE` | PASS (fresh) |
| INV-54-POLICYFREE | `verdictOf_threshold_congr` | — | gate v2 `mutant red inherited-policyfree reason=POLICYFREE` | PASS (fresh) |

## Verification receipts

| Command | Exit | Evidence (sha256) |
|---|---:|---|
| Focused RED `lake build KelGroups.Vote.Invariants KelGroups.Vote.Tests` | 1 (intended) | `evidence/red-focused-v4.log` `4d4de71ea5fad74565b59a586626ed7f7e06e332d0e38f43c036ba833a6f90a1` |
| Focused GREEN (same command) | 0 | `evidence/green-focused-v3.log` `1f2cb610874f652cdf0d199de27a2c1b9ebc7542a4c1ea8c389b9216dbf977b1` |
| Immutable gate v1 (blocked run, Q-001) | 1 (gate defect) | `evidence/gate-s57-a-green.log` `bf305b0da995cb8eafa24d506ffb744e337d7ae0de6a14c48db07b4aff467f2d` |
| **Immutable gate v2** `gate-s57-a.sh.v2` sha256 `bc9c336bcf854f84192f4c6f62d107ff9613bfd0cb21bd8004b7aeb9f348fc1b` (manifest v2 `a0dc3c5ae2505535d9e53d7f4ac44dc72f0014aec30c399f9c158a72cf336942`) | **0** | `evidence/gate-s57-a-green-v2.log` `830ac4c484f06eee316de3d486fb6093c8ff4951d0779412cb28fd77d93a9256` — all legs ok, final line `gate: GREEN issue=57 slice=S57-A Lean-4.25.0` |

## Measurements

- `draft=NONE`, salvage ratio `0`, owner delta = full diff
- Pi-reported tokens: unavailable (no telemetry); deterministic fallbacks above
- Owner wall time: START 13:00:47Z → PROOF-COMPLETE 2026-08-29T14:06Z (includes Q-001 blocked wait)
- Worktree/index clean at candidate; no push, no PR/issue edits; audit budget untouched (`builds_spent=0` of 20 by me)

## Honest limits

- Gate crash on v1 line 74 was a gate defect, not a candidate property; the
  semantic checks v1 completed before crashing are superseded by the v2 run
  recorded above (v1 not rerun, not altered).
- Mutation campaign statements here are limited to the six named frozen
  mutants plus the seeded surface-extension control; no claim of mutant-class
  completeness is made.

---

# Submission receipt — S57-A submission 2 (F-001 repair)

- Audit report: `6a4985eeb95c440dfbf891c86bb49dce06ee928aeacd636141c755deb4e813ba`
  (verdict FINDINGS, single accepted blocking finding F-001); repair
  authorization: inbox/AUDIT-REPAIR-AUTHORIZED-s1.md; audit builds 6/20
- Prior rejected candidate: `400f5b2829eeae27faeb0994ba8cfcc03c37dd3d`
- Repair candidate: `9d68abb0930bb31d9bcd1116979765e974547ffd` (base
  `bb3ac41a1456c50b1bba7dafd522c174461b42ea`; ancestry base → RED → GREEN → repair)
- Repair delta: `handoffs/repair-s57-a-f001.diff` sha256
  `ee4e96f403e1b80b3a55f672023113f53e5fc40b1cec0e52e9ff728c9aa89477`
  (manifest v2, base-commit 400f5b2, verify-commit-handoff OK)
- Changed paths in delta: exactly `Vote/Fold.lean` (guard removal + stale doc),
  `Vote/Invariants.lean` (proof adaptations: effectedState_sweepReady,
  effectedState_preserves_qid, effectedState_tally_growth +admission premise,
  call site); 45+/56−
- F-001 property class: every event effect is authorization-free; all signer
  authorization occurs only in the total exhaustive `validateVoteEvent`
  boundary. Permanent structural control: gate v3 `event effects
  authorization-free` — frozen RED on the rejected candidate
  (`gate-s57-a-f001-red.log` `0fa6d82c…`), GREEN on the repair.
- Immutable repair gate v3 sha256
  `96fba04a558994d57d6bb12a5ee9fbfcdab442f460e46ab9ccaeee45124f2997`
  (manifest v3 `b628b5673f031671393e6fe5ef30d53b4c1034bdd44e0aa3b3ee6a6172f52a9a`):
  **exit 0** — all legs ok including the new structural check, all prior
  semantic checks, instruments, mutants, and full repository CI.
  Evidence: `evidence/gate-s57-a-green-v3-repair.log` sha256
  `fc554d8e8b7f5534fa66c8aa40dfb1168f272561b5c83462b3a8f8103656da0d`
- Focused repair build: exit 0 — `evidence/repair-focused.log` sha256
  `840ea9873cb5a45677744696b8eaa45da2086dc06aa04403ea45a3a3c5833ad7`
- Production-path behavior unchanged (rejection still returns input exactly
  before effect and sweep); no validator, spec, task, or other-module change
- Worktree/index clean; no push, no squash, no task stamp (awaiting acceptance)
