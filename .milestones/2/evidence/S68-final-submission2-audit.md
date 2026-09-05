# Commit Audit

- Submission: 2/2 (repair of F-01 + F-02; FULL re-verify of original + amended mandate)
- Base (rejected): `e9db9a181f636e1d50862ea3990145e5d11c95e8`
- Mandate parent: `9a549f21b1a607d66932d48c71ac09c031563ef4`
- Candidate: `3ee5c12d4bb5893d25f6ff9d95f741e3eb76d1f3`
- Mandate: spec blob `988b2ab4` (R68-07 boundary qualification); data blob `45c112c8`; tasks blob `cdb2a133` (T68-25). Brief swapped the data/tasks labels; independently assigned hashes match those files.
- Tests blob: `40cf2635573f9e092ef9be33051bf93f37392726`
- Oracle sha256: `9448e889e1b8149f356c23c3706323f7c6a96d4d57541c77f6c9d6e69bf96428`
- Gate v3 sha256: `29e49c9dbaf9d20205ad09967dfe3235d85926be69a32a5aba74a2184ed482c8`
- Prior report: `37f3f1b2017646b07c14c4d1859846b17454a32bfbc6b53a589cb559b04899dd`
- Scope: FULL `e9db9a1..3ee5c12` plus named open findings F-01, F-02
- Verdict: PASS
- Audit loop: submission `2/2`; next submission `FORBIDDEN`
- Ceiling raises: `2/2`; 6→10 (NOTE-005), 10→14 (NOTE-008). No third grant.
- Campaign: CLOSED — ended by SET-POINT
- Builds: owner `13/14`; this audit `2/3` (just-lean cold-local+replay 56s; just-ci warm-lean 106s); contingency unused. Targeted overlay compiles not counted as full builds.
- Lean modes: PROOFS + INVERSIONS + FULL

Supersession (re-derived, not quoted as authority): the raw-domain *governance* guarantee is withdrawn; certify the qualified R68-07 statements. Structural inversion coverage is 14/14; exact-premise 11/14 with three inherited stall-premise omissions routed to #66 S5.

AUDIT-VERDICT submission=2 verdict=PASS

## Frozen inputs

| Item | Independent value |
|---|---|
| HEAD / dirty | `3ee5c12d4bb5893d25f6ff9d95f741e3eb76d1f3` detached, porcelain empty before and after every run |
| Repair delta | 2 files, +295/−1: `lean/KelGroups/Invariants.lean`, `lean/KelGroups/Tests.lean` |
| green-r2 manifest | Invariants blob `f07566e38b2fdba92729a4b05b1e72d77eb4d88b`; Tests blob `40cf2635…`; base `9a549f2` |
| `/tmp/reproof-68` | HEAD `3ee5c12`, Fold/Integration/Tests/Invariants byte-identical to candidate, porcelain empty (restored; not removed) |
| Instrument source | `3b4229fc5ce816e8a7de5c796b47e69dd9a71739aa10c0502be9902a2c23cd4d` |
| Instrument run | `0a2799b7a129f62416f74cf6b1e32bb2f31e05c707ee710f59aff8278fc1ddcb` |
| Witnesses A/B | `db9ee87292c9…` / `ed53a6da0c9a…` (frozen pre-execution; copies hashed here) |
| Public theorems | source `^theorem` = 163; lake `declared=163 elaborated-backed=163 expected=163` |
| gate.sh in audit tree | ABSENT (legs run manually) |

## Invariant matrix

Severity undeclared in the packet defaults to BLOCKING.

| Invariant | Severity | Verdict | Row state | Proof / evidence |
|---|---|---|---|---|
| I68-01 zero-open | BLOCKING | PASS | KILLED | Oracle `hEmptyOpen`/`iEmptyOpen` silent-true; Tests `t68HistEmptyOpen`/`t68IntEmptyOpen` compiled. Independent exact-one-edit A/B: `wEmptyOpen` GREEN then FALSE. |
| I68-02 self-assent bar | BLOCKING | PASS | KILLED | Both `validateApproval` and `validateBaseApproval` return `.proposerSelfApproval key id` before `alreadyApproved`. Oracle `hValidBar`/`iSelfRefused` + Tests `t68HistSelfBar`/`t68IntSelfRefused` execute. Validate untouched by the repair. |
| I68-03 sole-admin | BLOCKING | PASS | KILLED | Oracle/Tests n=1 two-step guards execute. `sole_admin_self_approval_ok` axioms `[propext, Classical.choice, Quot.sound]`. |
| I68-04 enactment sets | BLOCKING | PASS | KILLED | All 13 oracle guards execute (n=2/3/5, killer, admin-change). |
| I68-05 arithmetic | BLOCKING | PASS | KILLED | `majority_table` / `majority_not_strict_on_even` elaborated in just-lean/just-ci logs. |
| I68-06 WellFormed / every transition | BLOCKING | PASS | KILLED | **Qualified.** Governance preservation is `TraceAdmissible` / integrated-internal, not raw. Unconditional raw core is `RawStructural` (no proposer/adminCount/self-assent). 7-event exhibit executes (8 Tests guards). |
| I68-07 dependents / proof trust | BLOCKING | PASS | KILLED | Ruled pair present: `proposer_absent_above_one` axiom-free; `sole_admin_self_approval_ok` standard axioms. Private structural theorems axiom-clean (no `sorryAx`). Zero public-theorem delta. |
| I68-08 witnesses + two-path mutants | BLOCKING | PASS | KILLED | F-02 independently closed: exact-one-edit identity + module rebuild + semantic RED + restore equality + GREEN controls. |
| STATEMENT-SCOPE | BLOCKING | PASS | KILLED | `foldGroup_wellFormed` keeps `hadm`; `foldGroup_structural` has the single premise `RawStructural (emptyState initial)`, discharged by `emptyState_structural`. Excluded-domain comments name instrument `3b4229fc` vs run `0a2799b7`. |
| FAILURE-MODES | BLOCKING | PASS | KILLED | Repair does not change production Fold/Integration/Validate. New error variant still distinct and observable. Raw self-approval remains executable and is now exhibited, not silently accepted as well-formed. |
| INVERSIONS | BLOCKING structural / inherited exact | PASS | KILLED structural; 3 exact-premise onward | 14/14 constructors; negative control detects withheld `backdonate`. Exact-premise 11/14; pledge/accept/close stall omissions unchanged, not reopened. |
| SPEND-LEDGER | BLOCKING | PASS | KILLED | Owner 13/14 with matching log hashes for attempts 9–13; two ceiling raises at cap. This audit 2/3. |

## F-01 — qualified statements certified

`RawStructural` (private, Invariants.lean:1043) is:

- member-key Nodup
- pendingProposals-key Nodup
- `MembersCoherent` (`member.key = key` only)
- per-entry approvals-Nodup on `pendingProposals`
- pendingBase-key Nodup
- per-entry approvals-Nodup on `pendingBase`

No governance/self-assent/adminCount content.

`foldGroup_structural` quantifies `digest`/`appFoldFn`/`initial`/`events` with the single premise `RawStructural (emptyState initial)`. `emptyState_structural` proves that premise for every `initial`. Induction: `foldEvents_structural` → `applyEvent_structural`, exhaustive over `GroupEvent` (`base propose` / `base approve` / `app`) matching `applyEvent`. Approvals-Nodup is preserved on both stores (pendingBase by non-mutation on the historical path).

Also private, compiled, axiom-checked: `approvePending_idempotent` `[propext, Classical.choice, Quot.sound]`; `applyEvent_app_lists` axiom-free; `enact_implies_threshold_met` `[propext]` (no WellFormed premise — threshold evidence is unconditional and correctly “cited unchanged”).

Worker 7-event family (`t68RawTrace` prefix 6 + appended self-approve): prefix `t68RawTraceValidFrom` threads per-step `validateEvent` (stronger than final-decision-only); bar identity `.proposerSelfApproval "a" "remove:c"`; after-state `approvals==["a"]` at `adminCount==3` with member `c` present. Eight `#guard`s compiled true in `just lean`. Honest limit: first prefix event uses signer `"stranger"`; archived ScopeWitness uses `"a"`. Violation shape matches; this family does not import the auditor fixture.

`proposerSelfApproval (key) (proposalId)` remains at both validation call sites (Validate.lean:130 and :180). Sole-admin path unchanged (`1 < adminCount` is the bar).

## F-02 — independently closed

Exact-one-edit reconstructed from candidate bytes (unique hunk, line counts unchanged):

| Path | Site | Mutant sha256 | Restore git blob |
|---|---|---|---|
| A | Fold.lean:49 `[]` → `[signer]` | `c6cbb818705db481b5f6fd8469e7a7b279fe2e6f9a9675f4cb262df570d679cf` | `0b05fcaec76d45257a1768d5795e9832001cb192` |
| B | Integration.lean:191 `[]` → `[signer]` | `a23b27e8265024368e7b39399d4111548f9ce56ce18782550799d76199c706e2` | `be41debd28f043430058b990515773abd586fabf` |

These match the owner receipt prefixes. Tests blob and oracle hash unchanged by the mutants (overlays only). Candidate tree never edited.

Independent execution against candidate oleans, overlay module rebuild **before** each RED:

1. Witness A GREEN exit 0 (empty log) then mutant Fold.olean compile exit 0 then RED: `wEmptyOpen`, `wN2Pends`, `wBarIntact` FALSE (exit 1). Evidence `T9-A-red-independent.log` sha256 `62e757e1…`.
2. Witness B GREEN exit 0 then mutant Integration.olean compile exit 0 then RED: `wEmptyOpen`, `wKiller` FALSE (exit 1). Evidence `T15-B-red-independent.log` sha256 `352ef937…`.

Owner full-gate mutant logs remain semantic (A `95b879cd…` hstep-trio + stale/bootstrap; B `5463d8a7…` hroute2-simpa; tripwire unreached in both). `/tmp/reproof-68` is restored-clean at 3ee5c12.

`wBarIntact` FALSE under A is pending-shape coupling (`approvals==[]` / pending gone after auto-enact), not a Validate break. The witness file comment claiming it “stays TRUE” is inaccurate; the intended empty-open kills still fire. Bar survival is module confinement (Validate untouched). Advisory only.

## Lean surfaces

**Proofs.** Overlay `#print axioms` inside `namespace KelGroups` (instrument Invariants sha256 `4c27b791…`, COMPILE_I_EXIT=0): structural fold/step theorems `[propext, Quot.sound]`; `foldGroup_wellFormed` / `applyApprove_preserves_wellFormed` `[propext, Classical.choice, Quot.sound]`; no `sorryAx`. Tree-wide `sorry`/`admit` hits are comments/`admitMember` names (lead only; axioms close proof trust).

**Inversions.** `just lean` coverage: constructors=14 covered=14 missing=0; `--negative-control` withholds `backdonate` and detects it. Exact-premise: `step_pledge_inv` / `step_accept_inv` / `step_close_inv` still omit the stall conjunct (read, not reopened). Six accepted #66 inversions untouched.

**Correspondence.** Historical `applyEvent` and `applyEvent_structural` share the same three constructors. Integrated propose/approve still go through `validateBaseMutation` / `validateBaseApproval` then empty-open `approvals := []`.

## Failure modes altered

none altered — checked: repair touches only Invariants proofs/comments and Tests regression guards. Validate/Fold/Integration production bytes unchanged vs 9a549f2. Resource acquisition, threads, synchronisation primitives, and degradation paths are not in this slice. Observable refusal identity for barred self-approval remains `.proposerSelfApproval`; duplicates remain `.alreadyApproved`; rejected integrated approvals still return the pre-state via the error arm.

## Residuals

None on #68 BLOCKING rows.

## Candidate invariants

None.

## Onward discoveries — outside this ticket

- ONWARD-68-INV-01 (carried from s1, RECORDED, NOT-OPENED): `step_pledge_inv` / `step_accept_inv` / `step_close_inv` omit stall. Recipient: #66 S5 / inversion backlog via the ticket owner. Evidence: this-round coverage 14/14 + source read of the three theorems. Honest limit: converse incompleteness, not a false forward theorem; genesis reachability not claimed.

## Blocking findings

None.

## Verification receipts

| Command | Exit | Duration | Evidence |
|---|---:|---:|---|
| `git diff --check` | 0 | <1s | `leg0-diff-check.log` empty sha256 `e3b0c442…` |
| `nix develop --command just lean-toolchain-contract` | 0 | ~1s | `leg1-toolchain.log` `b6117b60…` |
| `nix develop --command just lean` (build 1/3, cache=cold-local+lake-replay) | 0 | 56s | `leg2-just-lean.log` `2d87d506…`; inversion 14/14; theorems 163/163/163 |
| `just lean-corpus-gate` + inner stdout | 0 | 3s | stdout `true` (`corpus-stdout.log` `a17fcf0a…`) |
| tripwire `rg approvals\s*:=\s*\[signer\]` | 1 (absent) | <1s | empty log; positive control: `approvals` hits in Fold.lean |
| `lake env lean specs/68-proposer-assent/witness-t68.lean` | 0 | 2s | oracle stdout 0 bytes |
| witness A/B GREEN | 0 / 0 | 2s / 2s | empty logs |
| overlay Fold compile + witness A RED | 0 then 1 | 2s | `T9-A-red-independent.log` `62e757e1…` |
| overlay Integration compile + witness B RED | 0 then 1 | 2s | `T15-B-red-independent.log` `352ef937…` |
| overlay Invariants `#print axioms` | 0 | 4s | `axioms-private.log` `89b60f6f…` |
| `nix develop --command just ci` (build 2/3, required) | 0 | 106s | `just-ci-independent.log` `b1ee60d2af490a96e45889bd97bbfc7d0079e00234bb23619b5de44242f19762` (byte-identical to owner gate-12) |

Candidate porcelain empty after all overlay work. `/tmp/reproof-68` not modified, not removed.

## Owner spend vs evidence (attempts 9–13)

| id | Claim | Log sha256 prefix | Independent hash |
|---|---|---|---|
| 9 | repair GREEN | 105a03b0 | `105a03b0d3f03212b5ca1839e6aac22f9c44a0689e61161fd321ef62165d8163` |
| 10 | mutant-A RED | 95b879cd | `95b879cdb5f1dd598a36e9f4208175046807522d701bc10e532482e456b77cd9` |
| 11 | mutant-B RED | 5463d8a7 | `5463d8a73b223b8b6fdde6e577cf2937092c068898fc7c92b70b600ad75a545c` |
| 12 | just-ci | b1ee60d2 | `b1ee60d2af490a96e45889bd97bbfc7d0079e00234bb23619b5de44242f19762` (rerun matches) |
| 13 | submission-2 gate | 581a7425 | `581a7425743b07b263811f1e3bcb5b6032ab11b33c2d7f1c561ae38149f8ec6d` |

Reserve 14 unused. Two recorded ceiling raises, none beyond.

## Advisories

- `wBarIntact` is not a surviving-bar control under mutant A; it fails via the empty-open conjunct. Property shape: a bar-survival control must not require `approvals==[]` on the mutated propose. Instrument: `evidence/witness-A-emptopen.lean` sha256 `db9ee872…`.
- Worker raw prefix first signer differs from ScopeWitness (`stranger` vs `a`). Shape of the excluded class still matches.

## Honest limits

This audit did not re-mutate Validate, majority arithmetic, or the vote/Step machines (repair fence). Exact-premise stall omissions are inherited and not assigned here. `foldGroup_structural` takes `emptyState_structural` as an external premise rather than inlining it; the premise holds for every `initial`. Overlay axiom prints compile a copy of Invariants with extra commands; they do not alter candidate bytes.

Submission 2/2. A FINDINGS verdict would have closed the owner campaign. This is PASS: acceptance remains the ticket owner's decision. No further bounce is available inside this ticket.

Wall time: START `2026-09-05T11:02:29Z` → freeze below. Pane `%536` grok-4.6 effort=high observed; parent `%512` muse; author `%519` muse; window `reactivegas:6`.
