# Epic map — `paolino/kelgroups#29` (upstream substrate for Reactivegas milestone2)

Epic owner Muse (Pi/opencode-go/muse-spark-1.3-contributor/xhigh), pane `%532`
(`reactivegas:8.1`), runtime root `/tmp/reactivegas/ms2/e-kelgroups-substrate/`.
Parent: milestone desk `%510`, runtime `/tmp/reactivegas/ms2`.
Brief: `artifacts/ASK-kelgroups-e29-substrate.md`
sha256 `399d9268904fbdc5bf9932bfc4e44e0acea432aac6c042b0b9c30b435379c36d`.
Authority: explicit operator authorization 2026-09-05 ("yes let a team work on
kelgroups") for limited sibling `#29/#28/#30` lane resolving reactivegas#73.
Resurrection-grade; rebuilt from durable state only.

Base: `/code/kelgroups` clean `main 368b596fef0b6d393c2ac7afc631d236c55d86d1`
(verified 2026-09-05T10:57Z, `status --porcelain` empty, HEAD == origin/main
pinned value). Existing `/code/kelgroups-issue-28` branch
`feat/28-generalize-app-api` at `6ec0248` behind main, zero own commits, clean,
unowned: no tmux pane except this epic owner claims a kelgroups cwd (scoped
`list-panes` grep 2026-09-05 shows only `%532`); desk inventory stated none
claimed. Preserved; reuse = fetch + rebase onto `368b596`, never mistake
existence for progress. No `AGENTS.md` in kelgroups root (checked; absent).

## Source-of-truth reconciliation (frozen inputs, read 2026-09-05)

Accepted spec = Reactivegas `master 4a6cd87` Lean + later operator rulings.
Current kelgroups issue bodies are STALE INPUTS, corrected below with dates.

- `LEDGER.md` (2026-09-05T10:50Z) + `artifacts/PLAN-TO-MILESTONE-2.md`: outcome
  = stranger drives election→collection→pledge→assenso→purchase→refund via
  published Haskell coordinator on kelgroups + released browser + CLI, one pure
  core native/wasm32-wasi. #73 upstream blocker owns rejecting fold, GroupView,
  sealed atomic hook, direct admission, pendingBase, vote lifecycle. #76
  consumer binding stays with Reactivegas. No vendoring.
- `e-haskell-impl/handoffs/D1-ASSESSMENT.md`: 0/26 `Integration.lean` names
  present in kelgroups Haskell (Threshold hits are KERI inception, unrelated).
  Historical measured gap, NOT a permanent 26-name whitelist — discovery-driven
  extent governs.
- `e-haskell-impl/handoffs/EPIC43-REQUIREMENT-MAP.md`: R9c/R11 corrections —
  `kelgroups-client` is a package inside kelgroups (keys/transport/shell/bundle
  present; Reactivegas UI + core.wasm absent; `Client/Fold.purs` is a second
  base-fold implementation in tension with core.wasm). Delivery choice for D4
  (which package links the app `Main`) is packaging, not architecture. Voci
  unmodelled non-goal. Consumed, not re-argued.
- `e-haskell-impl/handoffs/ASSENSO-ORACLE-GAP.md` (rev2): grant/deny/backdonate
  must be provably vote-derived (NOTE-016/A-Q001); `Composition.lean`
  classification is delivered, the runtime wire is missing (reachability /
  target / polarity unbound); C1–C5 executable negative witnesses belong to
  #76. Do NOT implement Reactivegas economics in kelgroups; do NOT claim
  Composition proves an executable consumer path.
- `lean/KelGroups/{Types,State,Validate,Fold,Integration}.lean` +
  `Vote/{Types,State,Event,Validate,Fold}.lean` at `e6c5924`/`4a6cd87`:
  `IntegratedAppFold` (signer→pre/post GroupView→AppState→AppEvent→
  `Except AppError AppState`), `BaseHook` sealed atomic via `commitBaseChange`,
  `IntegratedEvent` distinct BaseProposal/AppEvent params, `IntegratedError`,
  `IntegratedResult` with `Option BaseChange`, `Integration` bundle (reserved,
  digest, proposalMutation→BaseMutation which cannot admit, appFold, baseHook),
  `DirectCommand.admitMember` sole insertion, `pendingBase` typed by
  `BaseMutation`, `GroupView` sole read-only projection, `foldIntegrated`
  keeps aggregate on error, `validateDirectAdmission` /
  `validateBaseMutation` / `validateBaseApproval` exact refusal identities,
  one writable members relation (`INV-62-DIRECT-ONLY`).
  Vote subtree is `REQUIRED-OF-SUBSTRATE` vocabulary + production fold:
  `Verdict` (positive/negative/open), `Threshold` parameter (legacyThreshold /
  zeroThreshold exhibits, NOT defaults), `Ballot`, `QuestionKind`
  (collective/permission designee), `ClosureCause`
  (tally/franchiseChange + carried proposerDeparted/renounced for Slice B),
  `placeBallot` one-position placement, `sweepClosures` same-step close with
  record (R-51/R-61), `validateVoteEvent` boundary, no clock/no-expiry (R-54).
- `questions/A-V2-AND-PLEDGE-AGENCY.md` (operator 2026-09-05): V-2 zero-open /
  proposer-not-assent + pledge-agency (free while pending, referente after
  acceptance). Settled but NOT YET LANDED — do not anticipate; #28 plans
  integration after acceptance explicitly (threshold parameter stays; no new
  theta default).
- `t68-proposer-assent/answers/A-001*`: n=1 sole admin separate explicit
  approve; n>1 proposer cannot supply counted assent; majority arithmetic
  unchanged; n=3 needs two OTHER assents. Applies to base `approve` path when
  it lands; #28 freezes against current accepted base, rebinds on landing.
- `#75/#76/#81` + `REPLAY-CONTEXT-CONTRACT.md` (R3.1 planning contract):
  replay-context table authority, pre-replay refusal vs runtime abort vs
  mismatch, source-side re-derivation required for #75. #28 exercises the
  actual append/replay boundary + persistence (Store/KEL), not only the pure
  helper. R3.1 context stays a Reactivegas-side test input; not a kelgroups
  shipped default.

## Dated issue-body corrections (stale → accepted)

- 2026-09-05 #28 sketch `AppLogic{appStep, appOnBase total}` + "base never
  rolls back": STALE. Accepted requires `BaseHook` returning
  `Except AppError AppState` + `commitBaseChange` sealed atomicity (hook
  refusal discards the whole transition) + rejecting integrated route
  (`IntegratedError`). Do NOT implement the total/nonrollback sketch.
  Also #28's "Lean proofs updated to generalized types" is stale direction:
  Lean already specifies the generalized shape; Haskell converges to it.
  `kelgroups-client` fold/state mirror is out of #28 fence (belongs to #84 /
  future client decision, not this lane).
- 2026-09-05 #30 "single-admin immediate enactment, generic base proposals,
  rejection/expiry observable": STALE. Reconcile against distinct
  direct/base/app routes, `QuestionKind`, explicit `ClosureRecord`
  (verdict+cause, retention not expiry — R-54 no clock, R-61 never drop
  silently), current `majority` franchise rule + pending V-2 selfbar rebind.
  Do NOT smuggle refusals from dormant `notDesignee`/`notProposer` (zero
  construction sites, Slice-B forward declarations) and do NOT assume a
  shipped theta default (threshold is a parameter; exhibits are not defaults).
- 2026-09-05 #29 "legacy laws authoritative via substrate-mapping doc":
  STALE. Accepted Lean is the executable specification; later operator rulings
  supersede legacy prose. Runnable artifact/demo + v2 major release rows are
  MAPPED but need separate publication authority (see limits).
- Unresolved authority conflict: none currently blocking #28 slice 1. If one
  surfaces, ticket owner BLOCKS with exact competing evidence; settled rulings
  are not reopened.

## Children

| slice | issue | owner | outcome (user-runnable) | depends on | proof | state |
|---|---|---|---|---|---|---|
| S28-1 | kelgroups `#28` | `t28-app-api`, seat `muse` (pane `%534`) + commit owner `%545` | ONE coherent slice (test-only demo proves API slice) | accepted Lean (frozen); #68 V-2 rebind when landed (explicit) | r4 gate GREEN pending; RED `570fe4a` committed+verified; Codex audit after GREEN candidate | RED-complete → GREEN (mechanical v4 corrections via ticket owner, NOTE-005 authority) |
| S28-2 | kelgroups `#28` | same ticket owner, IF opened | CONTINGENT, UNSCOPED: opens ONLY on a named remainder from S28-1 review (exact leftover behavior specified then); two slices is an upper bound, not a plan — S28-1 defers no promised boundary proof here | S28-1 review names a remainder | same gate family, new mandate | contingent (not promised) |
| S30 | kelgroups `#30` | TBD after S28-1 | accepted substrate vote interface + closure evidence (`Question`/`Ballot`/`Verdict`/`Threshold`-param/`ClosureRecord`/`foldVote`/`sweepClosures`); missing upstream Lean semantics enumerated as deps, not invented | S28 accepted interface + #68 V-2 landing (explicit rebind) | contract proposed after #28 establishes actual interface | planned, NOT dispatched (no unbounded downstream) |
| S29-close | kelgroups `#29` | epic owner | epic acceptance handoff for reactivegas#73: nondegenerate demo capability + release/demo remainder mapped; publishing needs separate authority | S28 + S30 | interface packet returned to desk `%510` via local inbox | mapped |

Merge order: S28-1 → [S28-2 ONLY if S28-1 review names a remainder] → S30 → S29-close. Sequential (#30 blocked by #28 per issue body, confirmed). No parallel heavy
builds inside the ticket. S28-1 is the one coherent #28 slice carrying the FULL append/replay/persistence boundary; S28-2 is a contingent upper bound, never a place to defer a promised S28-1 proof (no double-counting one boundary as delivered-now and deferred-hardening).

Epic artifact: per `resolve-epic` a runnable exists from first behavior child.
The slice-proving vehicle is the ticket's test-only nondegenerate instance (proves the API slice, nothing more). Epic #29's requested runnable demo (child 3) REMAINS a distinct owned deliverable: a follow-on ticket with its own bounded contract after S28 establishes the interface — local demo implementation inside this agreed epic is authorized once that contract is set, no prohibition invented here. The v2 major release + downstream consumption notes (child 4) are likewise registered owned remainder; publishing/release stays separately gated. D2/D3 stay with sibling `e-haskell-impl`; this lane
sends it NO instructions, only returns the accepted interface packet to the
desk.

## Contract registry

| contract | producer | consumers | stable version | release signal | enforcing check |
|---|---|---|---|---|---|
| `KelGroups.Integration` surface (IntegratedAppFold/BaseHook/IntegratedEvent/IntegratedError/IntegratedResult/Integration/direct-only admission/pendingBase/GroupView/commitBaseChange/foldIntegrated + validators) | this lane (S28) in `paolino/kelgroups` | reactivegas D2/D3 via #73 | NONE yet | kelgroups tag/release (separate authority) | frozen gate G28-1 + `just ci`; ticket-level, no bypass |
| substrate vote interface + closure evidence | this lane (S30) | reactivegas #76 (consumer binding stays there) | NONE yet | same | S30 contract (proposed after S28) |
| `reactivegas.trace/v1` + R3.1 replay context | Reactivegas Lean/#75 | S28-1 boundary exercise (test input) | v1 frozen in Lean source | #74/#75 | replayer abort/mismatch distinction; no shipped theta default |
| V-2 selfbar (zero-open, no proposer assent, n=1 separate approve) | operator, landing via #68 | S28 approve path + S30 | ruled, unlanded | #68 merge | explicit rebind + revalidation, never anticipation |
| one writable membership store / direct-only insertion / sealed hook atomicity / refusal-leaves-state-and-log-unchanged | accepted Lean | S28-1 | proved in Lean | n/a | per-requirement relevant witnesses + can-fail mutants (frozen before impl) |

## Invariant ledger

| shape | instance | mechanism |
|---|---|---|
| single writable store, single insertion path | `INV-62-DIRECT-ONLY`, `BaseMutation` cannot admit | Haskell types (S28-1) + mutant: votable admission unrepresentable |
| fold writes payload only | `IntegratedAppFold` return type | Haskell types + mutant: group-aggregate return rejected |
| committed base + consequences atomic | `commitBaseChange` sealed hook | S28-1 rollback test: tentative base + failing hook → state AND log unchanged |
| refusal advances nothing | `foldIntegrated` keeps aggregate on error | S28-1: refused app event never appended; replay of accepted KEL never rejects |
| validate/fold agreement | single step shared | property: accept/replay never disagree |
| no client-decided conflicting authority | GroupView read-only, no second fold deciding | fence: necessary existing client type/API adaptations to keep `just ci` green are INCLUDED (ASK-authorized, adapt-only); broader Reactivegas UI + wholesale `Client/Fold.purs` deletion/redesign are NOT (belong to #84) |

## Budgets and supervision (binding)

Per initial #28 slice: ≤2 submissions; owner ≤8 substantive full build/gate
attempts total + ≤24 explicitly counted targeted probes; one fresh FULL
independent audit per candidate (first auditor Codex `gpt-6-astra/high`,
model+effort in live argv), ≤5 substantive audit builds + ≤20 targeted probes
per audit. Every failed setup/attempt journaled. No automatic ceiling raises —
return concrete workload/cost gap before exceeding. No parallel heavy builds.
Owner supervises immediate children only (ticket owner → its pair; epic owner
never touches commit owner/auditor). Every commit incl. rebase/amend
journaled. Every stop terminal or PARKED (exact wake) or BLOCKED (question).
Local-only audit reports, no external gist. Monitor only active roots so
archived noise cannot bury alarms; prove known-stale alarm reaches live owner.

## Open questions / dependencies enumerated (not invented)

- #68 V-2 landing → explicit rebind of approve path + revalidation.
- #76 composition wire (reachability/target/polarity/one-use) → Reactivegas
  side; kelgroups exposes the interface + closure evidence only.
- #75 R3.1 replay context → test input for S28-1, not a kelgroups default.
- #81 lifecycle (renounce/departure causes, retention) → Slice-B vote
  semantics are dependencies, not S28 scope.
- Upstream Lean gaps for #30 → enumerated in S30 contract, never invented.

## Resume record

- Epic owner pane `%532`, window `reactivegas:8` (renamed below),
  worktree `/code/kelgroups`, branch `main`, runtime root
  `/tmp/reactivegas/ms2/e-kelgroups-substrate/`.
- Launch: `muse --approve` (wrapper pins
  Pi/opencode-go/muse-spark-1.3-contributor/xhigh); verified live
  `ps` shows `/nix/store/.../pi/pi --provider opencode-go --model
  muse-spark-1.3-contributor --thinking xhigh --approve`.
- Next: ticket owner `t28-app-api` START, then G28-1 freeze + mandate.
