# Epic map — `paolino/kelgroups#29` (upstream substrate for Reactivegas milestone2)

Epic owner Muse (Pi/opencode-go/muse-spark-1.3-contributor/xhigh), pane `%532`
(reactivegas:12.1 after root reorganizations; runtime root
`/tmp/reactivegas/ms2/e-kelgroups-substrate/`).
Parent: milestone desk `%510`, runtime `/tmp/reactivegas/ms2`.
Brief: `artifacts/ASK-kelgroups-e29-substrate.md`
sha256 `399d9268904fbdc5bf9932bfc4e44e0acea432aac6c042b0b9c30b435379c36d`.
Authority: explicit operator authorization 2026-09-05 ("yes let a team work on
kelgroups") for limited sibling `#29/#28/#30` lane resolving reactivegas#73.
Resurrection-grade; rebuilt from durable state only.

## Operative baseline (2026-09-06 — current; intake below is HISTORICAL)

#28 LANDED (`933e385d`, PR#32 guarded squash, post-merge CI+Release SUCCESS,
issue #28 CLOSED post-scope-verdict). Operative base: kelgroups `main` @
`933e385d` (S28 interface + F1/F2 repairs verified on main). #30 REMAINS
UNIMPLEMENTED — next action: #30 commission ruling on mandate v2
(`handoffs/T30-MANDATE-v2.md`) + requirement map
(`handoffs/T30-REQUIREMENT-MAP.md`) + proposed envelope (PROPOSED, not
granted). #29 + Reactivegas#73 OPEN. Ticket worktree
`/code/kelgroups-issue-28` @ branch `fix/28-r2-refusal-order` (post-merge
state; do NOT reuse blindly for #30 — fresh worktree/branch at dispatch).

## HISTORICAL intake record (2026-09-05; superseded observations kept for provenance)

Base (then): `/code/kelgroups` clean `main 368b596fef0b6d393c2ac7afc631d236c55d86d1`
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
| S28-1 | kelgroups `#28` | `t28-app-api` | first slice (integrated API + test-only demo; RED `570fe4a`, GREEN `84a2dae`) | accepted Lean | superseded: terminal findings (F1/R2/R6 + F2/R4 BLOCKING) | CLOSED into S28-R1 |
| S28-R1 | kelgroups `#28` | `t28-app-api` | repair campaign (owner 13/16 + audit 10/12+16/24, ONE submission): submission `3af3d06`, terminal findings (F1/R2/R6 + F2/R4 BLOCKING, 3 OPEN + 3 BLOCKED) | superseded | CLOSED into S28-R2 |
| S28-R2 | kelgroups `#28` | `t28-app-api` (owner 26/26 + 4/24 + 2/4; auditor 11/12 + 22/24; ONE submission) | repair + audit campaign: submission `ab25cd1` (tree `e52114c1`), gate v10.2 exit 0, SLIM identical, terminal AUDIT-PASS `d1d19060` (finite scope); MERGED as `933e385d` 2026-09-06 (parent `368b596`, landed tree == audited); PR#32; post-merge CI + Release SUCCESS | dual acceptance filed (ticket + epic) | MERGED + issue #28 CLOSED (no comments); #29 + Rg#73 OPEN |
| S30 | kelgroups `#30` | TBD (mandate draft `handoffs/T30-MANDATE-DRAFT.md` filed, budgets PROPOSED 14/24+12/24, no execution grant) | accepted substrate vote interface + closure evidence (`Question`/`Ballot`/`Verdict`/`Threshold`-param/`ClosureRecord`/`foldVote`/`sweepClosures`); missing upstream Lean semantics enumerated as deps, not invented | S28 LANDED interface + #68 V-2 state at dispatch (explicit rebind) | mandate + costed plan ready for ruling; NOT dispatched |
| S29-close | kelgroups `#29` | epic owner | epic acceptance handoff for reactivegas#73: S28 LANDED; delivery tickets #33 (demo, blocked #30) + #34 (release+notes, blocked #33+#30) FILED 2026-09-06 (no dispatch/publication authority); #29 checklist updated (#28 landed-checked, zero comments) | S28 landed; #30 pending | interface packet to desk on #30 landing; publication separately gated | mapped, containers filed |

Merge order (executed): S28-1 → S28-R1 → S28-R2 MERGED; next S30 → S29-close. #28 CLOSED 2026-09-06 post-scope-verdict (no comments). No parallel heavy
builds inside the ticket. S28-1 is the one coherent #28 slice carrying the FULL append/replay/persistence boundary; S28-2 is a contingent upper bound, never a place to defer a promised S28-1 proof (no double-counting one boundary as delivered-now and deferred-hardening).

Epic artifact: per `resolve-epic` a runnable exists from first behavior child.
The slice-proving vehicle is the ticket's test-only nondegenerate instance (proves the API slice, nothing more). Epic #29's requested runnable demo (child 3) REMAINS a distinct owned deliverable: a follow-on ticket with its own bounded contract after S28 establishes the interface — local demo implementation inside this agreed epic is authorized once that contract is set, no prohibition invented here. The v2 major release + downstream consumption notes (child 4) are likewise registered owned remainder; publishing/release stays separately gated. D2/D3 stay with sibling `e-haskell-impl`; this lane
sends it NO instructions, only returns the accepted interface packet to the
desk.

## Contract registry

| contract | producer | consumers | stable version | release signal | enforcing check |
|---|---|---|---|---|---|
| `KelGroups.Integration` surface (IntegratedAppFold/BaseHook/IntegratedEvent/IntegratedError/IntegratedResult/Integration/direct-only admission/pendingBase/GroupView/commitBaseChange/foldIntegrated + validators) | this lane (S28) in `paolino/kelgroups` — LANDED at `933e385d` (PR#32, post-merge CI+Release SUCCESS) | reactivegas D2/D3 via #73 | v2-major line (release itself separately gated) | `just ci` on main; terminal audit d1d19060 + inventory 3f352562 |
| substrate vote interface + closure evidence | this lane (S30 — mandate v2 + requirement map filed 2026-09-06, NOT dispatched) | reactivegas #76 (consumer binding stays there) | NONE yet | S30 landing (separate authority) | S30 contract at commissioning (witnesses + can-fail mutants, full CI, fresh audit) |
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

---

## 2026-09-06 — T30 synthetic campaign outcome (appended by the Opus epic owner; nothing above altered)

Epic ownership transferred to Opus at `%532` under `artifacts/ROLE-SUBSTITUTION-OPUS-20260906.md`; live map
maintenance continues here. Current epic runtime `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/`.

**S30 row update.** Ticket owner is now `t30-contract-opus-20260906` (`%572`, `claude-opus-5[1m]`/high);
the Muse `t30-contract` owner is terminal and its root is preserved read-only. State: contract preparation
delivered (r1–r8 + INV1 assessment + PF8R packet + INV2 result); `#30` itself UNIMPLEMENTED and UNGRANTED.

**Bounded synthetic campaign: CLOSED, budget exhausted.** Invocation 1 (pf8) `SUITE FAIL
setup-failures=0 mispredicts=17`; invocation 2 (pf8r, under epic preflight binding) `SUITE FAIL
baseline=BROKEN setup-failures=0 mispredicts=11`. Aggregate 4 of 4 consumed; product builds 0.
Established as instrument properties: FIX-1 both directions, FIX-4 (0/31 non-empty stderr), FIX-5 taxonomy
precedence, FIX-6 provenance refusal, plus A21 (first `FINAL: PASS`), A28 (`0-overlay-base` negative
control) and A7 (D-3→D-4 end to end). Suite verdict stays FAIL: the baseline gate is upheld.

**Contract state:** frozen at r8. TAXONOMY-v2 accepted **for the single rerun only**; contract §8 and the
command-map taxonomy block are NOT amended, and a separate versioned re-freeze is required before r9 is used
for `#30` acceptance. Owner 26/24 and auditor 25/24 remain PROPOSALS.

**Proof row for the S30 gate:** the drift leg's synthetic layer is demonstrated only in the parts named
above; count integrity, exact-line vs substring, row uniqueness, stale-product RED, no-inheritance,
per-REQ exact-success and baseline GREEN remain UNESTABLISHED, and the compiler layer is untouched.
Next prerequisite: P1 = B3 (1 owner build), P2 = B22a + B22b (2 owner builds) — **3 product builds**, inside
the ungranted owner budget.

### Invariant ledger — new recurring shape

| shape | instances | mechanism |
|---|---|---|
| a check whose subject is not the artifact under test | (1) `setup-failures=0` counted structural setup gates while the fixture *content* was defective (pf8 inv1); (2) the FIX-2 template invariant asserted marker-older-than-`.hi` on `fx/tmpl` while the leg reads the copied `cases/<id>` tree, and `cp -r` discards the mtimes it asserted on (pf8r inv2) | **enforced: NONE.** Both instances passed truthfully and proved nothing. Consolidation candidate for the `#30` gate: every assertion must name the tree the subject actually reads, and be falsified against it. |

### 2026-09-06 (later) — recovery slice pf8r2: synthetic layer demonstrated

Exceptional fifth invocation (desk grant, aggregate ceiling 5, no retry reserve) executed once under epic
freeze: `SUITE: PASS (baseline=GREEN setup-failures=0 mispredicts=0)`, runner-exit 0, 32 as-predicted / 0
mispredict / 0 setup-failure. **The gate leg was byte-identical (`69c529ca…`) to the one that failed in both
prior invocations** — only the runner's two authorized deltas changed — so the green is evidence the fixture
was repaired, not that the checks were relaxed. Every deliberate negative control still fired: A23a
(`BUILD_MARKER absent`), A10, A26, A27 (`4-provenance`, forbidden string absent), A28, and A24 in both
directions. Aggregate 5 of 5; product builds 0.

**S30 proof row now reads:** the drift leg's *synthetic* layer is demonstrated (count integrity, uniqueness,
exact-line vs substring, per-REQ exact-success, stale-product with intact control, no-inheritance, baseline
GREEN, plus the instrument properties from invocation 2). The *compiler* layer is untouched.

**Structural finding for the `#30` gate design — carry it, do not rediscover it.** `M22b` (independence of
the source/byte-hash tripwire from the `.hi` tripwire) is **unreachable in any synthetic harness of this
design**: in live mode the hash tripwires read through the frozen oid so a source edit cannot fire them, and
they are falsifiable only in overlay mode, which skips D-3 entirely. The two channels are never both live in
one mode. This makes **P2 (B22a + B22b) the only way to establish M22b, not merely the cheapest** — a design
property, not a fixture defect.

**Prerequisite row (unchanged in content, sharpened in force):** P1 = B3 one owner build; P2 = B22a + B22b
two owner builds; **3 product builds inside the ungranted owner budget.** The harness layer and the compiler
layer are distinct and separately costed — a compilation grant does not repair or establish the harness, and
the harness green establishes nothing about a compiler.

Invariant-ledger shape "a check whose subject is not the artifact under test" now has its **enforcing
mechanism**: the FIX-7 assertion was moved to `case_env`, acting on the copied case tree the leg consumes,
asserting only that the re-stamp took effect and guarded on marker presence so no negative control is
setup-failed before the subject sees it. `enforced: pf8r2 runner FIX-7` (was `NONE`).

### 2026-09-06 (later still) — `#30` FULL IMPLEMENTATION COMMISSIONED

Desk grant `NOTE-007`. **S30 state: planned → ACTIVE.** Binding mandate
`e-kelgroups-substrate-opus-20260906/handoffs/T30-COMMISSION-MANDATE-v4.md` sha `173e0b5fe4af108a…`,
which carries an explicit **superseded-statement table** (r8 single-slice → the approved decomposition;
Muse *ticket*-owner → Opus, Muse as *commit* owner; r8 leg → demonstrated r9 leg `69c529ca…`;
"fixture unexecuted" → pf8r2 PASS; TAXONOMY-v1 → v2) so no worker adjudicates which text governs.
All superseded texts preserved; product semantics not widened.

**Owned surface / seats:** ticket owner `t30-contract-opus-20260906` (`%572`, Opus `claude-opus-5[1m]`/high);
commit owner **one Muse**; **one fresh Codex `gpt-6-astra`/high auditor per admitted submission**
(never Muse/GLM/Claude auditing; Grok only under the one-family-seat cap).

**Budget row:** owner **28 substantive / 22 probes** cumulative from product spend 0 (26-unit envelope +
`S30-0a`/`S30-0b` as named additions, not replacements); author max 2 submissions on the same cumulative
ceiling, no separate repair pool; auditor **25/24** across max 2 fresh audits. Synthetic 5/5 closed, no sixth.

**Merge order:** S28 (merged) → **S30** → S29-close. Merge authorization stays at the desk; this lane may
push and open a **draft** PR only after full local acceptance plus a qualifying independent audit, with no
closing keywords for `#29`/`#73`.

**Contract registry update — substrate vote interface row:** producer this lane (S30, ACTIVE); enforcing
check is the versioned TAXONOMY-v2 contract + the **demonstrated r9 leg**, whose *synthetic* layer is proven
and whose *candidate* rows become evidence only on the actual Vote modules. Recorded limits carried into the
gate: `1-fileset-hs` is a D-1 `fail`, so the frozen leg cannot produce a meaningful live run until the first
candidate exists; and **an unexported source edit is not guaranteed to leave a whole `--show-iface` dump
unchanged**, so channel independence needs a concrete can-fail observation or an honest design finding —
never a fabricated stability or normalized-away data.
