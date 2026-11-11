# S28-1 frozen ticket contract r5 — kelgroups #28 (G28-1 v4)

Ticket owner `t28-app-api` (Muse `%534`), epic `paolino/kelgroups#29`
(`%532`), runtime `/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/`.
Brief `df98d099…`. Pinned origin/main `368b596…`; frozen RED base
`570fe4a68f510fad3c9912ea59c1e492f3e11740` (test(#28) RED, parent 368b596;
worktree HEAD == RED, clean at freeze). Worktree
`/code/kelgroups-issue-28`, branch `feat/28-generalize-app-api`. Frozen
2026-09-05 per NOTE-004 + child Q-001 + A-001 §3 binding rule, before ANY
GREEN mutation run. This r5 SUPERSEDES r4 for execution; r1–r4 (+ backups
`e164a121`, v2, v3) PRESERVED unmodified. V-2 settled/unlanded: rebind after
#68, never anticipate.

Governance: r4 stands EXCEPT as amended below. Section titles name the
governing version; on any conflict this r5 wins for execution. No section
here changes the accepted Lean, the S28-1 objective, fences, the
vote-machine S30 boundary, or the one-submission/audit regime.

## NOTE-004 dispositions (all four + binding rule + D5)

D1–D3 ruled same-requirement mechanical (child Q-001 + epic NOTE-004;
verified at source by both owners): corrected spellings/splices below, no
desk round per A-001 §3 (granted). D4: GREEN on RED `570fe4a`, no fresh RED;
absence stands (RED logs `2b64d6bf…`/`d5b0a1e2…`, hashes verified); spend
FINAL below. Binding rule applied: property + operation + failure class
frozen here; concrete locations bound to ACTUAL RED bytes in
`handoffs/MUTANT-BINDING-570fe4a.md` (versioned/hashed pre-GREEN-mutation);
production locations TBB with ANCHOR-ATTEST + leg-5 preconditions (fail
closed); same-requirement splice corrections need no desk round (granted),
but re-bind (new binding version + hashes) before execution. D5 (founding
deadlock, ticket-owner finding, provisional-pending-desk-confirmation):
ruled same-requirement per D1 precedent (settled Lean text, undelivered
interface, contained blast radius); confirmation invited via STATUS;
overrule → contained repair (Store.hs open fn + 3–4 Store tests + r5 text),
no campaign invalidation.

## §D1 — voted-constructor renames + mapping appendix (r4 §4/§6 amended)

Cause: `Event.hs:49/57/61` historical `Proposal` arms
`IntroduceMember/RemoveMember/ChangeRoles` share the module with frozen
`BaseMutation` arms → GHC duplicate-declaration (impossible as frozen).
Full scan (RED bytes): every OTHER frozen-new name is absent in its target
file (Event/Types/Validate/Fold/State/Store all 0 hits) — the clash is
exactly these two arms. Ruling: historical keeps names; NEW arms SUFFIXED:
`RemoveMemberVoted`, `ChangeRolesVoted` (shapes `Text` / `Text (Set Role)`
unchanged). Lean↔Haskell map: Lean `BaseMutation.removeMember key` →
`RemoveMemberVoted Text`; Lean `.changeRoles key roles` →
`ChangeRolesVoted Text (Set Role)`; every other frozen name is spelled
identically to the Lean (reason: GHC2021 module-level constructor namespace
vs Lean per-inductive namespace). H4 SUPERSEDED by H4' (new arm spellings;
demo/spec/generator/test-name refs follow; historical untouched). M4 v4
targets `ChangeRolesVoted Text (Set Role)` in the BaseMutation block;
exhaustiveness targets `enactMutation`/`validateBaseMutation` unchanged;
mutant ctor `AdmitMemberVoted Text Text (Set Role)` stays fresh (verified
absent repo-wide at freeze).

## §D2 — M6 arity fix (r4 §6 M6 cell amended)

Backdoor takes two patterns (`var newApp`) so its type takes two arrows:
`unsafeSetAppStateSTM :: TVar (GroupState s) -> s -> STM ()` (STM-disciplined;
`TVar`+`GroupState` already imported per A8; `STM` added by the splice at
anchor A8b `^    , writeTVar$`). Same requirement, same failure class
(one-splice fix + one import line). v4 splice: export insert + STM-import
insert + backdoor append (WITH signature) + H5 rewire; splice count == 4;
kill unchanged (authority replay-equality quoting an outside verdict).

## §D3 — M1 format-robust preconditions (r4 §6 M1 cell amended)

v3 single-line precondition killed by fourmolu (actual RED bytes
`S28DemoApp.hs:144-145` split `demoIntegration` / `:: Integration …`,
quoted in BINDING). v4 preconditions: `^data DemoState` + `^data DemoEvent`
name lines; FLATTENED match (`tr '\n' ' '` then
`demoIntegration +:: +Integration DemoState DemoEvent DemoProposal
DemoError` — matches the actual split); boundary mentions
(`applyIntegratedEvent`, `emptyState`); freshness
(`! _m1_boundarySeparates`). Splice text unchanged (boundary use
`_m1_boundarySeparates` + unification kill). The signature stays a REAL
declaration — compiler failure is the proof; grep only applies it.

## §D5 — founding-members fix (r4 §4 openIntegratedKEL amended; NEW)

Deadlock proof (all four routes enumerated on RED+base bytes against
Validate.lean: no bootstrap arm; founding via guarded initial aggregate):
from empty members, direct/propose/approve/app each require an
admin/member signer — the first member is unobtainable; frozen
`openIntegratedKEL … -> s -> FilePath` cannot express founding, so the
durable-accept, domain-invalid, and atomic#3 Store tests are unwinnable as
frozen. Freeze defect under settled text (r4 §4 states the founding rule).
Ruling H7: `openIntegratedKEL :: (FromJSON s, FromJSON e, FromJSON bp) =>
Integration s e bp err -> GroupState s -> FilePath -> IO (KELStore s)`
(initial FOUNDING AGGREGATE); fresh db persists founding (`founding` table)
+ starts from it; existing db loads founding + REQUIRES passed==loaded else
IO fail (lying-founding refusal witness); rows replay over founding. No
bootstrap arm (Lean-excluded). Gate-untouched (no leg names its args).
GREEN: demo helper `foundingDemo :: GroupState DemoState` (admin-key-1 admin
+ `DemoState 0 []`, record construction); Store tests open with it (drop the
dead gs1/_ scaffolding); pure tests keep `demoInitialState`/`gsWithAdmin`
(`foldIntegrated` still takes `s` — UNCHANGED). Blast radius on overrule:
Store.hs open fn + 3–4 Store-test call sites + this text (contained).

## §3 spend FINAL (append-only; all caps binding)

Standing: 0 builds / 4 probes (recon p1–p4; p5 ghc-no-code probe below →
0/5). Sunk (owner journal verified): RED phase 4B (base-gate legs 3,4,6 =
3B + RED-commit Not-in-scope run = 1B). v4-verify (mine): 0 builds
(M1-applicability quotes + M4-awk synthetic dry-run + M6 export/import
dry-run on RED copy + backdoor fourmolu-parse + ghc-no-code single-module
probe p5 + freshness baselines + extraction validation) + enumerated
charge-0 items. GREEN envelope 9B (owner). SLIM-final 3B (mine: legs
1,2,2b,3,4,6,7 — FULL slim per ruling "9+3 preserved"; identical-envelope
`HEAD^{tree}` proof required; any change → re-establish + full audit of
actual final, else BLOCK). TOTAL 4+0+9+3 = 16/16 EXACT, headroom 0. Probes:
recon 4 + p5 + dev ≤14 (trigger 12) = ≤19/24, headroom 5. Formatters
charge-0 RATIFIED (scans only; format-before-commits). Dev rule: NARROWED
invocations only (lib/component single-target, single-file no-code,
--match), each journaled with scope; whole-project outside gate legs
FORBIDDEN without pre-approval. BLOCK triggers: any build beyond these
rows; dev probes ≥12 (re-plan); splice re-runs beyond GREEN leg-5.
Auditor envelope 12/24 separate (pre-launch written reconciliation:
cold prerequisites + full gate + mandatory CI incl. nested calls; owner
kills never inherited; no auto raises).

## H-mandates (H1-H3/H5-H6 stand; H4'; H7 new; two-layout format rule)

H1 app-guard `if isMemberInView signer view then` (×1); H2 refusal arm
`Left _ -> gs`, accumulator `gs` (×1 in block), fallback H2b
`either (const gs)` (exactly one present); H3 commitBaseChange
signature+equations; H4' BaseMutation arms `RemoveMemberVoted`/
`ChangeRolesVoted` (+ all refs); H5 success write
`writeTVar (stateVar store) (irState result)` in `atomically $ do` shape
with `gs` binding; H6 demo file per §5 + boundary imports; H7 founding
shape per §D5. Spec format: same-line `it/prop "name"` OR fourmolu-split
`it|prop` + indented `"name"` line — NOTHING else (registration depends on
it; violations fail closed). Example names describe witness behavior.
Leg-5 preconditions verify every mandate at runtime (fail closed).

## §6 killers v4 (M2/M3/M5 operations stand; M1/M4/M6 per §D2/D3/D1)

M1: flattened preconditions + freshness + boundary-use splice; kill =
unification error naming DemoEvent+DemoState+applyIntegratedEvent (parse
excluded). M2: H1 bypass (`if True`, tail preserved); kill = Failures:
naming a registered rejecting-step example. M3: always-commit stub; kill =
Failures: naming a registered atomic-hook example. M4: insert
`AdmitMemberVoted` in BaseMutation block (freshness pre-checked); kill =
exhaustiveness error quoted (parse excluded). M5: H2/H2b selection (exactly
one, ×1) → crash-on-refusal; kill = Failures: naming a registered agreement
example. M6: export+STM-import+STM-backdoor+rewire (count 4); kill =
Failures: naming a registered authority example. Standing rule: empty
failure sections, crashes, parse/setup/dependency errors NEVER count
(MUTANT-FAILURE/INCONCLUSIVE → fix mutant, row not passed).

## Gate v4 summary (`GATE_VERSION="G28-1 v4 (r5)"`, FROZEN_BASE=`570fe4a…`)

v3 mechanics carried (evidence teeing+hashes, PIPESTATUS, entry-refuse,
hash-verified restore + `exit 3` abort, kill_check over registered names,
exact pins + exit-first + stop-before-spend, cold measurement) with v4
deltas: M1/M4/M6 splices+preconditions per above; leg-4 robust two-layout
registration extractor + per-row ≥1 registered enforcement + two-layout
total cross-check; FROZEN_BASE = RED `570fe4a…` (pinned origin/main
`368b596…` recorded alongside) + ancestry enforcement
(`merge-base --is-ancestor`, fail closed — GREEN must descend from RED);
leg-4 RED note names the two-step absence (pristine-base legs + RED-commit
Not-in-scope owner-verified). Pinned origin/main `368b596…` in meta.

## v3 spelling limits (named explicitly per NOTE-004 D4; superseded above)

v3 M1 single-line precondition (fourmolu-split kills it); v3 M6 IO-backdoor
arity (setup failure at STM site); v3 M4 duplicate-name insert (parse
failure, proves nothing about exhaustiveness); v3 M5 Lean-arrow anchor
(speculative spelling). All superseded by v4 bindings; preserved in
gate-v3.sh.backup for the record.

## GREEN test refinements (directed, owned test surface, no new files)

R-a atomic#3 (`S28AppApiSpec.hs:250-272`): assert Left DemoHookRefused +
gs1==gs0 + log length+bytes unchanged + replay confirms (remove pure-sink).
R-b agreement props (`:315-336`, both tautological `True`): real per-prefix
comparisons with forced evaluation over traces incl. invalid (keep
generators). R-c replay-equality it (`:359-365`): compare
foldIntegrated-over-decoded-rows vs live with shouldBe. R-d self-review ALL
examples (no pure-sinks/both-True/unforced lets; drop rejecting dead gs1/_
~:169-190). R-e `*Voted` refs everywhere (arms, matches, generator, example
names). R-f founding setups per §D5. Kill-closure: M2→rejecting non-member
test; M3→atomic#2 (+#3 after R-a); M5→agreement after R-b; M6→replay-equality
after R-c; M1/M4 build-level. Verified at GREEN leg-5 (fail closed) +
submission review + audit.

## Appendix R (REGISTERED actuals from RED bytes via v4 extractor; EXECUTED/KILLED TBD-GREEN)

distinct 3 / rejecting 3 / atomic 3 / direct-only 3 / agreement 3 / authority
4 = 19 registered (row files in BINDING record). Discovered ancestors per r3
appendix R (ValidateSpec:147/158 historical membership; FoldSpec:45-46;
StoreSpec:134/187/340; StoreInvariants:59; ValidateSpec:175ff/239ff;
Invariants:77ff; TransitionInvariants:122; ServerSpec+E2E single-path;
remote CI green 368b596). Known-vacuous spots directed above (R-a/b/c).
Regression: full historical suite must stay exit-0 (any RED = regression
finding, not a kill).

## Residual risks

D5-confirmation-pending (contained rework stated); headroom-0 operations
(any surprise build BLOCKS); hspec-render drift (parsing-only correction
under granted authority + re-freeze + re-falsification before its mutant
executes); V-2 rebind after #68 (out of scope).
