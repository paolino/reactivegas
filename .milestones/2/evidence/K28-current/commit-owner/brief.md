# Brief — commit owner `t28-app-api/commit-owner` (kelgroups #28, S28-1, RED-first)

Role: commit owner (accountable implementer). Worker ID: `commit-owner`.
Parent scope: ticket owner `t28-app-api` (Muse pane `%534`,
`reactivegas:8` `kelgroups-e29-t28-substrate`), runtime root
`/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/`.
Your runtime root: `.../t28-app-api/commit-owner/` (`brief.md` = this file,
`STATUS.md`, `questions/`, `answers/`, `inbox/`, `handoffs/`).
Worktree: `/code/kelgroups-issue-28`, branch `feat/28-generalize-app-api`,
base `368b596fef0b6d393c2ac7afc631d236c55d86d1` (clean, verified). You work
IN this worktree (same checkout the ticket owner supervises). No second
worktree, no hidden subagents. `draft=NONE` (no draft-tool family authorized;
never dispatch one).
Family/seat: Muse via `muse --approve` (= pi provider opencode-go, model
muse-spark-1.3-contributor, thinking xhigh, --approve). Same family as the
ticket owner under the standing milestone override permitting Muse ticket AND
commit owners; record `same-family-override=cited-standing-milestone-override`
in START. Auditors are never Muse; your auditor comes later, separately.
You are not alone in the codebase; do not revert edits made by others.

Required skill load chain (in order): `commit-owner`, `worker-protocol`,
`verification`, `invariants`, `gate-script`, `haskell`, `nix`, `lean4`
(read-only: read `/code/reactivegas/lean/KelGroups/` to conform, never edit
`lean/` anywhere). Plus `orchestrator-contract` §§ supervision/corrections
for interpretation only (you own no children).

## Authority and fences

Owned surface (ONLY these may change): `lib/KelGroups/{Event,State,Validate,
Fold,Types,Store}.hs` (+ `Bootstrap.hs`/`Server.hs` compile-fix-only if the
`GroupState` field addition forces it), `test/S28DemoApp.hs` + 
`test/S28AppApiSpec.hs` (NEW test-only files), `kelgroups.cabal` test wiring,
`test/Main.hs` spec wiring, `test/Generators.hs` positional-construction
adaptation for the new `pendingBase` field. READ-ONLY (never edit):
`/code/kelgroups-issue-28/gate.sh` (frozen G28-1 v3,
`GATE_SHA256=e358cc3884c2c60174a79afca6dd1a903aac639fc2ca867a97ec53984a5836d8`),
ticket contract r4 (`.../t28-app-api/handoffs/S28-1-CONTRACT-r4.md`),
`lean/`, `client/` (EXCEPTION: minimal adaptation ONLY if the library change
breaks `build-client`/`test-client` — adapt, never redesign `Fold.purs`),
`lib/KelGroups/Trivial.hs` (must keep compiling UNCHANGED),
historical API bodies (keep, mark `HISTORICAL-NON-PRODUCTION`, no new
responsibility). Forbidden: executables, new lib files, Reactivegas
economics/UI, vendoring, publication, release-please, issue/PR comments,
push (NO push, NO PR, NO merge — local signed commits only; the ticket owner
handles all remote motion).
Commit authority: local SIGNED commits only (`git -c commit.gpgsign=true`
is already configured true; verify with `git config commit.gpgsign`).
Journal every commit (SHA+subject) in YOUR STATUS. Stop conditions: scope
change without durable ruling; second family on one seat; gate edited to go
green; blanket test exclusion; push attempted; grandchild dispatched.

## Objective (S28-1)

Implement the frozen production API + test-only demo + six-group spec from
r4 §§4–6 so frozen gate v3 exits 0 on your candidate AND each of M1–M6 goes
RED for its row inside that same gate run. Full API text: read r4 §4 in full
(it is the mandate; key names repeated below for grep-ability, r4 governs on
any mismatch): `GroupView` (`gvMembers`, `lookupMemberInView`,
`isMemberInView`, `isAdminInView`); `DirectCommand.AdmitMember`;
`BaseMutation` (`RemoveMember`|`ChangeRoles`, exhaustive);
`BaseChange` (three evidence constructors); `IntegratedEvent`
(`IEDirect`|`IEPropose`|`IEApprove`|`IEApp`, distinct params);
`PendingBase` (`pbMutation` non-admitting); `GroupState s` (+`pendingBase`,
`appFold` holds AppState); `groupView`, `lookupPendingBase`;
`ReservedKey` + `validateDirectAdmission` (order notAnAdmin/reserved/exists)
+ `validateBaseMutation` (exhaustive) + `validateBaseApproval`
(over `pendingBase`); `IntegratedAppFold`, `BaseHook`, `IntegratedError`
(`IEValidation`|`IEApp`), `IntegratedResult` (`irState`+`irChange`),
`Integration` (`intReserved`, `intDigest`, `intProposalMutation`,
`intAppFold`, `intBaseHook`); `commitBaseChange` (sealed atomic);
`tryEnactBase`; `applyIntegratedEvent` (SINGLE shared step);
`foldIntegrated` (keeps aggregate on error); `openIntegratedKEL`,
`appendIntegratedEvent` (validate-then-append; refusal persists NOTHING).
Demo (`test/S28DemoApp.hs`): `DemoState{ demoCounter :: Int, demoLog ::
[Text] }`, `DemoEvent` (`DemoAdd Int`|`DemoReset`|`DemoNoop`), `DemoError`
(`DemoNotAdmin`|`DemoNegative`|`DemoHookRefused`), `DemoProposal`
(`DemoRemove`|`DemoChangeRoles`), `demoProposalMutation`, `demoDigest`,
`demoReserved`, `demoAppFold` (signer via view; `DemoReset` needs
`isAdminInView` else `DemoNotAdmin`; `DemoAdd n<0` → `DemoNegative`),
`demoBaseHook` (refuses `MemberRemoved protectedKey` with
`DemoHookRefused`, else logs visibly), `demoIntegration`; import
`applyIntegratedEvent`, `emptyState`, `IntegratedEvent`, `Integration`,
`IntegratedError`, `IntegratedResult` (M1 boundary-use requirement).
Spec (`test/S28AppApiSpec.hs`): EXACT six describes from r4 §5; EVERY example
on ONE line as `it "<name>"`/`prop "<name>"` (8-space indent; names describe
the witness behavior); agreement traces MUST include non-member and
domain-invalid events; QuickCheck uses standalone generators (no Arbitrary
instances). Wire into `kelgroups.cabal` + `test/Main.hs`.

MANDATED harness spellings (implementation requirements; leg-5
preconditions verify at runtime, fail closed): (H1) app-route guard
literally `if isMemberInView signer view then`, count==1 in Fold.hs; (H2)
foldIntegrated refusal arm literally `Left _ -> gs`, accumulator named
`gs`, count==1 in its block (fallback H2b `either (const gs)` iff H2 absent,
exactly one of the two); (H3) `commitBaseChange` signature adjacent to its
equations; (H4) BaseMutation block with a `ChangeRoles Text (Set Role)` arm;
(H5) appendIntegratedEvent success block mirroring historical `atomically $
do` with a `gs` binding and success write literally `writeTVar (stateVar
store) (irState result)`; (H6) demo file per above incl. `demoIntegration ::
Integration DemoState DemoEvent DemoProposal DemoError`.

## Procedure (RED-first; brief governs, gate decides)

1. START in YOUR STATUS first (pane, cli=muse + harness/provider/model/effort,
   same-family-override cited, base HEAD, gate hash quoted). No START = never
   started; nothing you do counts.
2. RED bundle on pristine base (legs 1–7 WILL refuse leg 5 at entry — that IS
   the expected base RED): run `./gate.sh` from the worktree root EXACTLY
   once on the clean base; capture the full output + sha256 (charge: 3
   builds). Expect legs 3,6,7 green + leg-4 inventory 0 (historical exit-0:
   absence, not breakage) + leg-5 entry refusal. Save the log to YOUR
   `handoffs/RED-base-gate.log` (+sha256 in STATUS).
3. Write the failing-first evidence: author `test/S28DemoApp.hs` +
   `test/S28AppApiSpec.hs` (+ minimal wiring) referencing the frozen API;
   run `cabal test` (or the gate's leg-4 command) and capture the
   Not-in-scope compile failure naming frozen API names (absence, not typos
   — read it yourself and say which names are missing). Commit as local
   signed RED commit `test(#28): RED S28-1 integrated app-api properties`
   with body naming the absent API + `handoffs/RED-commit.log` (+sha256).
   Charge: RED phase = 4 builds: (i) base leg-3 build, (ii) base leg-4 full test, (iii) base leg-6 just-ci, (iv) RED-commit Not-in-scope run on the RED tree. (ii) and (iv) cannot merge (different trees: pristine base vs RED tree). GREEN = 9. SLIM-final (ticket owner) = 3. Campaign total 16 of 16 EXACTLY, headroom 0: fits the ruled 16-cap with zero margin; any further overrun BLOCKS (ask, never absorb). The ticket owner amends the campaign table append-only at RED time. Record every invocation (command+exit+cause) in YOUR STATUS.
4. GREEN implementation in the owned surface only (historical API kept +
   marked; Trivial untouched; H1–H6 spellings honored; fourmolu/hlint clean
   — run `just format`/`cabal-fmt` equivalents via the devshell BEFORE
   committing so leg 6 stays clean).
5. Full `./gate.sh` on the committed candidate (charge: 9 builds): exit 0
   REQUIRED, all six kills quoting their witnesses, tree restored
   hash-verified, evidence teed to `handoffs/evidence/` by the gate itself.
   Save NOTHING extra (gate evidence is the record); record log hashes +
   gate exit in STATUS.
6. PROOF-COMPLETE in YOUR STATUS with: base SHA, RED-commit SHA, candidate
   SHA, RED base-gate log+sha, RED-commit log+sha, GREEN log hashes (leg
   evidence dir listing), ANCHOR-ATTEST (grep -n hits proving H1–H6 +
   A6-boundary-imports in the candidate), spend ledger (builds/probes used
   vs RED≤4/GREEN≤9), residual doubts. Then PARK idle (write-idle,
   write nothing further) and await ticket-owner instruction. NO second
   GREEN run unprompted (ONE submission; findings go UP for disposition).
   Handoff essay: `handoffs/S28-1-SUBMISSION.md` (same content, durable).

## Verification &-Jean budgets (binding)

Fresh evidence before every claim (`verification`); checks must be able to
fail (`invariants`: executable subject, quantified extent, reachability —
every new property has its M-kill). Owner envelope: RED ≤4 builds,
GREEN ≤9 builds, ~0 probes; journal every invocation (command+exit+cause).
Campaign total 16 builds / 24 probes incl. ticket-owner slim-final (3) and
prior recon (0/4 recorded standing) — your overrun BLOCKS (ask, never
absorb). No parallel heavy builds.

## Questions / inbox / reporting

Blocked → `questions/Q-NNN-<slug>.md` + `BLOCKED Q-NNN`, park; the ticket
owner owns answers + delivery (`answers/A-NNN`), you record `RESUMED`.
Corrections arrive in YOUR `inbox/` (+ wake); acknowledge `NOTE NOTE-NNN
read` before relying on them. Check unread inbox before every phase, every
expensive command, before freezing evidence, before COMPLETE. Report LOCAL
FILES ONLY (your STATUS + handoffs); never type into any other pane, window,
or chat composer. Liveness signal = YOUR STATUS journal. Monitor staleness
of nothing (you own no children). Stop: two parents; scope change without
ruling; gate/contract edited to go green; push/PR/merge attempted or asked;
grandchild dispatched; worker called running without START.

Brief sha256 (this file): recorded by the ticket owner at dispatch; quote it
in START. Gate v3 sha256: e358cc3884c2c60174a79afca6dd1a903aac639fc2ca867a97ec53984a5836d8 (read-only; verify before first run with the leg-2 procedure by eye).
