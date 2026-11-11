# S28-R1 SUBMISSION — commit-owner-s28r1 (repair implementation, ONE submission)

Worker: commit-owner-s28r1. Seat: pane %545 (fresh process, S28-R1 campaign).
Family: Muse. draft=NONE. Authority: ticket owner t28-app-api per NOTE-024.
Date: 2026-09-05. Parked idle after this file; no second GREEN unprompted.

## SHAs (base / RED / start / candidate / gate / mandate)

- Base (accepted origin/main): `368b596fef0b6d393c2ac7afc631d236c55d86d1`
- Contextual RED: `570fe4a68f510fad3c9912ea59c1e492f3e11740`
- S28-R1 start (zero acceptance): `84a2daea1db81b1baf73c73a7874dcd68ce9f4b2`
- Candidate (this submission, signed, local only):
  `3af3d065b7d0c54f03d89b8c05d8b8acd4a53db4`
  (parent `84a2dae…`, 2 files, +111/−29), tree
  `b4eb37f2187c39db4950072e309c0d125d5c27af`
- Gate: `G28-1 v9 (S28R1-plan)`, normalized `3c433eff…`, full `dcbc8c2b…`,
  FROZEN_BASE `84a2dae…` (candidate descends; ancestry for leg 2 holds).
- Mandate: S28-R1-COMMAND-PLAN (frozen 2026-09-05; r5/addenda as history).
- No push, no PR, no merge, no remote write. Branch
  `fix/28-r1-conservation-effect`, tracked-clean at commit.

## RED-equivalence (inherited executed evidence, no fresh RED runs)

RED-first is satisfied by the frozen inherited record cited from the brief:
F1 — P2 exit-1 (`8d0fb0ff…aa23`); F2 — P4-compile (`8271c00c…45417e`) +
P5 exit-1 (`907df80e…cfd15`) + P6 exit-0 (`96fa404a…07ee4d`) + P7 exit-0
(`787e7664…101a7e`). Fresh reruns would be duplicate validation toward no
new information. Proceeded directly to GREEN implementation per mandate.

## F1 repair — mechanism (Store.hs only)

New per-store `storeAppendLock :: MVar ()` (both constructors initialize;
no external constructors exist — Server holds opaquely, tests use
accessors). `appendIntegratedEvent` is now: pre-compute payload bytes →
`evaluate` the strict-Text rendering → `withMVar` lock → fresh `readState`
→ `applyIntegratedEvent` → fresh `kelLength` → SQL INSERT → atomic TVar
commit of `irState` + `n+1`. Refusal persists nothing and touches no state
(unchanged). SQL failure propagates to the caller before any TVar write
(unchanged — lock released by `withMVar`). Sequential controls unchanged;
success returns never weakened; no events dropped. `seq_no` is now drawn
from the locked length read, so overlapping successes order distinctly.

Why encode-before-lock: the auditor's serialization rendezvous lives in the
test-only `ToJSON` rendering invoked by `encode`. Holding the lock across
`encode` would deadlock the controlled overlap (second caller blocks on the
lock before reaching its barrier entry while the first blocks in its
barrier waiting for the second). Production rendering is pure, so forcing
it first is semantically neutral; `evaluate` on strict `Text` (unlifted
payload array) runs the barrier exactly once per call, before any lock wait.

New-shape note (M6): the success-write literal is preserved byte-identical
(`writeTVar (stateVar store) (irState result)`, still exactly ×1, still the
sole verdict path inside its `atomically $ do` block with the length write
adjacent) but now nests one level deeper (inside `withMVar` + case). Full
v9-M6 predicates re-verified on committed bytes: closeKEL-export ×1,
writeTVar-import ×1, `^appendIntegratedEvent` present, success literal ×1,
no `unsafeSetAppStateSTM`, and `gs` in scope at the rewired line — the v9
M6 splice remains mechanically applicable. Rebind decision is the ticket
owner's at BINDING-GREEN (pre-granted same-requirement authority, v9.x
re-freeze if needed); this submission forces no variant.

## F2 repair — strengthened effect tests (S28AppApiSpec.hs, row 4 only)

Inside the existing `S28-1 direct-only admission` describe, same-line
layout, descriptive names: the vacuous constructor-enumeration prop is
replaced by a REAL subset check (`Map.keysSet` post `Set.isSubsetOf` pre
over `enactMutation` applied to every `genDemoProposal` mapping — both arms,
present key `admin-key-1` plus absent keys `member-key-2`/`outsider-key-9`);
plus three deterministic its — change-roles/absent (keys equal AND lookup
`Nothing`), change-roles/present (keys equal AND still member), and
remove/absent (keys equal AND still absent). One wrong effect defeats the
mandatory check: the M7 shadow insert fails the absent change-roles it
(keys grow, lookup turns `Just`) and falsifies the prop. Deliberately
case-free over `BaseMutation`, so M4's exhaustiveness kill still lands in
production code only. M7 target line intact ×1; `mutant@example` ×0.

## F1-regression test + auditor-side split

New row-2 it `two appends conserve state, rows, length and replay` binds
both returned successes, `kelLength == 2`, two SQL rows, two decodable
events, `demoCounter == 3`, and `foldIntegratedFrom`-over-decoded-rows
`shouldBe` live. It is deterministic and leg-4 executed. Documented split:
this guards the conservation property against sequential/regression drift;
race kill-evidence under controlled overlapping callers stays auditor-side
(the inherited P2 probe class), as the mandate directs.

## ANCHOR-ATTEST (committed `3af3d06` bytes)

- M6 success literal ×1 (`writeTVar (stateVar store) (irState result)`);
  `atomically $ do` shape kept; `gs` binding in scope; freshness clean.
- M7 target `Map.adjust (\m -> m{memberRoles = roles}) key (members gs)` ×1
  in `Fold.hs`; `^enactMutation ` present; freshness clean.
- Untouched: `Event.hs`, `Fold.hs`, `Types.hs`, `S28DemoApp.hs`, `gate.sh`
  (verified via `git diff --quiet` on each).
- Fence: `git diff --stat` names exactly `Store.hs` (+70/−detail) and
  `S28AppApiSpec.hs`; no E-class helpers were needed (MVar/evaluate are
  `base`; no cabal change; no Bootstrap/Server wiring).
- Registration self-count (gate extractor, committed bytes):
  3/4/3/9/3/5 = 27, 0 EXTRACT-FAIL, same-line 27 == file 27 (no orphans).
- Hygiene: fourmolu `--mode check` exit 0 on both files; hlint `No hints`
  on `Store.hs`; `Trivial.hs` untouched (leg 7 unaffected).

## Spend ledger (final for this submission; S28-R1 separate ledger)

Builds 0/13B-planned-of-16 (GREEN 10B + SLIM 3B unspent; no whole-project
invocation ran — GREEN executes ONLY on ticket-owner instruction).
Probes 0/10-dev (trigger 8 untouched). Formatters charge-0 ×2 (fourmolu
check, hlint scan). Recon/version greps charge-0. Margin 3B/10P remains
UNALLOCATED (spending needs ruling — margin is not permission).

## Residual doubts (honest limits)

1. No GREEN executed pre-submission: type-correctness rests on review plus
   fourmolu-parse and hlint, not on a compiler run. The instructed GREEN
   (legs 3, 4, M1–M7, 6) is the falsifier; any RED there is a repair defect,
   not a gate defect.
2. `evaluate`-to-WHNF forcing relies on strict-`Text` payload semantics
   (unlifted array: WHNF entails the full encode+render, hence the barrier).
   The auditor race probe adjudicates this empirically.
3. `KELStore` gains one exported field (`storeAppendLock`); external code
   holds the store opaquely (verified: only two named constructions, both
   updated). Recorded as an API addition inside the F1 zone, not a
   signature change.
4. Historical `appendEvent` path untouched by design (its STM-grouped
   reads/writes predate this finding; historic boundary preserved).

## Await

BINDING-GREEN review by ticket owner t28-app-api: GREEN instruction (10B),
M6-rebind ruling (v9 splice verified still applicable; v9.x re-freeze if
the owner prefers a same-requirement variant), then auditor seating. Inbox
empty at submission; questions: none; repairs beyond mandate: none
attempted (STOP-and-ask rule held throughout).
