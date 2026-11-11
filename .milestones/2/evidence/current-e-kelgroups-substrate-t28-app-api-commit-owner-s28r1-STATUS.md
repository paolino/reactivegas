# STATUS — commit-owner-s28r1 (S28-R1 repair implementation)

Worker: commit-owner-s28r1. Seat: pane %545 (fresh process, S28-R1 campaign).
Family: Muse. Branch: fix/28-r1-conservation-effect (@ 84a2dae…, clean).

## Spend ledgers (separate S28-R1 campaign; never a zeroed old ledger)

- Builds: 0 / 13B-planned-of-16 cap. Envelope: GREEN 10B (legs 3,4,5x7,6) + SLIM 3B.
- Probes: 0 / probes-of-24 (dev ≤10, trigger 8; narrowed single-module/file only).
- Formatters charge-0 (fourmolu/stylish-haskell/hlint, committed-tree scans only).

## Events

- START commit-owner-s28r1 — mandate (S28-R1-COMMAND-PLAN frozen), gate v9
  (G28-1 v9 S28R1-plan, normalized 3c433eff…, full dcbc8c2b…), findings
  (AUDIT-REPORT b7b793a3… F1+F2, LEDGER b2860a4f…) read. Retained instruments
  (StoreProbe.hs, Row4Probe.hs, row4-shadow Fold.hs) read. Predecessor record
  (.archived/commit-owner RELIANCE/SUBMISSION) read. Worktree clean @ 84a2dae.
- RED-equivalence cited (inherited, no fresh RED runs): P2 exit-1 (8d0fb0ff…aa23)
  for F1; P4-compile (8271c00c…45417e) + P5 exit-1 (907df80e…cfd15) + P6 exit-0
  (96fa404a…07ee4d) + P7 exit-0 (787e7664…101a7e) for F2. Per brief §RED-equivalence.
- Recon (charge-0, no builds/probes): KELStore constructed at exactly 2 sites
  (Store.hs:272 historical, :582 integrated); no positional construction elsewhere
  (Server.hs holds only, TestHelpers/StoreTestDSL import only). Adding a lock field
  is construction-local to Store.hs.
- F1 mechanism (planned, not yet implemented): per-store MVar append lock around
  integrated validate→insert→TVar-commit; payload encode BEFORE lock acquisition
  (probe barrier lives in ToJSON encode — holding the lock across encode would
  deadlock the auditor's serialization rendezvous); length read inside lock;
  SQL-error path (exception before TVar writes) untouched; refusal path persists
  nothing. Success-write literal kept greppable if possible; new shape noted at
  ANCHOR-ATTEST. M6 rebind expected at BINDING-GREEN.
- F2 mechanism (planned): strengthen inside existing `S28-1 direct-only admission`
  describe, frozen two-layout format, descriptive names; REAL membership comparisons
  (Map.keysSet subset / member lookup) over present AND absent targets for BOTH voted
  arms (RemoveMemberVoted no-op shape + ChangeRolesVoted Map.adjust non-insertion);
  constructor-enumeration property replaced (one wrong effect defeats the check).
- F1-regression (planned): deterministic conservation test (leg-4 executed,
  sequential double-append binding returned successes + SQL rows + replay + live
  state + kelLength); race kill-evidence stays auditor-side (split documented in
  submission).

## Implementation record (all charge-0 except where noted)

- Inbox checked empty at START and pre-commit (0 files); no corrections pending.
- Store.hs (5 regions, one edit call): MVar+evaluate imports; `storeAppendLock`
  field; both constructors initialize it; `appendIntegratedEvent` restructured
  (encode → `evaluate` → `withMVar` → read+validate → INSERT → TVar commit).
- S28AppApiSpec.hs (3 regions, one edit call): `enactMutation` import; row-2
  regression it; row-4 strengthened prop (case-free subset check) + 3 effect its.
- Charge-0 scans: `git diff --stat` (fence: exactly the 2 allowed files);
  M6-literal x1 / M7-target x1 / no-backdoor / Fold+Event+Types+Demo+gate untouched;
  fourmolu `--mode check` on both files exit 0 (via nix develop .#ci);
  hlint Store.hs `No hints` exit 0; registration self-count 3/4/3/9/3/5 = 27,
  0 EXTRACT-FAIL, same-line 27 == file 27 (no orphans); full v9 M6+M7
  precondition predicates re-verified applicable (see SUBMISSION.md).
- Spend: Builds 0/13B-planned-of-16 (no whole-project invocation; GREEN 10B and
  SLIM 3B untouched, awaiting ticket-owner instruction). Probes 0/10-dev
  (trigger 8 untouched; no single-module executions used). Formatters charge-0 x2.
- Commit `3af3d06` signed (Good sig paolo), tree `b4eb37f2…`, tracked-clean.
  No push/PR/remote-write. ONE submission (SUBMISSION.md) filed; awaiting
  BINDING-GREEN review. No second GREEN will run unprompted.

## NOTE-001 acknowledgment (2026-09-05, binding, brief addendum 01)

- Received NOTE-001 (plan-v2 hash corrections, NOTE-026 §2). Five superseded
  raw hashes in START/SUBMISSION STAND unedited per instruction; ACTUAL
  retained-artifact identities bound henceforth: P2 `5b93f9edeefd7028…a2f0cbe`,
  P4 `11b3ee189cfe59a6…f1e21b7a54`, P5 `296644b1288336d9…f20c00d70cdbd`,
  P6 `4011917b4c980039…1f7fd60ceacdf3d`, D `93aa23971b6fd727…866375baf1b82`;
  P7 `787e7664…101a7e` confirmed correct. RED-equivalence substance unchanged.
- BINDING-GREEN received: pass, no variant. Executing GREEN envelope NOW:
  ./gate.sh full run (legs 3, 4 + M1-M7 + 6 = 10B). No other builds.

## GREEN envelope record (2026-09-05/06, HEAD 3af3d06, exit 0)

- Evidence: `/tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/handoffs/evidence/20260905T231211Z-3af3d06-`
  (`gate-full.log` + `leg3-build` + `leg4-test` + `leg5-M1-build/M2..M7-test` +
  `leg6-ci` + `meta.txt` + `registered/row1-6.txt`).
- Leg 1-before PASS (no drift); leg 2 PASS (header==normalized `3c433eff…`,
  HEAD descends from FROZEN_BASE `84a2dae…`); leg 2b PASS (all 7 pins exact).
- Leg 3 PASS (`just build` exit 0, warm). Leg 4 PASS (inventory 8 hits;
  REGISTERED 27 == file 27, no orphans; all executed, none pending; exit 0).
- Leg 5 PASS all seven: M1 build-RED unification; M2 rejecting-step witness;
  M3 atomic-hook witness; M4 exhaustiveness; M5 agreement witness;
  M6 authority witness (`replayed log reproduces live state exactly` — v9 splice
  applied cleanly to repaired bytes, no variant needed); M7 direct-only witness
  (strengthened prop falsified after 4 tests on `DemoChangeRoles "member-key-2"`
  AND absent-change-roles it failed — double kill). Tree restored hash-verified.
- Honest note: under M6, 5 failures (incl. the new regression it and pre-existing
  Store tests) — kill_check needs only a row-6 quote, satisfied; extra-row
  failures are not kills and not defects.
- Leg 6 PASS (`just ci` exit 0: fourmolu/cabal-fmt clean, hlint No hints,
  131+91 examples 0 failures, Lean 17 jobs, client build+test green).
- Leg 7 PASS (Trivial present, uncounted). Leg 1-after PASS (tracked-clean).
- OVERALL_FAIL=0. No other builds ran (single gate invocation only).
- Spend: Builds 10/13B-planned-of-16 (GREEN envelope consumed; SLIM 3B + 3B margin
  unspent). Probes 0/10-dev (trigger 8 untouched). Worktree still `3af3d06`,
  tracked-clean. Returned to ticket owner for audit seating.

## SLIM record (3B, instructed, HEAD 3af3d06)

- Logs (seat dir, teed): `slim-build.log` (SLIM-BUILD-EXIT=0, Up to date),
  `slim-test.log` (SLIM-TEST-EXIT=0, 131 examples 0 failures + 91 keri-hs),
  `slim-ci.log` (SLIM-CI-EXIT=0, full `just ci`: formatters clean, hlint No
  hints, Lean 17 jobs, client build+test green).
- Post-SLIM: HEAD still `3af3d06`, tracked tree clean. No other builds ran.
- Spend: Builds 13/13B-planned-of-16 (GREEN 10B + SLIM 3B consumed; 3B margin
  unspent, unallocated). Probes 0/10-dev. No lane-wide claims.
2026-09-05T23:27:35Z  COMPLETE  GREEN-ENVELOPE candidate=3af3d06 gate=G28-1-v9 norm=3c433eff full=dcbc8c2b log=20260905T231211Z-3af3d06-gate-full.log logsha=1331e4b2 exits=legs346-pass leg5-seven-kills-quoted spend=10of13B-planned probes=0of10dev
