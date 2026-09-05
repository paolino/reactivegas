# Submission receipt — S71-B submission 1 (PROOF-COMPLETE, successor count)

- base: 90dae994da67d889831726dd1f17aaae9ad84be1 (S71-B planning amendment, specs only; NOTE-001 corrected from 67877b1)
- origin base / PIN: 4a6cd87fcbc3e4a536bbc9f240f5efe5704022af (S1 #79 landed)
- RED: gate v5 exit=1 at leg 13 (`CLAIM-RED`, 22 uncited), evidence `commit-owner-s2/handoffs/RED-gate.log` sha256=06ca94827030c6cc17df3d9d26d6aa1789e5e9158a3632dad1768d8ba280e407. RED executes the subject (gate legs read Lean+docs) and fails for the intended missing citations, not setup. No RED commit: docs slice owns no test files; gate is ticket-owner owned, untracked. Targeted, 0 builds.
- predecessor partial: `commit-owner-s2/handoffs/GREEN-partial.diff` sha256=331434748398d53be7b480a822980ce03fb63e27be0962ed1148ee73abeee82f (frozen pre-Q-001, manifest-verified; content identical to combined below, no edits since).
- candidate: (local signed commit on 90dae99, created after v6 GREEN; SHA recorded in STATUS COMMIT line)
- candidate diff (combined): `commit-owner-s2/handoffs/GREEN.diff` sha256=331434748398d53be7b480a822980ce03fb63e27be0962ed1148ee73abeee82f (= predecessor partial; manifest 075c857c… + af7f734d…, verify-handoff OK). 2 files, +37/−32, docs-only.
- gate: v6 sha256=877d9b9c596c216f688bf32c5f31ba29ad1b56753bb9ae083cf52007cdaf9085 (backup `evidence/gate-v6.sh` matches worktree), PIN 4a6cd87f. GREEN exit=0 legs 0-15 + C1-C14 + SYNTH + R1-R26 incl. full `just ci` leg 12, evidence `commit-owner-s2/handoffs/GREEN-gate-v6.log` sha256=368c51ecbd116d98099811f5eb0100149423d96341aee9994b6242fd50120c8c.
- commit-gate HEAD: OK (verified before PROOF-COMPLETE).
- path fence: only `docs/en/design/state-machine.md` + `docs/en/design/kelgroups-vote-machine.md` (markers only, no VM prose) modified; forbidden scope untouched (lean/**, specs/, gate.sh at pinned hash, Haskell/sim); worktree/index clean after commit, local-only until push per NOTE-002 upstream check.
- reliance: `commit-owner-s2/handoffs/RELIANCE.md` (5 rows, 0 BLOCKING).
- Q/A: Q-001 gate-v5-unwinnable (C10/C11 pipefail + C12a malformed) → A-Q001 Option A (gate v6) + NOTE-002 order; RESUMED Q-001.
- draft: NONE. salvage_ratio=0 (no draft), owner_delta_lines=69 (37+32).

## R71 → proof map (GREEN evidence = GREEN-gate-v6.log unless noted)

- R71-01 State: leg 2 green; stored-zero `(u,0)`/`(a,0)` sentence, never-stored removed; markers lean:State/bal/bump/absorbConto/comuneBal/stalled/GroupView resolve.
- R71-02 Event: leg 3 green (14 stated, no 15); retired four namespaced; AppEvent 17; Proposal departure/changeRoles.
- R71-03 Route: leg 5 green (appDecided present, baseEnacted unpopulated); viewed step leg 4 green; sealed baseHook three arms; grant/deny rows name pullCollection first (Step.lean:53/57, absent-id-refused).
- R71-04 Authority: AUTH role-only scope (role/only/absent + pullCollection-first), both forbids removed; pledge isResponsabile tension with file-qualified marker; V-2/pledge-agency ruled-not-implemented in pending table.
- R71-05 law-vs-witness: leg 7 green (heading + dotted majority_table + not-a-census caveat); root State/Event markers in How-to-cite; 29-name category caveat kept.
- R71-06 composition: leg 8a green (PROVED-IN-MODEL, leaf, unbound reachability/target/polarity stated, Three-links block intact).
- R71-07 vote lifecycle: leg 8b green (renounce unfinished-vs-ruled, notDesignee/notProposer forward-decls, appFold/voteApply, empty tallies, VOTE_TRACES_V1 vs #74 zero-vote, θ open with exhibits); vote blocks cited (step/appFold/effectedState/validateVoteEvent-x3/baseHook).
- R71-08 Voci: leg 9 green (21 + Quantita/Quantità pair + ImpegnoVincolato + non-goal + cost + open question intact).
- R71-09 authority: leg 10 green (2026-08-26/27, 2026-09-05, V-1..V-7, Q-001/NOTE-016/A-Q001/NOTE-031, tension paragraph intact).
- R71-10 current-vs-ruled: pending table 5-cell V-5 row with closureCause marker + #81 source + re-pin preserved; S1/#68/#69 rows intact; reconciliation hook intact.
- R71-11 citations: leg 11 green (117 markers resolve over 805 decls); leg 13 green (38 cited, 0 uncited; 22 SM + 8 VM added in-block, 109 kept in-section); C10/C11/C13/C14 green; fences green; tables green (C12a/b green).
- R71-12 canCloseGroup: leg 6 green (orphan single-usage + conjuncts, no invented theorem, verdict missing-guarantee).

## T71 → slice map

- T71-06 (F-01/F-02 + malformed row): L4 cassa-decrease + triplet + stored-zero + AUTH scope + pullCollection prerequisites + V-5 5-cell row; every economic/authorization row re-verified against its cited statement.
- T71-07 (claim-syntax + v5→v6): 22 + 8 markers in-block, tables well-formed, fences, SYNTH, replays; v6 C10-C14 green; witnesses/docs separation (leg 14 GREEN, documentary legs bounded, row truth human-audit duty).
- T71-08: pending fresh FULL audit acceptance (row-level duties), remote CI, PR readiness. No merge without desk authorization.

## Measurement

- builds: 1/4 full (v6 GREEN leg 12) + 4/40 targeted (RED, GREEN-targeted, c10retest, gate-x; all SKIP_CI, 0 builds). No parallel builds. Extension: none needed.
- tokens: unavailable (no provider telemetry in this harness).
- wall: START 11:53:32Z → BLOCKED 12:02:12Z (RED+repair+freeze+Q) → RESUMED 12:14:11Z → v6 GREEN 12:15:54Z (times UTC, see STATUS).
