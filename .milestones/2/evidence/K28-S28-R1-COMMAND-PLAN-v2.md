# S28-R1 COMMAND PLAN v2 (frozen — NOTE-024 §3 + NOTE-025 corrections)

Ticket owner `t28-app-api`. Authority: NOTE-024 (commission) + NOTE-025
(plan-receipt corrections, binding). This v2 is the S28-R1 mandate (with r5/
addenda as history). v1 (`9d1c5b73…`, preserved byte-identical at
`handoffs/S28-R1-COMMAND-PLAN.md`) is superseded as mandate, retained as
history. Status: FROZEN 2026-09-05. Fits caps → proceed, no checkpoint.

## 0. Correction history (NOTE-025 §1 — wrong citations kept visible)

| # | v1 cited (WRONG — matches nothing retained) | v2 corrected (recomputed raw = desk readback) | File |
|---|---|---|---|
| P2 | `8d0fb0ff…aa23` | `5b93f9edeefd7028…a2f0cbe` | evidence/P2-store-probe.log (590B, 8 result lines) |
| P4 | `8271c00c…45417e` | `11b3ee189cfe59a6…f1e21b7a54` | evidence/P4-row4-mutant-compile.log (1729B ghc output) |
| P5 | `907df80e…cfd15` | `296644b1288336d9…f20c00d70cdbd` | evidence/P5-mutant-negative.log (72B witness line) |
| P6 | `96fa404a…07ee4d` | `4011917b4c980039…1f7fd60ceacdf3d` | evidence/P6-candidate-positive.log (71B witness line) |
| D | `7bddd850…846558` | `93aa23971b6fd727…866375baf1b82` | evidence/row4-effect.diff (566B) |

P7 (`787e7664…101a7e`, 395B hspec output) already matched — left alone.
V1's five cited values match no retained artifact (whole-auditor-root search,
zero hits); corrected values are raw recomputations equal to the readback set.
No diagnosis (per order). Further v2 changes: M7 reframed PENDING (never
behavioural proof); auditor probes `~0-4` corrected to `≤10` (mapping exposed
the true cost); auditor coverage mapping added (§5); per-row bindings stated
(§2).

## 1. History preserved (old campaign closed, separate ledgers)

S28-1: owner 34/34, audit 9/12+7/24, one SPENT submission, terminal report
`b7b793a3…`, findings F1/F2 admitted, candidate `84a2dae…` NOT accepted.
Fresh S28-R1 budgets below are SEPARATELY identified (never a zeroed old
ledger, never refunded calls). Proposal fixes recorded: (a) starting bytes
`84a2dae…` confer ZERO acceptance; (b) audit base is ACCEPTED `368b596…`
full-range reassessment (replaces the proposal's parent-chain framing and
its no-re-derivation promise — neither carried forward).

## 2. Scope freeze + per-row bindings (no baseline re-runs: binding holds)

Requirements: R1/R3/R5 OPEN with ledgered limits; R2/R4/R6 BLOCKED with
F1/F2 inside; reliances with unassessed limits (all five, ledgered — see
§5). Repair deliverables: F1 production repair per §4 criteria (bind returned
successes + committed ordered events + replay + live state + event-count
under controlled overlapping callers; keep sequential + rejection controls;
never weaken success or drop events); F2 strengthened effect-observing
tests (real membership comparisons over present AND absent targets; one
wrong effect defeats the mandatory check); F1 deterministic conservation
regression test (leg-4 executed; race kill-evidence auditor-side).
H-mandates carried (H1-H7; M6 region excluded from preservation — F1 repair
zone, rebinds at BINDING-GREEN). M1-M6 programs byte-carried; M7 new
(shadow voted-insert; splice-application + syntax proven on committed
bytes; historical 216-arm intact; BEHAVIOURAL status PENDING per §4).
RED-equivalence: INHERITED executed evidence (corrected identities above),
no fresh RED runs (fresh runs on unchanged bytes would be duplicate
validation). Per-row binding — identities (corrected, desk-verified) +
unchanged relevant inputs (tree still `84a2dae` clean; toolchain pins
unchanged; probe sources content-match the logs exactly: StoreProbe.hs 103
lines implements negative/positive/SQL-error/4-pairs/conservation with the
logged constants; Row4Probe.hs witness logic matches P5/P6/P7 outputs) —
therefore the RED baselines bind and no baseline re-runs are scheduled.
Known limit (stated, not diagnosed): report §7 cites probe hashes
`42ef3918…`/`25ca1042…` while disk+inventory agree on `ae6fff29…`/`97e017e2…`
(shadow `71c2dbeb…` agrees three-way). The S28-R1 audit re-executes probes
on repaired bytes anyway (new evidence, §5 — not baseline repeats).

## 3. Gate v9 record

`GATE_VERSION="G28-1 v9 (S28R1-plan)"`, normalized
`3c433effb967052aa91aef2302268c05ab27b3d0f3e54c979504d6978611d340`,
full `dcbc8c2b8eefa111b5b71873be8d87fa95de2369642e6224417f9544e5a8e815`.
Delta from v8: lineage anchor → `84a2dae…` (starting bytes, zero
acceptance); M7 appended (row-4 generic kill via leg-4 runtime names);
envelope legs 3,4,5x7,6 = 10B. M6-rebind procedure at BINDING-GREEN:
verify v8-M6 applicability on repaired bytes; same-requirement variant
under pre-granted authority (D-lane precedent + NOTE-024 §4) with v9.x
re-freeze + re-falsification if needed; BLOCKED-with-evidence if
impossible (never force-fit).

## 4. Command tables with charges (ALL invocations counted)

GATE-AUTHOR named ledger: 0 builds + 0 probes + charge-0 items (gate re-cut
writes, M7 splice authoring/dry-runs/parses, anchor greps, embed/backup,
hashes). It records NO test/build spent and is NOT an exemption for later
execution. M7 falsification executes inside GREEN leg-5 (counted there).
Owner GREEN envelope 10B: leg-3 cold `nix build .#kelgroups:test:kelgroups-test`
(1B) + leg-4 `nix develop .#ci -c bash -c 'cabal update && cabal test all -O0 --test-show-details=direct'` (1B) + leg-5 M1-M7 (7B) + leg-6 `just ci` (1B).
Owner SLIM 3B: legs 1,2,2b,3,4,6,7 (no re-mutation; identical-envelope rule).
Owner probes: recon ~4 + dev ≤10 (trigger 8; narrowed discipline; whole-project
outside legs FORBIDDEN w/o pre-approval; formatters charge-0). Owner totals:
13B planned / 16 cap; probes ≤14/24 planned. Margin 3B/10P UNALLOCATED
(spending needs ruling — margin is not permission).
Auditor: envelope 10B + ≤2 discretionary builds + probes ≤10 (recon ~2 +
probe builds/reruns ≤8) → 12/24. Builds exact at cap (BLOCK trigger armed);
probes with margin. Auditor seat AFTER green submission: fresh pane/root/
checkout, Codex `gpt-6-astra` high pinned + live-verified, complete contract
BEFORE START. NEVER terminal %557 or prior context.

## 5. Auditor coverage mapping (NOTE-025 §2 — binds the audit; executed =
S28-1-audit spent evidence + S28-R1-audit planned executions, distinguished)

- R1 view values: S28-1 executed leg-4 row-1 (3 examples) + M1/M2 kills.
  S28-R1: full leg-4 rerun + M1/M2 re-kills (counted leg-5). LIMIT: canonical
  pre/post view value coverage unsettled (OPEN); M1's coupled edits are NOT
  an isolated event-parameter negative control.
- R3 hook output: S28-1 executed leg-4 row-3 + M3 kill. S28-R1: rerun +
  re-kill. LIMIT: success-side payload + exact pre/post views unsettled (OPEN).
- R5 agreement: S28-1 executed leg-4 row-5 + M5 kill. S28-R1: rerun + re-kill.
  LIMIT: proposal/approval-inclusive agreement is UNCOVERED (no executed
  witness; the generator omits those events; stays OPEN; owner test-gap
  fence item addresses it; auditor judges).
- Reliances: HISTORICAL-FOLD (executed: historical suites + diff review;
  beyond: UNJUDGED); CESR (executed: key tests; decoder-domain beyond:
  UNJUDGED); STORE-STM (executed: P2 4-pair + controls; beyond-4-pairs
  scheduling/crash: UNCOVERED; S28-R1: P2-class rerun on repaired bytes);
  MAJORITY (executed: source comparison + pending-entry test, partial;
  franchise/enactment-success: UNCOVERED); APPFOLD-SHAPE (executed: compile
  shape; semantics beyond: UNJUDGED).
- Shared probes count ONLY with concrete command + cases inside budget
  (P1/P3-class compilations + P2/P5/P6/P7-class reruns, auditor probes ≤10).
  ABSENT cases are never 'covered' by a full-gate marker.

## 6. Conclusion + stops

Fits both caps → proceed authorized, no further checkpoint. BLOCK triggers:
any overrun; M6-rebind-impossible; M7-no-kill (→ finding path, not force);
restriction-blocks-work (record + stop that work); provider issues (no
switches/workarounds/diagnosis). Returns: candidate/base/tree SHAs, gate +
mandate identities, CI receipt, audit report/ledger/inventory, cumulative
accounting, resume. No push/PR/merge/release/issue-comment/remote-write.
Carried inputs: base `368b596fef0b6d393c2ac7afc631d236c55d86d1`, RED
`570fe4a68f510fad3c9912ea59c1e492f3e11740`, start `84a2daea1db81b1baf73c73a7874dcd68ce9f4b2` tree `6f24bb30…`, branch `fix/28-r1-conservation-effect`.
Owner implementation proceeds under the frozen mandate while these
corrections finalize (NOTE-025 §3 — no invented checkpoint; owner
undisturbed this turn: design phase, tree clean, no redirect needed).
