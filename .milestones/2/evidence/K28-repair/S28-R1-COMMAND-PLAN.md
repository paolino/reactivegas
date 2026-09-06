# S28-R1 COMMAND PLAN (frozen — NOTE-024 §3 precondition)

Ticket owner `t28-app-api`. Authority: NOTE-024 (desk commission via epic;
proposal RECUT-PROPOSAL-S28-R1 superseded by the two §2 fixes recorded
here). This plan is the S28-R1 mandate (with r5/addenda as history). Status:
FROZEN 2026-09-05. Fits within caps → NO additional desk checkpoint:
proceed owner implementation → freeze → audit → packet-or-blocker.

## 1. History preserved (old campaign closed, separate ledgers)

S28-1: owner 34/34, audit 9/12+7/24, one SPENT submission, terminal report
`b7b793a3…`, findings F1/F2 admitted, candidate `84a2dae…` NOT accepted.
Fresh S28-R1 budgets below are SEPARATELY identified (never a zeroed old
ledger, never refunded calls). Proposal fixes recorded: (a) starting bytes
`84a2dae…` confer ZERO acceptance; (b) audit base is ACCEPTED `368b596…`
full-range reassessment (replaces the proposal's parent-chain framing and
its no-re-derivation promise — neither carried forward).

## 2. Scope freeze

Requirements: R1/R3/R5 OPEN with ledgered limits; R2/R4/R6 BLOCKED with
F1/F2 inside; reliances with unassessed limits (all five, ledgered).
Repair deliverables: F1 production repair per §4 criteria (bind returned
successes + committed ordered events + replay + live state + event-count
under controlled overlapping callers; keep sequential + rejection controls;
never weaken success or drop events); F2 strengthened effect-observing
tests (real membership comparisons over present AND absent targets; one
wrong effect defeats the mandatory check); F1 deterministic conservation
regression test (leg-4 executed; race kill-evidence auditor-side).
H-mandates carried (H1-H7; M6 region excluded from preservation — F1 repair
zone, rebinds at BINDING-GREEN). M1-M6 programs byte-carried; M7 new
(shadow voted-insert, triple-proven: dry-run + manual-escape verification
+ pull-from-file execution on committed bytes; historical 216-arm intact).
RED-equivalence: INHERITED executed evidence, no fresh RED runs (fresh runs
would be duplicate validation toward no new information): P2 exit-1
(`8d0fb0ff…aa23`) + P4-compile (`8271c00c…45417e`) + P5 exit-1
(`907df80e…cfd15`) + P6 exit-0 (`96fa404a…07ee4d`) + P7 exit-0
(`787e7664…101a7e`) + row4-effect.diff (`7bddd850…846558`).

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
hashes). Rule: any future gate-author build/probe charges here first (BLOCK
trigger). M7 falsification executes inside GREEN leg-5 (counted there, no
double count).
Owner GREEN envelope 10B: leg-3 cold `nix build .#kelgroups:test:kelgroups-test`
(1B) + leg-4 `nix develop .#ci -c bash -c 'cabal update && cabal test all -O0 --test-show-details=direct'` (1B) + leg-5 M1-M7 (7B) + leg-6 `just ci` (1B).
Owner SLIM 3B: legs 1,2,2b,3,4,6,7 (no re-mutation; identical-envelope rule).
Owner probes: recon ~4 + dev ≤10 (trigger 8; narrowed discipline; whole-project
outside legs FORBIDDEN w/o pre-approval; formatters charge-0). Owner totals:
13B planned / 16 cap; probes ≤14/24 planned. Margin 3B/10P UNALLOCATED
(spending needs ruling — margin is not permission).
Auditor envelope 10B + ≤2 discretionary = 12 + probes ~0-4. Fits exactly;
zero margin; BLOCK trigger armed. Auditor seat AFTER green submission:
fresh pane/root/checkout, Codex `gpt-6-astra` high pinned + live-verified,
complete contract BEFORE START. NEVER terminal %557 or prior context.

## 5. Conclusion + stops

Fits both caps → proceed authorized, no further checkpoint. BLOCK triggers:
any overrun; M6-rebind-impossible; M7-no-kill (→ finding path, not force);
restriction-blocks-work (record + stop that work); provider issues (no
switches/workarounds/diagnosis). Returns: candidate/base/tree SHAs, gate +
mandate identities, CI receipt, audit report/ledger/inventory, cumulative
accounting, resume. No push/PR/merge/release/issue-comment/remote-write.
Carried inputs: base `368b596fef0b6d393c2ac7afc631d236c55d86d1`, RED
`570fe4a68f510fad3c9912ea59c1e492f3e11740`, start `84a2daea1db81b1baf73c73a7874dcd68ce9f4b2` tree `6f24bb30…`, branch `fix/28-r1-conservation-effect`.
