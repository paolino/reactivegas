# S4-B SUBMISSION 2 — FINAL (audit-ready; candidate `94bb7bb`)

**Candidate:** `94bb7bb64324a48f7361252556b4d15e45b3923f36` (committed, clean
tree verified after every control and after final CI).
**Base:** `3590c001` (accepted). Prior `189e1ed` preserved as audited
submission-1 baseline (AUDIT-FINDINGS: 78 closed, 11 partly, 2 blocking).
**Instruments:** brief `b4a79201…`, reconciliation v2, acceptance v2
(`2214ff8a…`), v3.1 amendment (operative for P01/P07), frozen sheets incl.
amendment-2 + NOTE-020 binding + NOTE-023 grant + NOTE-024/025/026/027/029/030/031/032
as applicable; all self-verified by hash before use.
**Fence respected throughout:** script + owned mirror files only (bindings +
classification); no model/guard/theorem-statement/justfile/Invariants/docs
changes; no new theorems; sibling rule intact (additions-only justfile history).

## 1. Owned diff 3590c001 → 94bb7bb (4 commits, all local, none pushed)

- `189e1ed` submission-1 candidate (not accepted): 2 Mirrors modules (17 Bool
  mirrors + 19 correspondence theorems + 1 supporting projection), checker
  script, justfile wiring (additive only). +829/-0.
- `59309d6` submission-2 repair: F01 total kind classification with fail-closed
  arms, P01 promotion table. +102/-32 (script + Mirrors visibility ×2).
- `0f3ad01`: promotion mention-check harvests hypotheses + body (NOTE-013
  defect 1). +11/-4.
- `4d0a324`: Boundary-1 design (in-run exe build, CorpusExport import,
  tracked-set ownership, import-reach completeness). +43/-7.
- `b667648`: driver block structure + tracked array literal (O1 findings). +5/-4.
- `94bb7bb`: driver imports CorpusGate + TraceTests (O1retry import-reach
  finding). +2/-0. Plus one uncommitted-at-O-time? NO — tree verified clean
  at every O-run boundary and after O6 (final).

## 2. Rows and receipts (all under `handoffs/evidence/`)

| row | outcome | receipt |
|---|---|---|
| F01 repair green (O1retry2) | exit 0; 19 rows, 4 exceptions, promoted=2, orphans 17, discovery 24, census pred=24 unclassified=0, receipt+nonce+assertion | S2-O1retry2-94bb7bb-GREEN.log |
| O2 opaque+orphan (Pred-opaque, Corpus-opaque, orphan mirror) | exit 1; UNCOVERED×2 (homes named) + ORPHAN + CHECK-FAILED, no receipt | S2-O2.log |
| O3 classifier-omit (induct arm) | exit 1; 54 UNCLASSIFIED-KIND lines (owned inducts, classifier's own diagnostic, zero Lean errors) + CHECK-FAILED | S2-O3.log |
| O4 permission-atom (+print) | exit 1; FIRST failure Invariants.lean:318 inside step_close_inv (mutant vs expected quoted); print `true`; guard proves, close-P07 via broken link; checker never ran; trace modules unelaborated | S2-O4.log |
| O5 constant-false isMember | exit 1; Step decide-failures (preservation×2, comune) + sorryAx transitively; Mirrors unbuilt → helpers unreached here (masking confirmed empirically); checker never ran | S2-O5.log |
| C4/noop fresh (neuter to exit 0) | exit 1; build green; MIRROR-RECEIPT-ABSENT (correct attribution) | S2-noop.log |
| O6 final cold ci | exit 0 (17,987-line log in submission 1; this phase log hashed below); corpus verify OK; tree clean | S2-O6.log |
| OT3 witnesses | exit 0 (authorized-succeeds-exact-binding + unpermitted-refused + contrast, all by decide) | S2-OT3.log |
| OT4retry census | exit 0; old/new identity sets printed, agreement asserted, unclassified==0, opaque==0, conditional OK | S2-OT4retry.log |
| SH-P01 triad | compile 0 (olean); neg exit 1 EXACTLY at 2 helpers (P01-orig proves); pos exit 0 | S2-SH-P01{compile,neg,pos}.log |
| SH-P07 triad | compile 0 (olean); neg exit 1 EXACTLY at step_close_inv (others prove/elaborate); pos exit 0 | S2-SH-P07{compile,neg,pos}.log (+retry log identical class) |
| O1/O1retry fails | preserved as spent attempts (array literal; placeholder substitution) | S2-O1.log, S2-O1retry.log, S2-O1retry2-ba623667-RED.log (restored) |

Axioms/totality at final tree (via O6 mandatory-path gate, stronger than the
T42 driver): all 20 theorems + helpers depend only on permitted
(propext/Quot.sound/[]) — zero `sorryAx` anywhere in the whole CI log, zero
`PANIC at` in both streams. Statements exact-captured (T32) and unchanged
since (promotion = visibility keywords only, proven by diff).

## 3. Limitations carried open (not narrowed, not waived)

- **P07 single-variable isolation gap** (NOTE-029): overlay verification output
  not retained — mutant-Step-loaded authenticated by the diagnostic itself;
  every-other-byte-clean rests on unretained checks. Open to the auditor; the
  overlay was deliberately NOT recreated (today's bytes ≠ historical claim).
- **sortUndecided→fail path**: source-verified contract repair, NOT executed
  (bucket empty on clean tree by construction; no operation seeds one).
- **O1-failure archaeology**: two spent baseline attempts (array literal,
  placeholder substitution) preserved as sequencing cost, not external blocks.
- All original S4 requirements remain open to falsification; no row closed by
  implication; 19 correspondence proofs valid as proven.

## 4. Exact spend (failed and warm included)

- Substantive 18/18: hist 8 (S1,C1,C1r,C1g,C2,C3,C4,C26) + O1(9, array) +
  O1retry(10, placeholder) + O1retry2-red@ba623667(11, reach-gap) +
  O1retry2-green@94bb7bb(12) + O2(13) + O3(14) + O4(15) + O5(16) + noop(17) +
  O6(18). Hmm — that totals 8+10 = 18 with O6 as 18th: recount O-phase:
  O1(9), O1retry(10), O1retry2red(11), O1retry2green(12), O2(13), O3(14),
  O4(15), O5(16), noop(17), O6(18). Total substantive ever: 8 + 10 = 18/18 AT CAP.
- Targeted 52/60: 42 hist + OT3(43) + OT4fail(44) + OT4retry(45) + P01c(46) +
  P01n(47) + P01p(48) + P07c(49) + P07nfail(50) + P07pos(51) + P07nretry(52).
  No further targeted authorized or available short of a new grant.
- Submissions delivered 1 of 2 allowed; this packet prepares submission 2
  (the last). No third submission exists.

## 5. Handoff request

Independent FULL audit (fresh seat, `codex`/`grok` restricted set, explicit
model+effort, own START, local reports) over the ENTIRE original unaccepted
candidate at `94bb7bb` over `3590c001`, on its own retained evidence, with §3
limitations visible to it. Auditor dispatch is the owner's action (no pane
machinery in this seat). No push/PR/merge (none authorized).
