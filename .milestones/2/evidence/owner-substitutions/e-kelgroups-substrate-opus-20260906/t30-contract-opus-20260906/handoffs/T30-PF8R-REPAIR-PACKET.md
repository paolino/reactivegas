# T30-PF8R-REPAIR-PACKET — the single authorized repair rerun, filed for preflight

Author: ticket preparation owner `t30-contract-opus-20260906` (pane `%572`).
Companion: `T30-INV1-ASSESSMENT.md` sha256 `a665e76574c23a01cf4c519283dca559583946765f831703272b875013187c01`.
Decision on merit: **option (a) — run it.** Rationale in §1.

## 0. Bound artifacts (for the epic owner's preflight receipt)

| artifact | path | sha256 |
|---|---|---|
| runner | `t30-contract-opus-20260906/scratch/pf8r/run.sh` | `524f355dd7ca0106d862768042901a4f083ba604e5f078ea54858874a8c4f611` |
| gate leg under test | `t30-contract-opus-20260906/scratch/pf8r/T30-DRIFT-LEG-r9.sh` | `69c529ca22e1a798e1ffb1810902243d9f3c2a9223da50d1eef66b2a39833a25` |
| fixtures | generated at runtime by `setup_tree` — no frozen bytes to bind | — |

Predecessor bytes are untouched and stay the defect witness:
`scratch/pf8/run.sh` = `62025c179e85b6ab982e848a863daa7d48594eccbc8b25c46c616298fe84c39c`,
`handoffs/T30-DRIFT-LEG-r8.sh` = `f0afa32b4fbb13ac6084b6c3c5abd503f7e21f051fef458265b97fd56a4de3e3`.
`evidence/PRESERVATION-BASELINE.sha256` hashes all 3407 files under the
predecessor's `scratch/pf8` + `handoffs` before and after this work.

## 1. Why the rerun establishes something (the merit decision)

Seventeen mismatches decompose into four causes (assessment §1). Exactly one —
the taxonomy precedence — is a defect in the leg's decision logic. The other
three are two fixture defects and one wrong expected value. Their combined
effect was to stop D-3 and D-4 from executing at all in live mode, which is
where eight required mechanisms live: M2 count integrity, M9 exact-line vs
substring, M10 row uniqueness, M11 exact count, M12 stale product, M14
no-inheritance, M15 per-REQ exact-success discrimination, M23 overlay GREEN.
All eight are currently **UNESTABLISHED**, and they are the checks the eventual
#30 drift gate rests on.

The repair is four small edits whose predicted effect is computable line by
line from the preserved streams; §4 gives that computation per case. Declining
the rerun would leave the join/count/uniqueness/exactness machinery entirely
undemonstrated while the campaign budget expires — the strictly worse outcome.

What a green pf8r would still NOT mean is unchanged and restated in §7.

## 2. Repair — six fixes, each traced to an observation

| id | where | change | evidence it answers |
|---|---|---|---|
| FIX-1 | runner `setup_tree` | commit `dist-newstyle/` in a fixture `.gitignore` before the hs commit | `cases/*/stdout:8` in 28/28; porcelain of a copy = `?? dist-newstyle/` |
| FIX-2 | runner `setup_tree` | `touch -d 2000-01-01` the marker, making freshness independent of `cp` mtime semantics; plus two template invariants asserted (hs porcelain empty, marker older than the .hi) | measured mtime inversion, assessment §1 C-2; `setup-failures=0` critique §2 |
| FIX-3 | runner A20 | predicted substring corrected to the emitted text `1-hash: <path> differs` | `cases/A20/stdout:20` |
| FIX-4 | leg line 53 | restore the missing comment marker | every `cases/*/stderr` |
| FIX-5 | leg `refuse()` | TAXONOMY-v2 precedence — a refusal raised while `OVERALL_FAIL=1` prints its reason and exits **1**, because a verdict has been rendered. `OVERALL_FAIL=0` unchanged (exit 3) | 12 cases exited 3 carrying DRIFT-FAIL lines |
| FIX-6 | leg D-3/D-4 | record each emitted module in `$EVIDENCE_DIR/emitted.mods`; in live mode refuse to join a dump not emitted by this run | `cases/A11/ev/…dump` still reads `POISON` and was consumed |

New controls, because three fixes would otherwise remove or leave missing a
falsification:

| case | purpose |
|---|---|
| A26 | FIX-1 deletes the only demonstration that `1-clean-hs` can fail. A26 dirties the hs tree deliberately and keeps it falsifiable; frozen-oid reads stay PASS, so it also demonstrates reference-vs-content separation on the hs side |
| A27 | FIX-6's negative control: a **correct** dump is pre-seeded and emission is skipped. Without FIX-6 the join prints `4-type Verdict exact` off a file this run never produced — that string is A27's forbidden pattern, so a FIX-6 regression scores MISPREDICT instead of passing quietly |
| A28 | `0-overlay-base` had no negative control at all (assessment F-5). A28 binds a well-formed but wrong base oid and requires the refusal |
| BASELINE gate | A1's outcome now decides the suite verdict and is printed as `BASELINE: GREEN\|BROKEN`. A broken control can never again be reported alongside 27 further "verdicts" |

FIX-5 and FIX-6 both carry bounded blast radius by construction:
- FIX-5 changes no check, only the status a consumer branches on;
- FIX-6 is exempt in overlay mode (D-3 is skipped there), and in live mode a
  non-emitted dump is only reachable after an emission-skip path that has
  already set `OVERALL_FAIL`. **It therefore cannot turn a GREEN into a RED.**

## 3. TAXONOMY-v2 is an amendment, and it is the epic owner's to bind

NOTE-009 §3 requires one taxonomy bound identically in contract, runner and
script, "never reclassified after the fact — any change requires re-freeze +
new campaign binding". This packet IS that re-freeze request. Two points:

1. The amendment is not a convenience. r8's own author predicted **exit 1**
   for A10 (`run.sh:241`), which is unreachable under v1: A10's stale .hi
   RED is followed by a dump-missing refusal, so v1 must return 3. The
   prediction and the taxonomy already disagreed before invocation 1 ran.
2. **Fallback, no redesign needed.** `TAXONOMY_V2=0` in the environment
   restores r8 behaviour exactly. §4 carries both prediction columns; if the
   epic owner declines the amendment, bind the command with `TAXONOMY_V2=0`
   and score the v1 column. Nothing else changes.

Contract §8 and the command map's TAXONOMY-v1 block would need the same
amendment before the leg is used for #30 acceptance. That edit is NOT made
here: the contract is frozen at r8 and versioning it is a separate act.

## 4. Per-case predictions (31 cases; v2 primary, v1 fallback)

Derived by walking r9 against the fixture; ``=`` means unchanged from the r8
prediction. Exit under v1 differs only where a refusal follows a rendered
verdict.

| case | exit v2 | exit v1 | required substrings | forbidden |
|---|---|---|---|---|
| A1 | 0 | 0 | `FINAL: PASS`, `traversed=4 frozen=4`, `3-pinned KelGroups.Vote.Types`, `4-type Foo exact` | `FINAL: RED`, `DRIFT-FAIL`, `DRIFT-REFUSE` |
| A2 | 1 | 1 | `4-count`, `traversed=3 frozen=4` | — |
| A3 | 3 | 3 | `ZERO data rows`, `vacuous pass REFUSED` | — |
| A4 | 3 | 3 | `ZERO data rows` | — |
| A5 | 1 | 1 | `duplicate mapping rows`, `traversed=4 frozen=4` | `4-count` |
| A6 | 1 | 1 | `traversed=5 frozen=4` | — |
| A7 | 1 | 1 | `empty dump`, `FINAL: RED` | — |
| A8 | 3 | 3 | `ZERO .hi candidates` | — |
| A9 | 3 | 3 | `ambiguous selection REFUSED` | — |
| A10 | **1** | **3** | `stale inheritance refused` | — |
| A11 | 0 | 0 | `FINAL: PASS` | `FINAL: RED`, `DRIFT-FAIL`, `DRIFT-REFUSE` |
| A12 | 3 | 3 | `no producer evidence` | — |
| A13 | 3 | 3 | `BUILD_RECEIPT absent` | — |
| A14 | 3 | 3 | `unknown MODE` | — |
| A15 | 1 | 1 | `REQ-B has NO successful`, `REQ-C has NO successful` | — |
| A16 | 1 | 1 | `ZERO successful execution records` | — |
| A17 | 1 | 1 | `1-clean`, `uncommitted bytes` | — |
| A18 | 1 | 1 | `1-position-lean`, `rebind procedure` | `differs from frozen bytes`, `ZERO data rows`, `unbound config` |
| A19 | 1 | 1 | `1-position-lean` | — |
| A20 | 1 | 1 | `1-hash: lean/KelGroups/Vote/Types.lean differs`, `re-review required`, `0-overlay-base` | `1-pin-lean`, `FINAL: PASS` |
| A21 | 0 | 0 | `FINAL: PASS`, `0-overlay-base` | `FINAL: RED`, `DRIFT-FAIL`, `DRIFT-REFUSE` |
| A22 | 1 | 1 | `1-hash-hs: lib/KelGroups/Vote/Types.hs differs`, `0-overlay-base`, `FINAL: RED` | `1-pin-lean`, `rebind procedure`, `FINAL: PASS` |
| A23a | 3 | 3 | `BUILD_MARKER absent` | — |
| A23b | 3 | 3 | `not a regular file` | — |
| A23c | 3 | 3 | `not a regular file` | — |
| A23d | 3 | 3 | `unbound config:`, `FROZEN_ROWS`, `LEG4_LOG` | — |
| A23e | — | — | no `FINAL: PASS` in any `A23*/stdout` | — |
| A24 | 1 | 1 | `expected exact line [Fo] ABSENT` | — |
| A25 | 3 | 3 | `unbound config:`, `JOIN_ROWS`, `BUILD_RECEIPT` | — |
| **A26** | 1 | 1 | `1-clean-hs: uncommitted bytes in kelgroups tree` | `1-hash-hs: lib/KelGroups/Vote/State.hs differs`, `FINAL: PASS` |
| **A27** | **1** | **3** | `stale inheritance refused`, `4-provenance: dump for KelGroups.Vote.Types was not emitted by this run` | `FINAL: PASS`, `4-type Verdict exact` |
| **A28** | 3 | 3 | `0-overlay-base: export base`, `unfounded overlay` | `DRIFT-1: input binding`, `FINAL: PASS` |

Suite prediction: `BASELINE: GREEN`, `setup-failures=0`, `mispredicts=0`,
`===== SUITE: PASS =====`, runner exit 0.

**These are predictions, not results.** They are derived by reading r9 against
the fixture and the preserved streams; no part of pf8r has been executed. Any
row that misses is data, and §6 says what to do with it.

## 5. Exact command (for the binding receipt)

CWD `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/`:

```sh
bash scratch/pf8r/run.sh > scratch/pf8r/RUN.log 2>&1; echo "runner-exit=$?" >> scratch/pf8r/RUN.log
```

(With `TAXONOMY_V2=0 bash scratch/pf8r/run.sh …` if the amendment is declined.)

The redirection is deliberate and is itself a repair: invocation 1's command
was a bare `bash scratch/pf8/run.sh`, so the harness's own 28 per-case verdict
lines were never written to disk (assessment §0, F-7). `RUN.log` lives at the
tree root, which `run.sh` does not wipe.

Counter: synthetic-preflight **2 of 2** — the authorized repair rerun. Own
counter; historical product-build spend 0 and stays 0. Aggregate
historical+new ceiling 4, of which 3 consumed; this is the fourth and last.
No quiet third in this campaign, and a failure here ends it.

## 6. Branch rules after the rerun

- **All rows as predicted** → the eight currently-unestablished live-mode
  mechanisms move to ESTABLISHED at line level; M22b, M20-in-live and the
  compiler layer stay UNESTABLISHED regardless (§7). Report and stop.
- **A1 BROKEN** → the suite says so in one line; report the baseline
  diagnostic and stop. Do not read the other 30 rows as verdicts.
- **A27 shows `4-type Verdict exact`** → FIX-6 regressed; the false-inherit
  channel is open. Report it as a blocking instrument finding.
- **Any single row misses with the baseline GREEN** → that row's mechanism is
  reported UNESTABLISHED with its stream; the other rows stand, because with
  a green baseline each row's named lines are attributable.
- **Any new stderr diagnostic** → report verbatim; do not re-run.

No outcome authorizes another invocation from this seat.

## 7. Honest scope of a green pf8r (unchanged)

Exercised shell/git plumbing on synthetic fixtures under a stubbed `ghc` shim.
Not compiler compatibility, not compiler discovery, not semantic coverage, not
mapping completeness, not product readiness. In particular a green pf8r leaves
**M22b** (source-hash vs `.hi` tripwire independence) unestablished in every
mode, because overlay skips D-3 by design and live mode has no case where both
channels fire; and it leaves the whole compiler layer untouched — see the
handback for the exact next prerequisite and its cost.
