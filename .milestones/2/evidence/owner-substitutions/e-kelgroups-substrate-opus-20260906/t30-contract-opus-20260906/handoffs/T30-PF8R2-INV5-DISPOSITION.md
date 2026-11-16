# T30-PF8R2-INV5-DISPOSITION — executed fifth invocation: SUITE PASS, baseline GREEN

Author: ticket preparation owner `t30-contract-opus-20260906`, pane `%572`.
Authority: NOTE-004 freeze receipt. Executed **once**, exactly as frozen.
Actuals below; predictions are named as such wherever they appear.

## 1. Run identity and counter

Command, verbatim as bound, CWD = this runtime root:

```sh
bash scratch/pf8r2/run.sh > scratch/pf8r2/RUN.log 2>&1; echo "runner-exit=$?" >> scratch/pf8r2/RUN.log
```

Bindings re-verified by this seat immediately before launch and after the run,
unchanged: `run.sh` = `86533877935f6ea21f9e822f106c421a00699233b654848c6e8fb97c46a587cf`;
`T30-DRIFT-LEG-r9.sh` = `69c529ca22e1a798e1ffb1810902243d9f3c2a9223da50d1eef66b2a39833a25`
(still byte-identical to the pf8r leg — the semantic checks that produced this
green are the same bytes that produced the two failures).

**Counter, actual: exceptional fifth invocation SPENT. Aggregate 5 of 5 — no
retry reserve, none used, none requested. Product builds 0.**

Result: `runner-exit=0`.
`ev/SUITE.log` = `===== SUITE: PASS (baseline=GREEN setup-failures=0 mispredicts=0) =====`

Evidence: `scratch/pf8r2/RUN.log`
(sha256 `08b3fcb7bc7251d811c53eb68f8750c0d844b2acd07ac74cece48d3f9b5cae78`);
tree manifest `evidence/PF8R2-INV5-MANIFEST.sha256`
(sha256 `b4946a7d49369cd063e6b282dafa1221f723edd85473b2aeb18b95777c457250`, 3898 files).

Preservation after the run: predecessor `scratch/pf8` + `handoffs` identical
over 3407 files; own `scratch/pf8r` invocation-2 tree identical over 3832;
`pf1`/`pf7` untouched. `/code/kelgroups` `933e385d` porcelain empty;
`/code/reactivegas` `3590c001` with only the pre-existing untracked `sessioni`.

## 2. Baseline outcome

**A1 GREEN, first, as the hard stop requires** — `exit 0`,
`FINAL: PASS (traversed=4 frozen=4 leg4pass=3)`, with `3-pinned
KelGroups.Vote.Types interface bytes == frozen`, three `4-type … exact`, three
`4-exec … successfully executed`, `4-excluded`, `4-unique` and `4-livedir` all
green. `BASELINE: GREEN` therefore let the remaining 30 cases run. This is the
first sound baseline the campaign has produced.

## 3. Every required case, scored independently

The runner scored itself 31/31 AS-PREDICTED. That is the instrument reporting
on itself, so it was re-scored from the preserved `exit`/`stdout`/`stderr` by
`evidence/score-inv5.sh`, transcribed from the packet's v2 column rather than
from `run.sh`: **matches=32 deviations=0**
(`evidence/SCORE-INV5.txt`). The scorer was falsified — mutating one expected
exit makes it report `deviations=1`.

| case | expected (v2) | actual | decisive line |
|---|---|---|---|
| A1 | 0 | 0 | `FINAL: PASS (traversed=4 frozen=4 leg4pass=3)` |
| A2 | 1 | 1 | `4-count: traversed 3 != frozen 4`, with `4-unique` PASSING |
| A3 | 3 | 3 | `ZERO data rows — vacuous pass REFUSED` |
| A4 | 3 | 3 | `ZERO data rows` on a comments-only mapping |
| A5 | 1 | 1 | `4-unique: duplicate mapping rows present` while count passes |
| A6 | 1 | 1 | both `4-unique` and `4-count: traversed 5 != frozen 4` |
| A7 | 1 | 1 | `3-emit: empty dump` |
| A8 | 3 | 3 | `ZERO .hi candidates` |
| A9 | 3 | 3 | `ambiguous selection REFUSED` |
| A10 | 1 | 1 | `3-fresh … stale inheritance refused` |
| A11 | 0 | 0 | `3-pinned … == frozen`; the pre-seeded `POISON` was overwritten |
| A12 | 3 | 3 | `no producer evidence` |
| A13 | 3 | 3 | `BUILD_RECEIPT absent` |
| A14 | 3 | 3 | `unknown MODE` |
| A15 | 1 | 1 | `4-exec REQ-A` PASS vs REQ-B/REQ-C `NO successful execution record`, `leg4pass=1` |
| A16 | 1 | 1 | `ZERO successful execution records` |
| A17 | 1 | 1 | `1-clean: uncommitted bytes present` |
| A18 | 1 | 1 | position RED, `differs from frozen bytes` absent |
| A19 | 1 | 1 | 7-char oid rejected |
| A20 | 1 | 1 | `1-hash: lean/…/Types.lean differs` |
| A21 | 0 | 0 | overlay `FINAL: PASS` |
| A22 | 1 | 1 | `1-hash-hs: lib/…/Types.hs differs (incl. unexported edits)` |
| A23a–d | 3 | 3 | marker / non-regular-file / non-regular-file / `unbound config:` naming all |
| A23e | — | — | no `FINAL: PASS` in any `A23*` |
| A24 | 1 | 1 | `Foo` exact PASSES, `expected exact line [Fo] ABSENT` |
| A25 | 3 | 3 | single refusal naming `JOIN_ROWS` and `BUILD_RECEIPT` |
| A26 | 1 | 1 | `1-clean-hs: uncommitted bytes`, frozen-oid read still PASS |
| A27 | 1 | 1 | `4-provenance: dump … not emitted by this run`; `4-type Verdict exact` absent |
| A28 | 3 | 3 | `0-overlay-base: export base [52418cb6…] != frozen … unfounded overlay` |

Zero of the 31 cases has a non-empty `stderr`.

## 4. Mechanisms now ESTABLISHED

Each with the control that makes it attributable — a green baseline is what
supplies those controls, which is why the two earlier runs could not.

| mechanism | evidence | control |
|---|---|---|
| baseline GREEN / full D-1→D-4 pipeline | A1 | — |
| exact-count integrity | A2 `traversed 3 != frozen 4` | A1 count passes; A2's `4-unique` passes, so count and uniqueness are independent |
| row uniqueness vs same-size swap | A5 `duplicate mapping rows` while count passes | A1 `4-unique` passes |
| both fire together | A6 | A2/A5 fire singly |
| exact-line vs substring | A24: `Foo` exact PASSES, `[Fo]` ABSENT — `Fo` is a substring of `Foo`, `FooBar`, `Foo2` | A1's three exact matches |
| per-REQ exact-success discrimination | A15: `PASS: REQ-A OK` accepted; `FAILED:`, `SKIPPED:` and bare-name lines rejected; `leg4pass=1` | A1 `leg4pass=3`; A16 `leg4pass=0` |
| stale product → RED | A10 `3-fresh` | **A1's `3-fresh` now passes** — the control destroyed in both earlier runs |
| no inheritance: emission overwrites | A11 `3-pinned … == frozen`, `FINAL: PASS`; the seeded `POISON` is gone from `ev/…dump` | A27, where emission is skipped and the seed survives |
| dump provenance (FIX-6) | A27 `4-provenance … not emitted by this run`, `4-type Verdict exact` absent | A1/A11, where emission runs and the join proceeds |
| `.hi` hash-pin against frozen | A1 `3-pinned … == frozen` | A7 empty dump; A10 stale |
| overlay GREEN path | A21 `FINAL: PASS` | A20/A22 overlay REDs |
| `0-overlay-base` falsifiable | A28 refusal | A20/A21/A22 PASS branch |
| `1-clean-hs` falsifiable | A26 | A1 clean PASS |
| clean-sample, position, full-oid, refusal taxonomy, vacuity guards | A17/A18/A19, A3/A4/A8/A9/A12/A13/A14/A23a–d/A25 | A1 passes each |
| TAXONOMY-v2 precedence | present and correct wherever a refusal follows a rendered verdict | A3/A4/A8/A9/A12–A14/A23/A25/A28 exit 3 with no prior verdict |

## 5. What remains UNESTABLISHED — and one is structural, not a fixture gap

- **Channel independence between the source/byte-hash tripwire and the `.hi`
  tripwire (M22b). Not reachable in this harness by construction.** In live
  mode the hash tripwires read through the frozen oid
  (`git show $FROZEN_HS_BASE:path`), so a source edit *cannot* make them fire —
  A17 and A26 both show working-tree edits leaving `1-hash`/`1-hash-hs` green.
  They are falsifiable only in overlay mode (A20, A22), and overlay skips D-3
  entirely (`3-skipped in overlay`). **The two channels are therefore never
  both live in the same mode**, and no synthetic fixture can put them there.
  Demonstrating independence needs the real-build path — B22a/B22b — not
  another harness invocation. This is a property of the design, not a defect
  introduced by any fixture.
- **The entire compiler layer.** Every metadata path here runs through the stub
  `ghc` serving fixture bytes. Nothing observed says anything about real
  `--show-iface` output, real `.hi` discovery in a real `dist-newstyle`, or
  hash-pin stability across real rebuilds.

## 6. Honest scope of this green

It means: the exercised shell/git plumbing behaved exactly as predicted on
synthetic fixtures under a stubbed `ghc`, with every row scored against a
column frozen before the run and re-scored independently afterwards, and with
the gate leg byte-identical to the one that failed twice.

It is **not** compiler compatibility, **not** compiler discovery, **not**
semantic coverage, **not** mapping completeness, **not** product readiness.

## 7. Exact real-compiler / product prerequisites

Unchanged by this run, and — carrying NOTE-003 §1 — on a **different layer with
its own cost**: compilation establishes the real interface boundary; it neither
repairs nor establishes this harness, and no part of this harness work was or
will be free inside any owner budget.

- **P1 — B3, one owner BUILD.** `nix develop .#ci --quiet -c just build`, plus
  pre-build marker touch, build-receipt capture, per-frozen-module
  `ghc --show-iface <hi>` emission and hash-pin. Establishes real `.hi`
  selection uniqueness in a real `dist-newstyle`, real `--show-iface` hash-pin
  stability, and the marker/receipt discipline against real build timestamps.
- **P2 — B22a + B22b, two owner BUILDs.** Baseline GREEN plus overlay-edit
  build, emission and diff-fire. The only way the `.hi` tripwire's can-fail is
  demonstrated, and — per §5 — the only way M22b closes.

**Total 3 product builds**, all inside the **ungranted** owner budget, none
payable inside this preparation fence.

Owner 26/24 and auditor 25/24 remain **PROPOSALS**. `#30` implementation and
audit remain **UNGRANTED**. TAXONOMY-v2 remains bound to this synthetic
experiment only; contract §8 and the command-map taxonomy block stay
**unamended**, and their reconciliation is a separate versioned act before r9
is used for any `#30` acceptance.
