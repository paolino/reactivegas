# Independent S4-B audit — submission 1

**Terminal verdict: AUDIT-FINDINGS.** Two BLOCKING assurance findings remain against candidate `189e1ed306f8f8e8bcdd11eeab4fc5657a518fc8`, base `3590c0015b84fd58004bf6fb44dd18b107304c48`. Current correspondence proofs are valid; passing full CI does not close the two demonstrated assurance gaps. No candidate acceptance, repair, push, merge or remote action is implied.

## Findings

### F01 — owned opaque predicates disappear from mandatory discovery

**BLOCKING; R7/R8/R9, original S4 completeness and required-consumer classification, V2-ID. Label: new-execution.**

The exact mandatory command `nix develop --quiet -c just lean` returned **0** after adding only this declaration to the already tracked/imported `lean/Reactivegas/Predicates.lean`:

```lean
opaque auditOpaque (s : State) : Prop := s.conti = []
```

It had no counterpart, correctness theorem, table row or authorized exception. The gate printed `MIRROR-CHECK-OK ... discovered=24` and wrote a valid fresh-nonce receipt omitting it. A separate compiled probe then found **25 identities**, named `Reactivegas.Predicates :: auditOpaque`, and confirmed **opaqueInfo with type `State → Prop`**. This is a real compiled omission, not an uncompiled file, namespace escape, count-only inference or missing-tool failure.

The failure class is declaration-kind filtering: `scripts/check-lean-mirrors` admits defnInfo/inductInfo and silently drops opaqueInfo before correspondence/exception accounting. A finite new owned predicate can therefore bypass the original “any Prop” assurance obligation and both classification axes. It needs explicit accountable classification even if executability is bounded or not established; invisibility is not a legitimate exception.

Positive controls distinguish the failure: S02 rejects newly introduced ordinary unannotated/alias predicates and excludes the Prop-parameter/Nat-result specimen; S03 independently rejects a new predicate with an orphan mirror and no theorem. These successes do not cover the opaque declaration form.

Evidence: `controls/S06/mutant.diff`, raw original/mutant, identities, retained mandatory nonce/receipt and restoration; `evidence/S06.receipt.json` (exit 0), `evidence/I-OPAQUE.receipt.json` (exit 0). S06 stdout SHA-256 `58a3b330c6456a6c18f34c79434e52358d3b5bf7023263b661f8627ec20c0234`; I-OPAQUE stdout `5066c74f787c4596381e2a8b03f431944be03de77214efee2f3fde6453ee5000`.

Limit: one concrete opaque declaration in an existing owned imported module. This establishes that failure class, not a census of every Lean declaration form or every possible provenance bypass. The original candidate's current 24 entries reconcile; its future-discovery guarantee does not.

### F02 — P01/P07 relatum controls do not satisfy the explicit executable-body sensitivity requirement

**BLOCKING; operative v2 Amendment 1, R11, C5/C11. Label: new-execution.**

V2 specifically requires mutating the executable expression/operative definition for P01/P07, preserving the original theorem, and observing that theorem stop elaborating. It also requires accurate descriptions of alternate definitional evidence. The submission truthfully describes weaker relatum controls, but labels all controls closed.

Independent controls reproduce the distinction:

| Identity | Single operative change | Typed module | Original target theorem | Observed executable behavior |
|---|---|---|---|---|
| P01 | `KelGroups.GroupView.isMember` returns false | E-P01-BUILD exit 0 | E-P01-R exit 0 | Present user reports false |
| P07 | `step` closePurchase permission atom becomes true | E-P07-BUILD exit 0 | E-P07-R exit 0 | Unpermitted collection closes |

Each Probe contains the exact original target theorem statement and proof bytes, with unrelated helper proofs excluded to prevent first-error masking. The probes genuinely load the changed compiled module. Final AXIOMS positively establishes the restored original behaviors: membership true, unpermitted closure false.

The separate P01/P07 relatum mutants both fail at their original-named theorem with explicit false goals. That evidence is credited for what it observes. Changing the proposition being related does not supply the distinct observation of sensitivity to the reused executable body. **C5/C11 remain PARTLY**, not executable-body KILLED. The theorem is value-parametric or relates an inline expression independently of its production use; this finding does **not** claim that the original correspondence theorem is false.

The disclosure closes the honesty-of-limitation row V2-DEFEQ, not the conflicting explicit body-sensitivity condition. The commissioner must carry that unmet requirement/claim mismatch; this auditor does not choose a model change, theorem change or waiver.

Evidence: `expression-controls/P01/identities.json`, `expression-controls/P07/identities.json`, exact probes, retained raw mutant sources; E-P01-BUILD/E-P07-BUILD/E-P01-R/E-P07-R receipts; P01/P07 relatum receipts; `evidence/expression-restoration.json`; final AXIOMS. Survivor stdout hashes: P01 `8c6cb59d0db09335e50a5a5b03da1f4317bbf2d6ffe123bcab8ea3b688c89a58`, P07 `b2ee00ac2f9e2eb9cca46828904743e2de2a0dc3df4a0e3d4325ad7ee04eb349`.

Limit: narrow target-proof sensitivity under isolated module overlays, not certification of the whole mutated development. Initial overlay probes failed on Lean's first-package-root resolution and are counted setup failures, not kills. Complete candidate libraries were then copied without overwriting each compiled mutant; no second project build was disguised as a query.

## Coverage and successful evidence

Full original S4 requirements, operative v2 and amendments are represented by **89 obligation rows** in `CAMPAIGN-LEDGER.md`. That administrative/semantic obligation set is distinct from the compiled predicate denominator and the mutation denominator. Every row has CLOSED/OPEN/PARTLY, its observation and its evidence label; no aggregate green substitutes for a row.

- **Classification:** independent source-derived imports cover 29 tracked modules. Compiled result-sort inspection yields 24 entries: 23 predicate identities plus `Reach.below` structural machinery. `CLASSIFICATION.md` records both axes, counterparts, authority, source paths and rebind dependencies per identity. Its classifier finds unannotated and alias positives, excludes a Prop-parameter/Nat-result negative and rejects a deliberately empty denominator before a restored positive run. The actual imported mandatory control is independently S02.
- **Proofs:** 19 correspondence theorems plus `productionWellFormed_proj` are PROVED at the final tree. Exact types are captured by AXIOMS; all 20 public proof checks use only `propext`/`Quot.sound` or no axioms. The final mandatory whole-project axiom gate separately reports 29 source/built modules and 1,285 distinct theorem identities. No sorry/admit escape is inferred absent merely from a name or source search.
- **Semantic mutation:** 44 separately executed single-definition atoms across the 17 new mirrors fail only in their intended original dependent theorem, with no earlier definition-typing error. Full originals/mutants, exact-edit anchors, preserved statement/proof hashes and raw diagnostics are retained. `MUTATION-RECEIPTS.md` lists each atom and its evidence. P01/P07 are separately PARTLY as above, not silently credited as executable kills.
- **Values and reachability:** WIT-R compiles distinct positive/negative values for all 19 correspondences, including nonzero financial quantities, distinct Nat enactment payloads, policy values 1/2, duplicate-key first-match lookup and absent-key zero. It constructs a real nonzero deposit `Reach` trace. Arbitrary-state proofs have no added well-formedness premise; this does not claim every literal fixture is reachable.
- **Exceptions:** V4 retains its rfl link to `preservesQuestionDecide`; authorizedStep has the existing 14 constructor projections; stalled preserves Decidable evidence plus evaluated decide. Reach has the specifically ruled bounded exception, never an unsupported undecidability claim. The required-consumer obligation is authority-based, not contingent on an existing caller.
- **Mandatory controls:** S01 clean `just lean` exits 0. S02 and S03 separately fail with the intended missing-counterpart and missing-theorem/orphan identities. S04 keeps the checker present, replaces it by unconditional exit 0, and the permanent invocation fails with `MIRROR-RECEIPT-ABSENT`; exit 127 is not credited. This proves that execution-enforcement control only; semantic sensitivity is separately measured.
- **Final integration:** S05 restored `nix develop --quiet -c just ci` exits **0**, from a checkout with zero oleans beforehand. It executes build/format/hlint/Lean/corpus paths; the corpus check reports 5 traces, 32 events and 7 steps. AXIOMS then exits **0**. Both final streams have zero `PANIC at` occurrences, with nonzero compiled/proof controls.

S05 receipt: 171.232 seconds; stdout SHA `4295630ea04f2f21dd6e66cf42947df7b3e979c1dfd8030f69138ccf06051861`, stderr SHA `b52f164044a7f30051a39b242933fca8827238913d17d2123a2d3054b4df00f0`. AXIOMS stdout SHA `2bd267d26d664d6528b295a02b7cd55bb69fe3e256a6cf1dd06a376693227731`. These are **new-execution** conclusions. Nix/store cache reuse is not excluded by “cold Lean.”

## Provenance, authority and exceptions

Label: **inspection**, except explicitly hash-identified unchanged inputs.

The candidate is detached, descended directly from the named base, with one local commit and exactly four changed paths, **830 additions, zero deletions**: the two new Mirrors modules, checker, and ten additive justfile lines. Existing source/model/theorem statements and sibling recipe lines remain byte-unchanged. No monitor, coordinator behavior, runtime call site, docs/en/design edit or extra equality premise on an original theorem was introduced. Enacts scopes DecidableEq only on the new pair; the threshold remains a callable parameter. Git provenance and the relevant incoming base deltas are retained in `evidence/provenance.txt`. New exporter source and S2R changes are accounted for by final source discovery and execution, not a reused historical count.

All 16 admitted manifest entries match. Operative v2 is `2214ff8a0d25f47afded7b7215e9873b5a237d97caea55eb72b1d8f884c5ca4f`; original S4 is `f872255f8fffe24f5b7ab360dbac50dda692b3887ab846703637fe2c696e4d87`. R1-R18 carry through v2; superseded v1 controls and stale owner-brief base references do not govern. The phase-A report referenced by the brief was located and verified as `881a9fba51e7eaf2c0f297d82a70dac5752c947bd6faa7b29ee5c2c37400fe3d`; its linked rulings are hashed separately. It supplies an input, not inherited acceptance. All 186 owner-archive files match the admitted evidence manifest: **unchanged-input with byte identity**, not proof of fresh execution here or of every historical timestamp.

Both express departures are recorded:

1. **Same-window departure:** pane `%564` is alone in `reactivegas:rg-s4b-audit3`, rather than the named `lean-quality` ticket window with commissioner `%503` and owner `%547`. Authority: brief exception A, `QUALITY-S4B-TERMINAL-CONTRACT-RECOVERY-20260905` item 2. The replacement rule requires this dedicated distinct sibling seat; live tmux and /proc confirm separation.
2. **Ticket-wide ceiling-history departure:** admitted ruling `b9e8c4673075b87de15e8ce06fd04e28579b05281fbb016051043c62f195ba30` item 2 replaces that exact prerequisite with the S4-B campaign's own ceilings and its one owner increase 6→8. Other-slice aggregate history remains unknown, retained separately, never zeroed/refunded and conferring no allowance. This exception is not another increase or submission reset.

Live auditor PID/PGID `2523581`, pane `%564`, cwd this detached audit worktree, argv-pinned Codex `gpt-6-astra`/`high`; owner PID/PGID `1493708`, Muse through Pi/opencode-go, pane `%547`. Full argv and clock are in `evidence/start-identity.txt`. Process start 22:15:35Z; START 22:25:14Z, about 9m39s of preflight/plan preparation, with zero prior charged execution. ACK specifically records POINTER-1788646565-2524823. Both predecessor terminal contract-block reports remain preserved, measured 0/0, with no semantic acceptance or resumed context.

## Budget, restoration and return

Actual total **6/8 substantive, 59/60 targeted**: 58 Lean targeted calls plus one conservatively charged non-Lean setup failure. Three Lean setup failures (WIT and the two initial overlay probes) remain spent and excluded from valid experiment totals. An accidental Node invocation of a nonexistent file is separately charged; its stderr was discarded and exact UTC unavailable, explicitly recorded, with no candidate conclusion derived. An earlier free classification was corrected append-only. A rejected recursive-removal tool call launched nothing; the safer replacement moved the old .lake into retained evidence before final CI.

`COMMAND-RECEIPTS.md` enumerates every charged invocation, actual exit/duration where recorded, cache qualification, exact argv and receipt hash. The plan and subsequent actual refinements stayed within the original ceilings. No budget was refunded, no new grant taken, no required row dropped, and no whole-project wrapper relabelled targeted. About 216.97 GB free space remained after full CI.

All temporary tracked controls were restored with exact original hashes. Isolated expression-module sources/oleans were restored separately while preserving their raw mutants; final original-behavior checks pass. The old mutated build cache is explicitly retained as historical evidence and is not the final candidate's cache. Raw mutants, scripts, stdout/stderr, typed witnesses and reports remain local. Final tracked Git status is clean. No source repair, stage, commit, fetch, remote write, seat contact or human composer action occurred.

Stopping: the finite planned audit has terminal row assessments and two evidence-bound blocking findings. It did not stop for budget exhaustion or a quiet exploratory tail. Blocking rows remain BLOCKED/PARTLY, never advisory residuals. Original transition inversion/replay/model redesign campaigns are outside this finite correspondence slice; the inherited mandatory gates still ran. No theorem refutation, whole-machine correctness, remote CI, ticket completion or owner acceptance is claimed.

The commissioner **%503** owns disposition of F01/F02 and any authorized repair/requirement ruling. `ONWARD-DISCOVERIES.md` explicitly records the empty unrelated-discovery set; any onward semantic routing belongs to the commissioner, not this seat. This is submission 1 of the commissioned two-submission limit.
