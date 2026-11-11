# S3 recovery — single adjudication and the one repair batch

Owner `%503`. **This is the one adjudication and the one repair batch.** There is
no second adjudication, no higher-level re-adjudication, and submission 2 is the
last submission.

Inspectors, both terminal, both AUDIT-FINDINGS, both manifests verified with no
self-entry, **both settled statically — E8 and E9 unspent**:

| | | |
|---|---|---|
| A instrument executability | `44fafe8c…` | manifest `15bd422c…`, 87/87 |
| B registry precision & validator | `069787e4…` | manifest `bebd6ce0…`, 48/48 |

## What the recovery genuinely achieved — not swept aside

Confirmed by the inspectors independently, and it is real progress:

- **Wrapper exit inversion is fixed across all 17 drivers** — explicit terminal
  exits replace the trailing AND-list. A's words: "This is **not** the former
  trailing AND-list inversion."
- **236/236 non-withdrawn row spans** match declaration start to just before the
  next declaration; rows 89–90 explicitly withdraw phantoms. The row-1 span
  discipline was generalized.
- **76/76 retained private compiled names match verbatim** the retained map.
- **Zero active `[single-atom…]` template literals** remain in before/after fields.
- `shaFile` **is** actually called (line 361).
- The identity extent is **correct**: 270 declarations, independently reparsed.

## The batch — every item is a repair, each with its failing control

### Group 1 — guards and classifiers that cannot currently fail correctly

**R-01 (A/F-A1) — the replay RED guard rejects the failure it exists to catch.**
`replay-run-red.sh:37-47` accepts nonzero only when `FAIL economic: view differs`
appears. The bound producer emits
`corpus-check FAIL economic: economic: view differs from live seedView`
(`CorpusExport.lean:101-110` throwing, `:159-176` printing) — no parse marker, but
**not** the accepted substring, so the wrapper derives **92**. Worse, the
`e2-suite.sh:60-80` stub supplies the *shorter* message at line 64, so the positive
control agrees with the guard while **disagreeing with the actual producer**: it
never crosses the producer/consumer interface. Fix the guard to the producer's
real string, keep the explicit `parse:` rejection, and make the positive control
use the **producer's** message, not a stub's.

**R-02 (A/F-A2) — the eight mutation drivers record but do not classify.**
Each captures the exit, writes logs, prints `WRONG-REASON-REJECTION`, then exits 0
without examining span or class. Timeout, missing compiler, unrelated syntax error
and the intended theorem RED all return wrapper 0. Ship a reason classifier that
distinguishes **intended-span RED / other-span RED / parse-import-tool failure /
timeout / GREEN**, preserving raw output and per-stage status. Bind the pilot to
the actual theorem body `KelGroups/Invariants.lean:628-642`. **Changing a printed
label is not this control.**

**R-03 (B/B-06) — fixture adjudication permits wrong-reason success.**
`e2-suite.sh:123-134` accepts **any one of a broad union** of classes for the first
eight fixtures. And `fixture-gen.cjs:24-33` reads `r.exitStatus` from `spawnSync`,
whose field is **`status`** — so its `code !== 0` predicate is broken. Require the
**named** class per fixture, and read the correct field.

**R-04 (B/B-05) — the runtime-eval fixture proves a lexical match, not a
misclassification.** `fixtures/eval-runtime/.../replay-run-green.sh` appends
`X_EVAL_SMOKE="#eval True"` **after `exit 0`** — unreachable. And
`validate-packet.cjs:382-386` ignores every line starting with `#`, so a heredoc
body containing `#eval 1` evades it. Make the fixture reachable and the predicate
structural.

### Group 2 — validator enforces its contract

**R-05 (B/B-01)** — **13 FROZEN atoms lack exact before-bytes at their stated
spans**: `SS-A-096`/`SS-A-102` name `Vote/Fold.lean:77` while their bytes are at
**78**; `SS-A-138`–`144` and `SS-A-147`–`150` (11) still carry descriptive prose in
FROZEN edit fields. Add the **all-FROZEN comparison against source** the validator
lacks (it checks only path shape and a few literal patterns at `:131-134`,
`:336-342`). Either supply exact bytes or move the row to an owned **OPEN**.

**R-06 (B/B-02)** — ownership status must follow atom precision. 592 relations,
443 PRESERVED / 149 OPEN, yet `/relations/0` and `/relations/1` are PRESERVED while
`SS-A-004`/`SS-A-005` carry `afterText:null` and `bytesComplete:false`. Validator
`:109-110` tests only a truthy atomId and membership. A relation whose atom is not
byte-complete **cannot be PRESERVED**.

**R-07 (B/B-03)** — the census bypasses live discovery. `validate-packet.cjs:232-249`
selects `census.declarations` **first**, with `--source` only an else-if fallback,
so a packet's own census is trusted rather than discovered; `repair-registry.cjs:80-124`
carries the old identity array and scans only the two `Mirrors` modules. **Discovery
must be the primary path.** The extent happens to be right — that is not the point.

**R-08 (B/B-04)** — `PRESENT` is not an experiment binding. `SS-R-044` is PRESENT
with argv, cwd and toolchain all `UNKNOWN` and a hash that is a report reference.
Validator `:140-148` checks a nonempty hash object but not its syntax or content.
Require real bindings for PRESENT, or label the record honestly. Even the
uneven search records (`SS-R-052`, `053`) need their bounded search stated.

### Group 3 — transport and accounting

**R-09 (A/F-A3, and NOTE-082's fold-in) — bind every linking or executing stage
inside the project dev shell.** `replay-build.sh:18-30` invokes bare
`lake build corpusExport`; `env.sh:6-8` supplies only scratch/output roots; the
schedule enters no dev shell. `M11A`/`M11B`/`M12` call the prebuilt executable
directly with the same gap. Outside the shell there is no `cc`; inside `nix
develop` it is at `gcc-wrapper-14.3.0/bin`. Elaboration stages are unaffected —
that is why E1–E5 passed and E6 was the first to bite.

**R-10 (A/F-A4) — stop collapsing stages in charging and batch totals.** Nested
receipts exist; the totals still bundle. Each nested substantive stage carries its
own numbered record under its actual type.

## Budget for submission 2 — five units remain of ten

Charged so far **5** (E1, E2 successful run, E3, E4, E5). E2 attempt 1 and E6 are
retained **uncharged setup failures**. **E8 and E9 went unspent** — both inspectors
settled statically.

| # | unit |
|---|---|
| **R1** | corrected artifact production **inside `nix develop`** (the E6R the transport fix earns) |
| **R2** | runtime execution of that prebuilt artifact (E7) |
| **R3** | one consolidated re-run of the plumbing suite after the guard, classifier and fixture repairs |
| **R4** | one delta inspection of submission 2 |
| **R5** | **unallocated contingency** — returns unspent if not needed; it is not a licence to retry |

**No raises. No stage bundling.** A setup failure that does no product work and
fails before the assertions is logged uncharged; anything reaching the product and
failing is charged. If this cannot fit, **return the exact branch still unexecuted
before spending** — do not launch an unfunded command.

## Fences

No product commit, push, PR or merge. **No production-semantics change. No
`docs/en/design/` writes.** Submission 2 writes to its **own** directory; the
frozen submission-1 is never overwritten. Known tag vocabulary only; every stop
carries `COMPLETE` or `BLOCKED`.

This closes nothing. **S3 is not accepted**; F-01, F-02, F-03, F-06, F-07 and every
unexecuted semantic obligation remain open; `#66` stays open.
