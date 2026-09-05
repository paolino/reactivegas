# S2 submission 3 — fresh full candidate audit

Verdict: **AUDIT-FINDINGS**. The default cold full CI and required source controls pass or reject their injected defect correctly. The ownership mechanism still does not establish the artifact provenance it claims: equivalent loader paths change its project/dependency classification. This is an in-scope blocking Row A1 / AMENDMENT-2 rows 5–6 finding, not a claim that the default candidate currently omits a theorem.

## Frozen subject, authority and independence

- Accepted base: `4a6cd87fcbc3e4a536bbc9f240f5efe5704022af`.
- Candidate: `b0c2cdbaa6e245ad04690045d9bda441d92bc666`.
- Scope: **entire `4a6cd87..b0c2cdb`**, all six changed files; not merely `561347d..b0c2cdb`. The latter changes only `scripts/check-lean-axioms`.
- Mandate v3: `a8e18e478ca8d063d544212dc9ce198066fc26f3372aef0adddfe1247571812f`, independently recomputed as owner brief ∥ AMENDMENT-1 ∥ AMENDMENT-2. Earlier mandates and predecessor reports remain evidence, not inherited acceptance.
- Submission: explicit operator exception **3 of 3**, no fourth. Two ceiling raises consumed. The owner's dated correction now records **14/14 gate attempts**, superseding its earlier 12/14 discount for warm invocations; probes 15/16 are separately enumerated. This audit does not reauthorize owner work.
- Auditor: fresh Codex context, pane `%538`, window `reactivegas-e-lean-compliance`; author family muse, commissioner `%503`. PID `1181614`, process START `Sat Sep 5 12:11:49 2026`; live argv `/nix/store/nqhk0522q8ncygwwx054iq76ckcjll82-codex-0.153.2/libexec/codex -m gpt-6-astra -c model_reasoning_effort=high --dangerously-bypass-approvals-and-sandbox`. Both pins are explicit. Identity was recorded in this root's first STATUS line.
- Fresh detached candidate checkout: `/code/reactivegas-66-s2-audit-s3-codex`, initially no `lean/.lake`. Controls used the separate `-fx` worktree, warmed only from this auditor's cold build. Base `-base` started without `.lake` and was independently rebuilt once.
- NOTE-001 was read and acknowledged **post-START**, with verdict independence preserved. It did not change the frozen SHA. No owner contact, pane input, external publication, commit, push or repair occurred.

## Instrument identities — distinct objects

| Instrument | Exact digest / role |
|---|---|
| `scripts/check-lean-axioms` | `c83ae5647485018e72eef85bd217dfe6ad5202fba224d4e6bb9680dd0f25feb5`; tracked shell wrapper and generated Lean axiom driver, actually invoked by `just lean` |
| Combined tracked executable contract | `8e1c73fef0539c0c95dbde58d16b5cd517410f3981d9b51cdda0fd5c34f39363`; raw concatenation in the exact order below |
| Ignored acceptance `./gate.sh` | **Absent. No digest, no invocation.** The reserved `.gitignore` entry is not a gate |

Combined digest order: `justfile`, `scripts/check-reactivegas-inversion-coverage`, `scripts/check-trace-coverage-agreement`, `scripts/check-lean-axioms`, **`nix/lean-dependency-direction.sh`**, `scripts/check-lean-toolchain`, `.github/workflows/ci.yaml`. The four files under `scripts/` alone do not describe the complete input set. `evidence/combined-contract.json` records the independently reproduced formula; `contract-files.sha256` binds individual inputs. The predecessor combined digest `cd67ade9bc137f87` is not the tracked-script digest.

The exact acceptance command was `nix develop --quiet -c just ci`. It invokes the pin contract, Haskell build, formatting, hlint, `just lean`, and corpus gate. `just lean` invokes dependency direction, positive and negative inversion audits, the axiom gate, trace coverage agreement, and Lake build. Bare-driver probes are expressly distinguished below; they are not extra acceptance receipts. Audit instruments are retained under `instruments/`, and all final artifacts are bound by `HASHES.txt`.

## Ranked findings

### F-004 — BLOCKING: lexical path classification is not resolved-artifact ownership

Authorized boundary: A1, AMENDMENT-2 rows 5–6, and the audit brief's explicit demand to attack `LEAN_PATH` order, the repository boundary and the dependency footprint. Candidate sites: `oleanHit`, `inProjectDomain`, `leanPathEntries`, `builtModules` in `scripts/check-lean-axioms`.

**Executed project-to-dependency counterexample.** From the independently built candidate, retain the exact driver and all 26 imports, but express the project build entry as the loader-valid relative path `.lake/build/lib/lean` before starting Lean. Command:

```sh
instruments/run probe relative-path /code/reactivegas-66-s2-audit-s3-codex/lean \
  nix develop --quiet -c lake env instruments/env-probe relative-path
```

Here and below `instruments/...` in commands denotes the absolute path under this audit root; `STATUS.md` retains the exact expanded argv. `env-probe` sets `LEAN_PATH=.lake/build/lib/lean:<unchanged toolchain entry>` and executes the **unmodified extracted candidate driver**. All imports elaborate. The driver reports S=26/26 but **B=0, T=0**, exit 1, with all 26 tracked source modules allegedly unreached. The same compiled artifacts and driver passed with the absolute path (`axiom-exact.log`, S=B=26, T=1213). The classifier tests the entry's spelling against `root ++ "/"`; it never resolves or normalizes the hit artifact.

This is a **bare-driver, pre-load environment override**, not a `just lean` reproduction and not an environment mutation after imports. It demonstrates a real successful import followed by incorrect ownership attribution, bounded to an alternate valid loader path. The default Lake environment uses absolute paths and passed the full CI.

**Dependency-to-project boundary.** `ownership.log` executes the candidate functions against actual olean fixtures: an external artifact exposed through `lean/.lake/audit-external-link` is classified project-owned; `.lake/packages/dep/...` is excluded, while `.lake/vendor/dep/...` is included. The latter carve-out is hardcoded rather than read from Lake's configured dependency location. The actual manifest currently declares `.lake/packages` and zero dependencies; that absence was verified, not assumed. The pinned `Lake.Config.WorkspaceConfig` exposes configurable `packagesDir`.

The additional pre-load `moved-dependency` probe exposes the unchanged pinned Std directory through `lean/.lake/vendor/stdlib/lib/lean/Std`, a symlink whose resolved `Std.Data.DHashMap.olean` is still the pinned Nix-store artifact. It imports `Std.Data.DHashMap`, uses the original driver body, and records the chosen `LEAN_PATH` plus canonical artifact path before elaboration. It completed with **exit 1 in 712 seconds: S=26/26, B=288, T=15707**. All **262 dependency Std modules** were misclassified as project-built; the named B-minus-S finding lists them, and a secondary T-derivation disagreement also fires. There is no panic or setup/import error. This is a dependency-placement/alias probe, **not a project module defining a shadow Std theorem**, not a changed Lake dependency manifest, and not a mandatory-path receipt.

The packet's assertion that a directory assertion is used nowhere is factually false: `inProjectDomain` is precisely a directory-prefix assertion. Resolving an entry is not sufficient evidence of ownership when textual aliases and placement alter the result. These experiments do not establish silent acceptance of a poisoned project theorem; the observed classification failures are loud false rejections.

### A-001 — ADVISORY mechanism limit: `oleanHit` does not implement the loader algorithm

Command: `instruments/probe ownership`. Two complete top-level artifacts under a project and external entry follow first-hit order in both implementations; reversing entries reverses both results. This closes the ordinary same-name search-order control for those actual fixture files.

With a first entry containing an `Audit/` directory but lacking `Audit/Nested.olean`, and a second entry containing that nested file, the executable result is:

- candidate `oleanHit`: second entry;
- pinned `Lean.SearchPath.findModuleWithExt`: `none`.

The loader selects the first **package root**, then constructs the nested path. `evidence/inputs/Lean-Util-Path.lean` also shows that the loader appends builtin search paths, whereas the candidate reads only raw `LEAN_PATH` entries. The root-selection disagreement was independently executed before NOTE-001 was read.

**Limit:** this resolver comparison is not a successfully imported counterexample. In that incomplete-root fixture the actual loader would fail before the audit runs; the copied nested artifact served only as a path-existence fixture. It establishes that the claimed algorithms differ, not that a loaded theorem was omitted. The builtin-path difference was inspected, not separately executed. It is not inflated into a second blocking live defect.

### G-001 — BLOCKING coverage gap: empty `LEAN_PATH` entry guard not independently exercised

AMENDMENT-2 row 6 requires failure behaviour when ownership authority is missing. This audit exercised missing ROOT, no-hit resolution and the resolver with empty entries, but not the distinct `audit` entry branch for empty/unset `LEAN_PATH`. None is relabelled as that branch. This row remains **BLOCKED**, not killed. The concrete remaining cost is **one targeted Lean elaboration, no build**, beyond the 24 allocated probe runs. The exact scope and proposed command are recorded under coverage limits below. This is an audit assurance gap, not evidence that the branch is broken.

## Full mandate ledger

All rows are binding unless expressly labelled advisory. `FAIL` means the claim is not established as compliant, not that its negative control failed to execute.

| Row | Result and independent evidence |
|---|---|
| A1: discovered S, environment B, compiled T | **FAIL for ownership generality (F-004)**. Default S=B=26; T=1213 distinct identities. Fresh independent constant fold, attributed by the frozen tracked module names rather than the gate's ownership helper, exactly matches all 1213 printed identities (`candidate-export`, `reconciliation.txt`). |
| A1 / A5: `S \ B` | PASS: removing only `Reactivegas.TraceTests` from driver imports produces the sole named missing-module finding, T=1201 (`s-minus-b`). This targets a module not recovered transitively. |
| A1 / A5: `B \ S`, actual project ownership | PASS at driver layer: retained `AuditGhostS3.olean`, built in attempt 2, imported transitively by tracked `AuditConsumerS3`, with ghost source withheld. B=28, T=1214; sole finding names `AuditGhostS3` (`b-minus-s-ghost`). |
| Mandatory-path omission layer | PASS as missing-source rejection: attempt 4 `just lean` fails earlier in Lake on `AuditGhostS3` missing source / bad import. **It does not execute the `B \ S` branch.** The separate driver result above proves the branch; neither result is relabelled as the other. No universal unstageability claim is made. |
| A1 / A5: S truncation | PASS: removing `KelGroups.Tests` from expected S while leaving imports and disk intact yields both walk-vs-S and B-vs-S named findings (`s-truncated`). |
| A1 / A5: one-sided T truncation | PASS: omit `Reactivegas.admin_admission_preservation_holds` from either walk or fold; both exit 1 and name that identity (`t-skip-walk`, `t-skip-fold`). |
| A2′: no numeric quota, valid add | PASS: complete diff removes `expectedDeclarations` and quota comparisons; attempt 2 accepts three registered roots, including retained `Std.Data.DHashMap` import, census 165/165 and axiom extent 29/29, T=1215. Counts are observations, not expectations encoded in the gate. |
| A2′: independent declaration/identity comparison retained | PASS: parser-derived declaration still exists while its compiled census row is withheld; failure names `Invariants.lean:2347`, theorem `admin_admission_preservation_holds` (`census-omit`). Both counters are not copied from one inventory. |
| A2′: nonzero declaration census | PASS: empty parsed census injection fails `derived zero theorem declarations from the audited sources` (`census-zero`). |
| A2′: actual identities/counts | PASS: axiom output prints every module and distinct theorem identity; inversion output prints per-file census and totals. Independent reconciliation closes the current extent, not future checker completeness. |
| A2′: six required inversions remain separate | PASS: all six axiom/binding/tightness rows run in cold CI; source diff preserves their policy and converse checks; the existing constructor-withholding control detects its live gap. |
| A2′ / A5: existing theorem made `by sorry` | PASS: attempt 5 changes only the body of `Reactivegas.admin_admission_preservation_holds`; census stays 163/163, inversion gates pass, then axiom gate rejects that theorem for `sorryAx`. |
| A3: cold provenance | PASS: attempt 1 exact detached candidate, initially absent `.lake`; full CI exit 0. Base independently cold at attempt 6. No owner or predecessor cache was used. |
| A4: zero S | PASS: empty `REACTIVEGAS_AXIOM_MODULES` fires the named zero-source finding (`s-zero`). Other reconciliation findings also occur; the guard itself executed. |
| A4: zero B | PASS: injected empty built set fires named zero-built guard, S-minus-B and zero-T (`zero-b`). |
| A4: zero T | PASS: corrected empty T-array injection preserves S=B=26, reports T=0 and **only** the named zero-theorem finding (`zero-t-corrected`). The first instrument had an invalid mutable-variable shadow; it is charged and not counted as a kill. |
| A5 / A6: theorem depends on nonstandard axiom | PASS: attempt 3 retains Std import, census 166/166, and rejects `auditStdS3Clean` for `auditStdS3Forbidden`; no quota failure. |
| AMENDMENT-1 transitivity follow-up | PASS in attempt 3: `auditStdS3Transitive` uses `auditStdS3ViaDef`, which uses the forbidden axiom; the theorem is rejected for that dependency. The declared axiom alone is not treated as the control. |
| A6: existing three-axiom policy | PASS on current extent: script policy remains `propext`, `Classical.choice`, `Quot.sound`, with the required explanation; denying that set in the driver triggers real collected dependencies (`policy-deny`). Nonstandard and sorry controls above discriminate rejected values. |
| A7 / added-module mandatory path | PASS: clean and poisoned registered roots went through `just lean` in attempts 2/3 and were explicitly swept. No new shipped driver module was added, so §5's new-root exception is not invoked by the candidate. |
| A8: panic and result totality | PASS: exact extracted wrapper tail accepts clean streams, rejects `PANIC at` on either stream, rejects each of four missing result markers, and preserves nonzero status 7 even with good markers (`shell-controls`). Cold CI has zero panic strings. These are shell-stream controls, not extra builds. |
| AMENDMENT-2: default Std import regression | PASS: attempt 2 retains import, sweeps the clean root, excludes dependency Std from B. Original F-003 example is closed at the default paths. F-004 remains a different ownership boundary. |
| AMENDMENT-2: missing authority / no-hit | PARTIAL / **BLOCKED for the empty-path entry guard (G-001)**. Actual missing `REACTIVEGAS_ROOT` is named and exits 1 (`root-unset`). Injecting an unavailable path into driver resolution after imports produces 1733 named no-artifact findings and exit 1 (`no-hit`). Empty entries passed directly to `builtModules` likewise produce 1733 findings (`ownership`). This is fault injection, not a claim that normal imports reached an impossible state. |
| AMENDMENT-2: provenance / package placement | **FAIL**, F-004. Current default footprint is correctly excluded; the general ownership claim is not. |
| B1/B2: nine accurate renames, behaviour and statements preserved | PASS: rebuilt-base and candidate structural `Expr` type/value exports match for all nine definitions/wrappers/theorems after exactly the licensed name substitutions. A corrupted value is rejected by the comparison. Current environment lacks all old names. See `reconciliation.txt`. |
| B3/B5: real obligations, precise claims | PASS for wiring/wording: trust→axiom gate, direction→existing script, pin→existing pin contract. Cold CI executes all three; pin's mismatch self-test runs; the direction scanner's independent seeded forbidden import fails (`shell-controls`). New comments accurately describe the Bools and do not claim these properties impossible in Lean. F-004 limits the trust gate's general provenance claim. |
| B4: removed re-exports dead at base | PASS: same rebuilt base environment as Expr comparison. Scan of every environment constant's type and value finds zero users of all three `TraceTests.checkI57*` wrappers; three separately seeded consumers are each detected (`base-consumers`). Source grep is not the closure evidence. |
| B6: honest unenforced obligations | **FAIL to the extent of F-004**: actual ownership authority is not proved as claimed; this report owns the finding for the commissioning ticket owner. No model-semantic obligation is added. |
| C: corrected doc path | PASS by complete diff and filesystem check: only the comment moves to existing `docs/en/design/state-machine.md`; no `docs/` content changes. |
| Fence / unchanged model and unlicensed statements | PASS: exactly six changed files, three described commits from the accepted base, no model edits, no new tracked Lean module, no unrelated proof edit. The nine licensed compiled changes are equivalent; other changed source consists of comments and the three removed wrappers. `git diff --check` passes. |

### Accepted advisory retained: CI-T-SHARED-FILTER

Fresh executions reproduce both survivors: skipping the same theorem in both T derivations passes with **1212** identities; removing `KelGroups.Types` from T-side membership after S/B reconciliation passes with **1162**. Both derivations share `thmInfo` and B membership: **two views of one inventory, not two independent theorem sources**. SUBMISSION-3 states this honestly. This is the desk-accepted advisory, not reopened as a blocker or labelled killed. The independent current-tree count of 1213 does not demonstrate resistance to common-filter omissions.

## Coverage limits and exact remaining costs

- Binding rows that cannot be closed **as compliant**: A1 ownership, AMENDMENT-2 rows 5–6 and the corresponding B6 truthful-coverage claim, due to F-004. They have executed adverse evidence, not inherited or unexecuted passes. Separately, AMENDMENT-2 row 6 has binding unexecuted entry-guard coverage G-001.
- The mandatory `B \ S` **branch itself** was not executed by `just lean`; that invocation stopped at missing source. The paired mandatory rejection and sole-finding compiled-driver control establish the required independent omission property in the two stated layers. The required independent omission obligation is judged closed by the paired observations, not by literal execution of the branch inside `just lean`. A further experiment specifically forcing that branch within `just lean` would cost **at least one seventh build/gate attempt**, and a suitable reachable staging method is not established. That stronger branch-placement experiment is an explicit unexecuted limit, not an extra claimed kill; no construction-impossibility claim substitutes for evidence.
- The `audit` entry guard for **empty/unset `LEAN_PATH`** was not separately fired. Missing ROOT, unavailable nonempty entries and `builtModules []` were fired; they are not that entry guard. The empty-path guard is a declared defensive branch, not an executed kill. This is the **binding coverage blocker G-001** under AMENDMENT-2 row 6. A driver containing only `import Lean` and the unchanged candidate body, launched with `LEAN_PATH=''`, would exercise it using Lean's builtin search path. The concrete command would be `nix develop --quiet -c lake env /tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s3-codex/instruments/empty-path-unrun`, from the candidate `lean/` directory, with ROOT and S retained. That exact instrument is prepared, hashed and explicitly **UNEXECUTED**: **one additional targeted elaboration**, beyond the consumed 24; zero additional builds. No claim of “unreachable” closes it.
- **Toolchain-name shadowing is NOT binding**, as the brief expressly rules. A project-defined module replacing a toolchain name was not built or tested; the executed order test uses duplicated project olean fixtures, and the Std alias points to the unchanged toolchain bytes. A genuine replacement-module campaign would need at least one additional build, with its associated driver probe; it is a proposed invariant, not a counted kill or a new blocker.
- Literal mandatory-path repetitions of the relative-path and moved-dependency probes were not performed. They are pre-load bare-driver cases; no post-load environment mutation is represented as a full pipeline counterexample. Another `just lean` would cost one build/gate attempt beyond six.
- The builtin-search-path algorithm difference was source-inspected, not isolated in a further running loader control. Resolver root selection was executed; an incomplete root failing to load is not a successful-import omission witness.
- Historical owner RED trees were read as lineage inputs, not rerun or converted into fresh acceptance. Fresh candidate controls, compiled base equality and compiled base consumer evidence are this audit's acceptance-facing checks. Combined #70/#68 branches and unrelated model completeness/proof campaigns are outside this frozen candidate audit.

## Failure behaviour and stopping receipt

The new gate propagates discovery disagreement, build/import failure, axiom dependency, missing output markers and panic output as nonzero. Attempts 3–5 show the caller receiving the failure; the axiom wrapper additionally emits its missing-`ok` marker diagnostic after genuine Lean errors. No new concurrency or synchronization primitive is present. Acquisition exceptions remain exceptions; arbitrary OS failures were not exhaustively injected. The changed ownership classifier adds the false-rejection behaviour in F-004.

All six itemized build/gate attempts were executed, including the early-failing and warm ones. **Attempt 6's one rebuilt base served both compiled Expr equality and consumer scan.** The probe ledger includes the failed initial T-zero setup. Shell output reconciliation, source inspection, file copying and hashing do not compile or invoke another gate; the shell-control suite is conservatively counted as one of the 24 probe runs. Every fixture in that suite is listed in its log.

Completed exact commands, exits and durations are in `evidence/runs.tsv` and append-only STATUS. **Final spend: builds/gates 6/6; probe runs 24/24 (23 Lean elaborations, including one failed setup, plus one shell-control suite).** The completed ledger has 30 receipts and no running attempt. The campaign ends with the finite commissioned rows accounted for and the limits above exposed; it does not claim arbitrary mutation completeness. No further build/probe or repair is authorized by this report.

## Local delivery and worktrees to retire

Delivery is this file, `handoffs/HASHES.txt`, and this root's terminal STATUS event. Nothing is sent to `%510`, any other human seat, the author, GitHub or an external artifact service. The commissioning ticket owner owns disposition and retirement, not this auditor.

Worktrees for that owner to retire:

- `/code/reactivegas-66-s2-audit-s3-codex` — frozen candidate; independent cold acceptance artifacts; ignored probe path fixtures.
- `/code/reactivegas-66-s2-audit-s3-codex-fx` — source/index restored clean at candidate; **cached artifacts include the sorry control**, so this is not a clean acceptance build cache.
- `/code/reactivegas-66-s2-audit-s3-codex-base` — clean base; the single independently rebuilt environment used by both base checks.

The shared author worktree `/code/reactivegas-66-s2` remains clean at the frozen candidate and is **not** included in this auditor's retirement request. No preceding audit tree was changed or retired. Source fixtures, patches, instruments and logs are retained locally.

## Final command receipt

| Attempt | Command / tree | Exit | Seconds |
|---|---|---:|---:|
| 1 | `nix develop --quiet -c just ci`, cold candidate | 0 | 146 |
| 2 | `nix develop --quiet -c just lean`, clean Std root fixture | 0 | 48 |
| 3 | same, poisoned Std root fixture | 1 | 32 |
| 4 | same, project ghost source withheld | 1 | 10 |
| 5 | same, existing theorem sorry body | 1 | 53 |
| 6 | same, cold base rebuilt for both compiled checks | 0 | 53 |

All 24 probes, including failed setup and advisory survivors, are itemized in `evidence/command-ledger.md` with raw exits/durations, and `evidence/runs.tsv` is the machine-readable receipt. The moved dependency counterexample command was `nix develop --quiet -c lake env /tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s3-codex/instruments/moved-dependency`, cwd `/code/reactivegas-66-s2-audit-s3-codex/lean`; the retained script establishes the alias and exact pre-load environment. The original mandatory acceptance tree remains clean; its proof was taken before any ignored audit path fixtures were added.

**Next state: COMPLETE — local delivery; write-idle, no further builds/probes, no repair or outward contact.** Unresolved binding result: F-004 ownership and G-001 empty-path entry-guard coverage. Acceptance/disposition belongs to the commissioning owner.

Checksum verification from the audit runtime root: `sha256sum -c handoffs/HASHES.txt`. The manifest excludes the live append-only STATUS and itself; `evidence/STATUS-before-delivery.md` freezes the pre-delivery journal. The terminal live STATUS event independently names the report digest.
