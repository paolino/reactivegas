# Commit Audit

- Submission: 2 of 2 (FULL re-audit of original + amended requirements at the final SHA; passing submission-1 rows were re-opened)
- Base: `4a6cd87fcbc3e4a536bbc9f240f5efe5704022af`
- Rejected submission 1: `5745a2c647c1276cbf90e640a23a44b0d9b8409b`
- Candidate: `561347d1f1cf75624991a1b1bee04071ecc7d9fa`
- Mandate: v2 `7cfb7aec95a37448` (brief.md ∥ AMENDMENT-1); v1 `0a1db9887ccc9d8f` preserved
- Frozen gate at base: `39d6aa4e2c0c0170ff28832f4794293dca64aa3ab65ec6b89ab0924199ce3433`
- Scope: FULL `4a6cd87..561347d` (repair delta `5745a2c..561347d` is 2 files, +150/−52)
- Verdict: FINDINGS
- Audit loop: submission `2/2`; next submission `FORBIDDEN`
- Ceiling raises: owner `1/2` (8→11); auditor `0/2`; this audit spent `4/5`
- Campaign: OPEN — ended by none; blocking F-003 still FAIL
- Builds: `4/5` this audit (`cache=cold` for receipt 1; `cache=warm-fixture` for 2–4)
- Seat: pane `%530`, `grok -m grok-4.6 --reasoning-effort xhigh`, PID 951895; owner `%523` muse; commissioner `%503`
- Snapshot: `/code/reactivegas-66-s2-audit-grok-561347d` detached, initially no `lean/.lake`, porcelain empty throughout
- Delivery: local `handoffs/AUDIT-REPORT.md` only. No gist, no push, no publication.
- NOTE-001 arrived post-START (`2026-09-05T10:12:44Z` ack). Timing recorded. Verdict independent.

Repair-delta blob check: `justfile`, `Invariants.lean`, `Predicates.lean`, `TraceTests.lean` are byte-identical to `5745a2c`. Only `scripts/check-lean-axioms` and `scripts/check-reactivegas-inversion-coverage` changed. Full diff is 6 files, +471/−43; `docs/` untouched; `scripts/check-lean-axioms` mode 100755.

## Ranked findings

### F-003 — BLOCKING: `import Lean` closure is not toolchain/Std provenance; a valid root that imports `Std.Data.DHashMap` is rejected as `B \ S`

Authorized rows: A1 membership (project vs dependency), NOTE-006 §1 (“misclassify dependencies as project-owned”), NOTE-005 repair constraint, desk NOTE-001. Site: `scripts/check-lean-axioms` `builtModules = env.header.moduleNames \ closure(import Lean)`.

The shipped comment claims “toolchain, `Lean`, `Std` and shipped dependencies are excluded by actual provenance.” The live `import Lean` probe on this toolchain returns **1707** modules (`evidence/base-modules.txt`). `Std.Data.DHashMap` is **not** in that set (count 0). `Lean.Elab` is. 261 `Std.*` names are inside the closure (internals); the public `Std.Data.DHashMap` module is outside.

Executed, through the mandatory path, not a probe-only:

1. Registered root **without** extra imports — `AuditRootS2r`, `theorem auditRootValidHolds : True := trivial`. `just lean` exit **0**. Census `declared=164 elaborated-backed=164`, `tracked=27 walked=27 built=27`, `axiom-module AuditRootS2r`, `axiom-theorem auditRootValidHolds`, `axiom-gate: ok`. Evidence: `02-just-lean-clean-root.log` sha256 `8661c8c93afb7b5970cdf6678242d4b00633a23856cdbcef8828121c34fa59e4`.
2. Registered root **with** `import Std.Data.DHashMap` and a clean theorem `auditStdImportHolds`. Census still **passes** (`AuditStdS2r.lean declared=1 backed=1`, totals 164/164). The axiom gate then exits **1** with the single finding `built project modules outside the source discovery: Std.Data.DHashMap`, `axiom-built count=28` (26 + `AuditStdS2r` + `Std.Data.DHashMap`). Evidence: `03-just-lean-std-import.log` sha256 `0d3f25ef91f4a7c3d3bca032395eff8ea6e841fde42cb0cf3d89ce257696e872`.
3. Same classification on the unmodified driver with an extra `import Std.Data.DHashMap` and no new project file: `tracked=26 walked=26`, `built=27`, same `B \ S` finding. Evidence: `probe-extra-std.out` sha256 `40e195946b64a81d04b03c11364136c9dab55729738f37246b234cc3e63813e9`.
4. Extra-import of `Lean.Elab` (inside the closure) does **not** enter B (`inspect-B-minus-S 0`). Control: `probe-inspect-elab2.out`.

A pass obtained by dropping the import, adding a name list, or setting B := S was not used. Shrinking B to S would make `B \ S` unrepresentable; that class remains live (see row 4 / L2 below).

Property class: project membership defined as “environment minus `import Lean`” treats every imported module outside that closure as a sourceless project module. A legitimate project extension that uses a shipped `Std` module the toolchain does not load via `import Lean` is rejected for the wrong reason. Empty probe still fail-closes (`derived zero base-closure modules` when the base child emits no `base-module` lines). Partial probe fail-closes by leaking the omitted name into `B \ S` (`probe-partial-base.out`: `Lean.Elab` after dropping it from the 1707-list). The interesting case is the successful, non-empty, **under-inclusive** closure.

Cap state at this finding: owner **10/11**, raise **1/2** used; auditor **4/5**, raise **0/2**; submissions **2/2**. No third submission is authorized by this report.

### F-001 prefix class — closed at this SHA (does not cancel F-003)

Registered root outside `Reactivegas`/`KelGroups`, no extra imports: clean **passes** (`02-just-lean-clean-root.log`). Poisoned `axiom auditRootForbidden` + `theorem auditRootUsesForbidden := auditRootForbidden`: census 164/164, `tracked=27 built=27`, then `axiom-gate: auditRootUsesForbidden: depends on axioms outside the permitted standard set: auditRootForbidden`. Evidence: `04-just-lean-poison-root.log` sha256 `9bea9051a3b9a4a5a5438c8d2445dceef3e0d3608bb8d06bc4275d0b7c59c9ef`. `projectRoots` is gone from both scripts. The prefix-filter hole is not the remaining defect.

### F-002 — closed for one-sided T truncation; dual-wrong remains a limit

Skip `Reactivegas.admin_admission_preservation_holds` in the module-walk only: exit 1, `theorem identities absent from the module-walk derivation: Reactivegas.admin_admission_preservation_holds`, `count=1212`. Skip in the fold only: exit 1, `… absent from the constant-fold derivation: …`, `count=1213`. Unmutated driver: exit 0, `count=1213`. Evidence: `probe-t-skip-walk.out`, `probe-t-skip-fold.out`, `probe-baseline.out`.

Skip the same identity in **both** derivations: exit **0**, `axiom-gate: ok`, `count=1212`. Drop `KelGroups.Types` from the T-side `builtStrings` copy after S/B reconcile: exit **0**, `count=1162` (was 1213), S/B still 26/26. Both T walks share `thmInfo` and B membership. Two views of one inventory. Required truncation control works; it cannot see a shared filter.

Independent enumerator agrees: occurrences **1214**, distinct **1213**, duplicate identity `KelGroups.setInsert.eq_1`. Neither number is asserted as a quota (`probe-independent-T2.out`). Cold CI prints `axiom-theorems count=1213` with 1213 unique `axiom-theorem` lines.

## Invariant matrix

| Invariant | Severity | Verdict | Row state | Proof / evidence |
|---|---|---|---|---|
| A1 S/B/T independent; break each | BLOCKING | FAIL | OPEN | S git-vs-walk: truncate and zero fail (`git-truncate.err`, `git-zero.err`). B omission via extra-import fires `B \ S` (`probe-extra-std.out`). T one-sided skip fires (F-002). Membership still misclassifies Std (F-003). |
| A2′ no quota | BLOCKING | PASS | KILLED | No `expectedDeclarations` / `== 163` / frozen 1213 in gate, driver, `justfile`, `lean/`. Valid root grew census 163→164 and passed. Counts reported. |
| A2′ c3 valid add passes | BLOCKING | FAIL in general | OPEN | Inside-tree / no-extra-Std root passes (164, `02-…`). Same shape with `Std.Data.DHashMap` fails (F-003). |
| A2′ c4 omit fails by identity | BLOCKING | PASS | KILLED | Git ls-files drop of `KelGroups.Tests` → discovery disagreement naming that module. `B \ S` alone on Std extra-import. `OMIT_FROM_S` fires both walk-vs-S and `B \ S` for `KelGroups.Tests`. |
| A2′ c5 sorry/axiom fails by dependency | BLOCKING | PASS for roots without extra-Std | KILLED | Poisoned registered root fails naming `auditRootUsesForbidden` / `auditRootForbidden` after census 164/164. Existing-theorem `by sorry` through `just lean` **not re-run** (Invariants.lean byte-identical to 5745a2c). |
| A6 three-axiom policy | BLOCKING | PASS on swept extent | KILLED | Driver `permittedAxioms` equals inversion `:101`. Poison names the extra axiom. Std misclassification is membership, not policy. |
| A6 using-theorem shape | BLOCKING | PASS | KILLED | Poison control is `axiom` + using theorem; the USE is named. |
| A4 zero S/B/T | BLOCKING | PASS | KILLED | Zero git ls-files → `derived zero tracked source modules`. Empty base-probe child → wrapper `derived zero base-closure modules`. T-zero not separately re-derived this round (driver still has `theorems.isEmpty` fail). |
| A8 panic / results | BLOCKING | PASS | KILLED | Shim only on `*AxiomGate.lean`: stdout panic → `gate output contains a panic string`; stderr panic rejected; missing `axiom-gate: ok` rejected; clean exit 0 (`wrapper3-*`). Cold CI has 0 `PANIC at`. |
| A5/A7 added module via `just lean` | BLOCKING | FAIL in general | OPEN | Clean root through `just lean` passes. Std-importing root through `just lean` fails F-003. Wired in `justfile` + `ci.yaml` `just lean`. |
| B bit-for-bit | BLOCKING | PASS at candidate | KILLED | `#eval` tuple `(true, true, true)`. Old names absent from env; new names present (`probe-row-b2.out`). Lean sources identical to 5745a2c. Base Expr rebuild **not** spent. |
| B dead re-exports | BLOCKING | PASS as tracked-ref lead + candidate env | KILLED | At base, `checkI57Trust/Direction/Toolchain` occur only in `Invariants.lean` and `TraceTests.lean`. Candidate env: those names `present=false`. No compiled base consumer scan this round. |
| No model/statement change | BLOCKING | PASS | KILLED | Six-file full diff; model files untouched; licensed renames only in Invariants/TraceTests; six `requiredInversions` still in the inversion driver and executed on cold CI. |
| Fence | BLOCKING | PASS | KILLED | Full: 6 files. Repair: 2 scripts. `docs/` empty in the name-status. Modes: new script 100755. |
| C doc path | ADVISORY | PASS | KILLED | `Predicates.lean` cites `docs/en/design/state-machine.md`. No `docs/` edit. |
| F-001 prefix | BLOCKING | PASS | KILLED | Clean+poison roots above. |
| F-002 T truncation | BLOCKING | PASS | KILLED | One-sided skips above. Dual-wrong is a limit, not a miss of the required control. |
| F-003 Std provenance | BLOCKING | FAIL | OPEN | Mandatory-path counterexample `03-just-lean-std-import.log`. |

## Failure modes altered

- New wrapper: source/walk disagreement, empty S, empty base-probe, `S \ B`, `B \ S`, T-set disagreement, forbidden axiom/`sorryAx`, `PANIC at` in either stream, missing reconciliation rows → nonzero. Demonstrated for panic, missing ok, git truncate/zero, Std `B \ S`, poison axiom, one-sided T skip.
- On a real axiom-gate failure the wrapper still `cat`s findings, then additionally exits 1 for missing `axiom-gate: ok`. Caller sees nonzero; the last line is the missing-row message. Checked on `03-` and `04-` receipts.
- `lake build` of the discovered module list remains the acquisition step; build failure is still printed and nonzero. No thread/sync primitive change.
- Membership change: a shipped module outside `import Lean` that enters the environment is now a **hard fail** (`B \ S`), not a silent skip. That is the F-003 observable.

## Residuals

- Dual T enumeration shares B and `thmInfo`. Skip-both and T-side B-shrink agree while omitting theorems. Advisory. Honest limit: F-002 does not prove two independent T sources.
- `moduleNameOf` round-trips all 26 walked sources (`roundtrip-failcount=0`). Crafted `lean/Foo.Bar.lean` maps to `Foo.Bar`; `modulePath` would look for `lean/Foo/Bar.lean`. Not a total inverse on dotted filenames. No such file in the tree.
- Inversion census still uses `getString!` in `modulePath`. Axiom-gate driver does not. Pre-existing partial surface; panic grep covers the axiom wrapper, not inversion.
- Existing-theorem `by sorry` through `just lean` not re-run (budget). Poisoned new root covers the using-axiom shape.
- Base compiled Expr equality not re-run; candidate values and 5745a2c blob identity used instead.
- `KelGroups.Tests` has zero `thmInfo`; shrinking it for T is vacuous. `KelGroups.Types` shrink was the non-vacuous dual-wrong.

## Candidate invariants

- **CI-T-SHARED-FILTER** (proposed ADVISORY): the two T derivations cannot disagree on a theorem that both filters drop. Unratified; does not block except as named limit on F-002.
- **CI-BASE-NOT-PROVENANCE** (proposed BLOCKING, realized as F-003): `B := env \ closure(import Lean)` is not “Std/shipped-deps excluded by provenance.”

## Onward discoveries — outside this ticket

- `specs/62-one-membership-model/functions-model.md:157` still names `checkI57Trust`, `checkI57Direction`, `checkI57Toolchain`. Historical prose, outside the fence, not an executable green. **RECORDED, NOT-OPENED.** Owner: commissioning ticket owner.
- Stale `.lake` from removed owner controls: this snapshot started with no `lean/.lake` and was built at `561347d`. Absence of recompilation on a warm tree was not treated as provenance. **RECORDED.** Owner: this ticket’s cache discipline, already established.

## Blocking findings

1. **F-003 / A1 membership** `scripts/check-lean-axioms` (`builtModules`) — a registered clean module importing `Std.Data.DHashMap` fails `just lean` because `Std.Data.DHashMap` is reported as `built project modules outside the source discovery`. Property class: environment-minus-`import Lean` is not project-vs-dependency provenance. Evidence: `03-just-lean-std-import.log` sha256 `0d3f25ef91f4a7c3d3bca032395eff8ea6e841fde42cb0cf3d89ce257696e872`; controls `02-just-lean-clean-root.log`, `probe-inspect-std2.out`, `probe-inspect-elab2.out`, `base-modules.txt`.

## Verification receipts

| Command | Exit | Duration | Evidence |
|---|---:|---:|---|
| `nix develop --quiet -c just ci` (cold snapshot) | 0 | 148503 ms | `01-cold-ci.log` `7e990dd2…f324ed` `cache=cold` S=26 B=26 T=1213 census=163/163 |
| `just lean` + `AuditRootS2r` | 0 | 50458 ms | `02-just-lean-clean-root.log` `8661c8c9…fa59e4` `cache=warm-fixture` |
| `just lean` + `AuditStdS2r` import Std.Data.DHashMap | 1 | 33516 ms | `03-just-lean-std-import.log` `0d3f25ef…96e872` F-003 |
| `just lean` + `AuditRootPoisonS2r` | 1 | 33535 ms | `04-just-lean-poison-root.log` `9bea9051…9c9ef` named axiom |

Warm `lake env lean` probes uncharged. Instruments: `evidence/instruments.sha256` `f95bdbad…d4bc3bf`.

## Advisories

- Dual-wrong T: `probe-t-skip-both.out`, `probe-t-shrink-B2.out`. Property shape: two enumerations over one `builtStrings`/`thmInfo` filter.
- Wrapper missing-`ok` overlay on a real finding: still nonzero; last line is the marker miss. Seen on receipts 03 and 04.
- `moduleNameOf` dotted-filename non-inverse: `probe-roundtrip2.out`.

## Rows this seat did not independently close

- A2′ constraint 5 on an **existing** non-inversion theorem made `by sorry` through `just lean` (quota-blind justification). Not re-run. Invariants.lean is byte-identical to 5745a2c; poisoned **new** root was used instead.
- Compiled Expr equality of the nine licensed renames against a rebuilt **base** environment. Candidate `#eval` and blob identity to 5745a2c only.
- Compiled consumer scan of the three TraceTests re-exports **on the base oleans**. Tracked-file grep at `4a6cd87` plus candidate env absence only.
- Isolated T-zero (driver branch `theorems.isEmpty`) as its own mutant.
- Project-module shadowing of a toolchain name (`lean/Init.lean`). Owner’s fail-closed-at-build claim untested.
- Combined #70 driver files. Independent roots only.

## Worktrees for the ticket owner to retire

| path | bytes | SHA |
|---|---:|---|
| `/code/reactivegas-66-s2-audit-grok-561347d` | 80398718 | `561347d` detached snapshot |
| `/code/reactivegas-66-s2-audit-grok-561347d-fx` | 26946404 | `561347d` mutation fixture, restored clean |

Shared `/code/reactivegas-66-s2` was not mutated. Prior auditor trees were not touched.

The commissioning owner owns acceptance and any re-cut. This report prescribes no repair.
