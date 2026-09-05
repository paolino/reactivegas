# S2 candidate audit — AUDIT-FINDINGS

Candidate **5745a2c647c1276cbf90e640a23a44b0d9b8409b** does not satisfy the complete S2 mandate. The unchanged candidate passes independent cold CI, but a registered project module outside the two hardcoded prefixes is excluded from both coverage and axiom inspection. A separate required theorem-inventory truncation control also survives.

This is the commissioned **fresh FULL submission-1 audit**, not an acceptance decision or a repair. No candidate files were edited, staged, committed, pushed, or published. All deliverables are local.

## Frozen authority and independence

- Base: `4a6cd87fcbc3e4a536bbc9f240f5efe5704022af`.
- Mandate v1: `0a1db9887ccc9d8f`; v2: `7cfb7aec95a37448`. Both packet versions, owner brief, and AMENDMENT-1 were read in full; both hashes independently recomputed. Also read and acknowledged NOTE-001, NOTE-002, and A-001.
- Frozen base executable contract independently hashes to `39d6aa4e2c0c0170ff28832f4794293dca64aa3ab65ec6b89ab0924199ce3433`; ordered inputs are in `evidence/frozen-gate-hash.txt`.
- Live launch identity: Codex `-m gpt-6-astra`, PID 850115 under pane PID 849866, pane `%525`, window `reactivegas:4`. Author `%523`/muse and commissioner `%503` are distinct seats in that window. Fresh conversation; no owner contact.
- Snapshot: `/code/reactivegas-66-s2-audit-codex-5745a2c`, detached and initially without `lean/.lake`. It remained mutation-free throughout. A separate detached base snapshot supports compiled before/after comparisons. All mutation sources live in this runtime's isolated `fixture/` and `instruments/`.
- Submission **1/2**; owner budget remains **8/8**. Audit budget **6/6**, including explicit raise **1/2 consumed**, from 5 to 6, for the auditor's fixture error described below. No second raise taken.

## Ranked findings

### F-001 — BLOCKING: registered root modules are excluded by the project-prefix filter

Authorized rows: A1, A2′ constraints 2–5, A5/A7; NOTE-002. Sites: `scripts/check-lean-axioms:122`, `:177`, `:244`, and the existing project filter used by `scripts/check-reactivegas-inversion-coverage`.

In the isolated fixture I added `AuditRootClean.lean` and `AuditRootPoison.lean`, each registered with its own `lean_lib` in Lake. Allocation 5 **successfully built both modules**. Subsequent independent elaborator witnesses returned:

- `IMPORTED AuditRootClean=true`; `AuditRootClean.valid` has no axioms.
- `IMPORTED AuditRootPoison=true`; `AuditRootPoison.usesForbidden` depends on `AuditRootPoison.forbidden`.

I then isolated each already-built module in the tracked source extent, one at a time. Both exact axiom-wrapper invocations exited **1**, reported `tracked=27 walked=27`, retained `built=26` / `theorems=1214`, omitted the added theorem, and reported the module as a tracked source that never reached the environment. This contradicts the independently observed imported environment. The poisoned theorem was **not rejected for its axiom dependency**.

Both `just lean` invocations also exited **1**, earlier at the inversion census: the root source “declares theorems but is not in the audited environment.” Thus the mandatory path independently reproduces the failure; fixing only a registration would not settle it. The census filter predates S2, but its behavior violates the amended added-valid-module requirement now being audited.

Commands and exact receipts:

- Allocation 5: full module list in `evidence/final-runs.json`, entry `05-control-module-build`; successful root compilation lines in its `.out` despite the unrelated audit-fixture failure.
- In the isolated fixture: `just lean`; `scripts/check-lean-axioms <fixture>`.
- In `<fixture>/lean`: `lake env lean <runtime>/instruments/AuditRootClean-env-witness.lean` and the corresponding Poison witness.
- Evidence: `evidence/AuditRoot{Clean,Poison}-{just-lean,axiom-wrapper,env-witness}.{out,err}`. Each command, cwd, status, duration, and stream hash is bound in `final-runs.json`.

Property class: project-owned source identities and actual imported modules disagree because project membership is narrowed by literal prefixes. This is a real clean-extension rejection plus an unswept poisoned theorem, not evidence that the unchanged candidate currently contains an untrusted proof. The inside-prefix added-module control passed and therefore did not exercise this boundary. I did not merge or audit #70's actual driver files; these are independent registered root witnesses for its reported integration shape.

### F-002 — BLOCKING assurance gap: the required T-truncation challenge survives

Authorized row: auditor brief row 1 (“Break each derivation separately and require the gate to notice”), A5 truncated inventory. Site: `scripts/check-lean-axioms:245` theorem enumeration.

An exact driver-body copy, with one checked mutation skipping only `Reactivegas.admin_admission_preservation_holds` in `data.constNames` enumeration, exited **0** and printed `axiom-gate: ok`. Reported theorem occurrences decreased from **1214 to 1213**; the omitted identity disappeared without a reconciliation finding. S and B were unchanged. This is an executed detector mutation, not a source-text assertion.

An auditor instrument separately enumerated `env.constants`, classified `thmInfo`, and attributed declarations with `env.getModuleIdxFor?`. It detects the missing identity in that mutant and agrees exactly with the unmodified gate's **1213 distinct identities**. The unmodified gate prints 1214 occurrences because `KelGroups.setInsert.eq_1` occurs twice in its module-data traversal; that duplicate does not imply a missed theorem.

Command: in the untouched candidate's `lean/`, `lake env lean <runtime>/instruments/t-truncated.lean`, with the full source-module environment listed in `instruments/modules.txt`. Independent comparator: `inventory-check.py`.

Evidence: `t-truncated.out` (exit 0), `axiom-baseline.out` (exit 0), `independent-inventory.out`, and `independent-inventory-comparison.txt`. The compiled inventory comparator rejects the known omission before accepting the original inventory.

Limit: this establishes failure of the expressly required truncation-detection control. It does **not** establish an omitted theorem in the present unmodified two-prefix candidate: that exact extent independently reconciles. It is distinct from F-001's unmodified-gate root-module failure.

## Row ledger

All numbered rows were independently examined. FAIL means an executed counterexample; it does not mean an unrun row. All mandate requirements are treated as blocking unless expressly bounded as historical/out-of-scope.

| Brief row | Result | Independent evidence |
|---|---|---|
| 1. Independent S/B/T derivations and break each | **FAIL** | S truncation/zero rejected; B omission rejected; B-minus-S fired alone; T omission survived (F-002); root extent fails (F-001). Exact current T also independently reconciled. |
| 2. No numeric quota | PASS | Complete diff reviewed; quota declaration/assertions removed without replacement. Valid extension grew census 163→164 and passed; poison at 163 failed by dependency. Text search used only as a lead. |
| 3. Valid theorem/module passes | **FAIL in general** | `03-added-valid-just-lean`: inside-prefix theorem/module passes, 164 declarations. Registered clean root fails (F-001). |
| 4. Omission fails by identity | PASS within existing extent; root limitation F-001 | Added-module import withheld → named S-minus-B; inversion driver names omitted module; source theorem appended without rebuilding → named `auditOmitted` at its source location. |
| 5. Sorry/axiom failure is dependency-based | PASS within existing extent; root limitation F-001 | Existing renamed theorem poisoned: `just lean` fails at axiom gate, census stays 163. Exact old quota driver stays green with expected=163. Custom using theorem and transitive controls rejected by named dependency. |
| 6. Exact three-axiom policy | PASS within swept extent | Original standard set passes; deny-standard-set detector mutation reports each of propext, Classical.choice, Quot.sound; custom axiom/sorry controls fail. Scope defect remains F-001. |
| 7. Nonstandard control actually uses axiom | PASS | `directUse : True := forbidden` compiled and rejected by its named dependency; declaration-only `unused` has no theorem-use finding. |
| 8. Zero S, B, T fails | PASS | `shell-source-zero`, `s-zero`, `b-zero`, and isolated `t-zero-isolated`; latter rejects only zero theorem discovery, without changing header lengths. |
| 9. Panic/result/exit handling | PASS | Actual wrapper with controlled child stdout/stderr rejects `PANIC at` in either stream, missing success row, and child exit 7; clean output control passes. Both real CI receipts have zero panic strings. |
| 10. Added module through mandatory path | **FAIL in general** | Inside-prefix addition passes `just lean`; registered roots fail through `just lean` for the wrong extent reason. |
| 11. Row B bit-for-bit preservation | PASS | Nine compiled declaration types and values equal after exactly nine licensed name substitutions; evaluated tuple `(true,true,true)` on both snapshots. Compiled false-tuple negative control is distinguished. |
| 12. Re-exports dead before deletion | PASS for compiled consumers | Full imported base environment scan reports zero consumers for each of three `TraceTests` wrappers; planted `auditSeed` is discovered. Full candidate CI passes their deletion. External text/recipe sweep was a lead, not the closure proof. |
| 13. No model/theorem statement change | PASS modulo licensed renames | Complete six-file diff and Git object provenance; all model files unchanged. Compiled Expr equality establishes renamed statements and proof terms, including unchanged `by decide` expansion. Six inversion obligations remain and execute in both base/candidate gates. |
| 14. Fence | PASS | Exactly six files, +359/−29, expected modes, one candidate commit over base; `docs/` untouched. Candidate, base and shared checkout remain clean. |

Row C: only the doc-comment path changed to the existing `docs/en/design/state-machine.md`. This is a documentary diff observation, not a fabricated semantic control. No new candidate Lean driver module was introduced; temporary driver generation is outside `lean/`.

## Declared limits, resolved precisely

- **L1 / theorem trust:** executed using-theorem controls reject poison via proof `def`, proof `opaque`, a sorry-valued proof def, and a Nat-valued def mentioned only in a theorem's type. Inspection of the pinned Lean `CollectAxioms` implementation confirms traversal of both type and value through these declaration kinds. No counterexample was found within this theorem-dependency scope. This does **not** establish universal runtime inertness, nor trust for an unused def with no theorem use. F-001 limits which project theorems reach collection in the first place.
- **L2 / B-minus-S:** closed by a dedicated control retaining the compiled/imported added module while removing its source and private-index entry. S equals the filesystem walk; the sole finding is `built project modules outside the source discovery: Reactivegas.AuditAdded` (`b-minus-s-only.out`).
- **L3 / historical spec identifiers:** the old three names remain in the issue-62 historical witness inventory at `specs/62-one-membership-model/functions-model.md:157`. This prose is stale as a current symbol lookup. Leaving it untouched respects the explicit fence; it is not an executable false green and is not an S2 repair finding. Historical-document maintenance remains with the commissioning owner, outside this audit's edit authority.
- **L4 / stale cache:** the owner's zero-recompile receipt proves neither freshness nor provenance. This audit's initially absent `.lake` was built at the exact candidate and never mutated. The independently compiled renamed symbols and final receipt therefore do not rely on owner cache history.
- **Removed transitive import:** removing only the direct `Reactivegas.Trace` import stays green, with the identical independently checked theorem inventory, because the umbrella still imports it. Removing `TraceTests`, which is not transitively imported, fails. Environment reachability is the correct trust boundary for this check; the direct-import result is not itself an omission finding.
- Printed `1214` is a traversal occurrence count, **1213 distinct identities**, as independently reconciled above. Neither is used as a quota.

## Mandatory receipts and provenance

1. **Cold exact candidate:** `nix develop --no-write-lock-file -c just ci`, exit **0**, **144.468 s**, `evidence/01-cold-ci.log`, SHA256 `0fc1d676bfd66ea8e4f88c360712628b689ab8a3fd77a1c2af5b9c28968f369d`.
2. **Exact final command required by NOTE-001:** `nix develop --quiet -c just ci`, exit **0**, **61.330 s**, `evidence/06-final-exact-ci.log`, SHA256 `684bb3818d3843776a4167386e76b377c6182680c03a7d661860d979a2febe46`.

Both execute build/format/hlint, toolchain comparator and negative control, dependency direction, inversion census and negative control, axiom gate, trace agreement and corpus gate. Both show S=26, B=26, T occurrences=1214, census=163/163, axiom success, and zero `PANIC at`. The final warm run's provenance rests on the untouched cold snapshot, **not** on absence of recompilation alone. The commissioner's or author's CI receipt was not substituted.

Wrapper panic controls contain only one injected first-line panic marker per dirty stream, with no backtrace. Marker counts and rejection messages are in `wrapper-stdout.{out,err}` and `wrapper-stderr.{out,err}`. The real dependency-direction scanner also rejects a forbidden import in a separate fixture (`direction-negative-real.log`); the initial attempt passed an unsupported root argument and is excluded as a tooling attempt.

## Failure modes altered

The new wrapper makes source-discovery disagreement, empty extent, failed Lake build, failed elaboration, forbidden theorem dependency, panic output and missing result markers observable as nonzero exit. The source-truncation, missing-import, zero-discovery, poison and wrapper-stream controls exercise those boundaries. Replacing the quota permits a valid inside-prefix extension while retaining identity-level source omission failure; F-001 and F-002 bound that assurance. Temporary-directory acquisition and unreadable-source failures propagate through shell/Lean exceptions; no concurrency or synchronization primitive changed. Those exceptional OS failures were inspected, not exhaustively fault-injected.

## Resource ledger and stopping receipt

| Allocation | Subject | Result |
|---|---|---|
| 1 | Cold exact-candidate full CI | PASS. Initially absent candidate `.lake`. |
| 2 | Cold detached base `just lean` | PASS; supports independently compiled old values/proofs/deadness. |
| 3 | Added valid inside-prefix theorem/module via `just lean` | PASS at 164 declarations. |
| 4 | Existing non-inversion theorem changed to sorry, same census | Expected dependency failure. The batch was interrupted while adapting to NOTE-002, after this allocation had started; charged once, unchanged-input warm rerun captured the complete receipt. |
| 5 | Restored original candidate modules plus root and dependency controls | **Both registered root modules built successfully.** Auditor dependency fixture failed because a Nat-valued axiom-dependent def lacked `noncomputable`. **NOT a candidate finding.** |
| 6 | Corrected `Reactivegas.AuditAdded` only | PASS compilation; dependency gate then rejects all six using-theorem controls for the intended dependencies. Authorized explicitly by A-001. |

**Raise 1/2: 5→6**, solely for the auditor's fixture error. Raise 2/2 remains unused. No additional substantive build was taken. Warm `lake env lean` probes and no-op wrapper builds are uncharged under the brief's accounting rule. Allocation 5's failed module-list build is charged even though partial root evidence was useful. No failed fixture elaboration is counted as a killed candidate mutant.

The campaign stopped after executing the requested finite row/control set, with the two failures above. There are **no independently untested numbered rows remaining**; rows 1, 3 and 10 cannot be closed as compliant, and root portions of rows 4–7 are bounded by F-001. No claim of universal mutation exhaustiveness is made. The actual combined #70 branch and universal runtime behavior of unswept defs were not audited.

## Evidence integrity and retirement

`handoffs/HASHES.txt` binds this report, final command ledger, retained instruments, source fixtures, and evidence. `evidence/final-runs.json` is the canonical latest completed receipt per name; `runs.jsonl` retains dispatch history, including superseded preliminary instrument attempts. Wrong-namespace and formatting-sensitive exporter attempts were corrected before the compiled equivalence evidence was accepted; their failures are not candidate findings. All successful instruments and counterexamples remain local.

Ticket-owner-retirable detached worktrees:

- `/code/reactivegas-66-s2-audit-codex-5745a2c` — 80,427,675 bytes measured before final freeze.
- `/code/reactivegas-66-s2-audit-codex-base` — 26,840,638 bytes.

The isolated mutation fixture is under this audit runtime, not the shared worktree. Its source/instrument evidence is retained; its reproducible `.lake` build tree is retired locally after evidence freeze. No owner artifact or worktree was removed.

The commissioning owner owns acceptance and any permitted single repair dispatch. This report prescribes no candidate repair and makes no external publication.
