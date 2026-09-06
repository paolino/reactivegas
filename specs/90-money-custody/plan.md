# #90 plan

Ceiling: 70 lines / 6000 bytes. One OWNER slice S90-CUSTODY, tasks T9001–T9005.
The module/data/signature contracts below define the only new production path.
The ticket owner owns these records and acceptance; the alternate GLM owner
owns executable proofs, code, dependencies, CI registration and local commits.

1. Freeze the invariant mandate and command schedule. Capture existing full-CI
   baseline. Gate falsification is the author's first proof phase; do not call
   a missing-component/compiler error a semantic RED.
2. Author establishes the complete RED proof; then implements the four arms,
   frame abstraction and real GroupView adapter and obtains full-CI GREEN.
3. Two fresh blind Codex gpt-6-astra/high inspectors check the frozen committed
   candidate in detached worktrees with distinct fault scenarios: semantic
   boundaries/frames versus provenance/wiring/mutation adequacy.
4. Adjudicate once. At most one repair batch and one delta inspector on the
   second SHA. No third submission or implicit budget expansion.
5. Stamp tasks, verify exact audited-tree correspondence, squash locally via
   author, run final CI, then push and create a draft PR. No comments or merge.

Execution accounting is frozen in the ticket runtime command-schedule.md by
the desk's A-002 ruling: one frozen mandatory full-CI invocation is one unit;
each separate attempt/ad-hoc run costs again. Ten shared; author total ≤4.
The ordinary permanent economic suite may exercise several assertions/fault
injections within one test executable. Independent experiments/retries may
not be wrapped into a new aggregate to evade this counter.

Minimal build wiring is explicitly in scope: two new Cabal packages, root
cabal.project package registration, exact substrate/dependency source pins,
Nix project source declarations and lock entries, test component registration,
`just economic-test` and its mandatory call from `just ci` plus GitHub CI.
All existing CI checks remain. Add observable command/stage exit and elapsed
reporting to the changed recipe; no hidden or skipped stage.

Owned implementation paths: `economic-core/**`, `economic-kelgroups/**`,
`cabal.project`, `flake.nix`, `flake.lock`, `nix/project.nix`, `justfile`,
`.github/workflows/ci.yaml`, `docs/money-custody.md`, and root README link only.
Existing reactivegas.cabal/legacy modules and all Lean sources/corpus remain
unchanged. Small local tool-generated cabal.project.local is ignored runtime,
not a second source of dependency truth. Exact source pins govern both native
Cabal and Nix development paths; no ambient /code dependency.

The pure library never depends on kelgroups, aeson, HTTP, persistence or crypto.
The adapter package depends on accepted kelgroups; its test component can use
aeson, Hspec and QuickCheck separately. Tests execute the production adapter
and pure step, with UTF-8/non-numeric and large-value cases, all selected guards,
balance effects, refusal, frame preservation and observed corpus coverage.

Artifact ceilings: each model 60 lines / 5000 bytes; tasks 35 / 3000.
Compiled owner brief ≤160 lines / 16000 bytes; total product/proof diff ceiling
2000 added/changed lines, excluding dependency lock entries. Return a concrete
scope/signature/budget question before exceeding the fence.
