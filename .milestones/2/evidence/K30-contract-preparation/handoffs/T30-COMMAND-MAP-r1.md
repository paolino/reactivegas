# T30-COMMAND-MAP-r1 — frozen requirement-to-command/control map (kelgroups #30)

Companion to `T30-CONTRACT-r1.md` §7 (same frozen content, row-addressable).
Preparation owner `t30-contract`, preparation-only (spend 0; commands NAMED
here, never executed by this seat). Base kelgroups `main` @ `933e385d`;
accepted Lean Reactivegas @ `3590c001` (zero-diff re-verified EMPTY vs
`4a6cd87` on `lean/KelGroups/Vote/` + `Integration.lean` + `State.lean` +
`Validate.lean`).

Conventions (binding): whole-project invocation = 1 BUILD (expected-RED,
warm reruns, per-mutant runs all count). Per-mutant cycle = apply + run +
revert with hash-verified restore (failure aborts exit 3). Kill counts iff
non-zero exit AND `Failures:` names ≥1 registered example of the row (M4
needs both placement witnesses) AND class matches (build-RED:
unification/exhaustiveness quoting ctor/site, no parse error; test-RED:
witness-quoted). Empty `Failures:`, crashes, timeouts, infra, parse errors
NEVER count. `pendingBase` compile-RED = narrow interface-existence claim
only ("no admission constructor is encodable"), never behavioural refusal.
No source-token search, no shrinkable fixture inventory, no absent-API
import failure cited as behavioural evidence. Charge-0 recon (reads, greps,
`git status/diff/rev-parse/log`, `gh issue view`, `--version` pin reads):
free, never evidence.

Toolchain (observed read-only; versions re-pinned exactly at ticket freeze):
`nix develop .#ci --quiet -c` wrapping `just build` (`cabal build all -O0`),
`cabal test all -O0 --test-show-details=direct`, `just ci` (format +
cabal-fmt + lint + build + test + `cd lean && lake build` + `cd client &&
spago build` + `spago test -p kelgroups-client`). Suite vehicle:
`kelgroups.cabal` `test-suite invariants` registered via `test/Main.hs`.

## Rows

- MAP-RED (T30-1): C1 `cabal build all --enable-tests -O0` expecting failure
  quoting the exact absent Vote API names (BUILD 1); C2 `cabal test all -O0`
  expecting the proving spec absent-failure with zero historical breakage
  (BUILD 1). Controls: zero-extent + S28 positive control (same commands
  succeed on LANDED S28 names). Shares with nothing (first cost).
- MAP-R30-1 (open/admit): shared leg-4 full-suite run + charged `--match`
  probes per witness. Kill M1 (openQuestion-nonresponsabile bypass →
  test-RED quoting refusal-before-effects witness).
- MAP-R30-2 (cast/place): shared leg-4 run + probes. Kills M2
  (cast-nonresponsabile bypass → test-RED), M3 (unknown-question accept →
  test-RED quoting `questionNotFound` witness), M4 (placement mutant dropping
  erase-other-list → test-RED quoting BOTH switch-moves AND idempotent-recast
  witnesses, one run).
- MAP-R30-3 (sweep/retain): shared leg-4 run + Store/KEL append and
  `foldIntegratedFrom` replay-equality at the persistence boundary (in
  leg-4/6). Kills M5 (tally-positive suppress → test-RED), M6
  (dissent-negative suppress → test-RED quoting negative-delivery witness),
  M7 (franchiseChange→tally collapse → test-RED), M9 (close-and-discard →
  test-RED quoting retention witness).
- MAP-R30-4 (verdict): shared run + permission-ignores-tally property. Kill
  M8 (permission-tally consultation → test-RED quoting permission witness).
- MAP-R30-5 (refusals): 4-ctor vocabulary + 3-arm exhaustive validation via
  shared cold build. NO producing-semantics mutant (unruled). Guard
  (recon-only, never a kill): any `notDesignee`/`notProposer` construction
  site outside the vocabulary declaration fails review + gate.
- MAP-R30-6 (franchise): canonical-view recompute property in the shared run.
  Kill: franchise-snapshot mutant (stash membership in payload → test-RED
  quoting recompute witness); M7 co-covers stale-tally cause.
- MAP-R30-7/14 (negative delivery): observed through
  `applyIntegratedEvent`/`appendIntegratedEvent` at the integrated boundary
  in the shared run. Kill M6 (above) quotes the witness at that boundary.
- MAP-R30-8 (route separation): 2-arm exhaustive `BaseMutation` enactment in
  the shared cold build. Kill M10 (added admission ctor → build-RED,
  exhaustiveness failure quoting the ctor; narrow claim only).
- MAP-R30-9 (approve/V-2): freeze on current base (shared runs). Conditional
  audit-time rebind check iff `paolino/reactivegas#68` landed (`tryEnactBase`
  majority + proposer rules, AUDIT-BUILD from the auditor cap). No
  anticipation mutant now.
- MAP-R30-10 (ruled lifecycle): close/record/cause/retention/atomicity
  mechanism surface in the shared run at the integrated boundary. Kills M11
  (hook-refusal-ignored → test-RED quoting restoration witness) and
  cause-collapse (renounced→tally → test-RED quoting cause witness). L-7
  refund gated on `paolino/reactivegas#76`, never mocked.
- MAP-R30-10U / MAP-R30-5-PROD: no commands, no mutants, no edge (preserved
  boundary; recon guard only).
- MAP-R30-11: evidence exposure only; no wire, no mock wire.
- MAP-R30-12 (client): `spago build` + `spago test -p kelgroups-client`
  (BUILD, leg-6) + Api propose/vote roundtrip at the test boundary. Kill M12
  (dropped propose path → client-test-RED quoting roundtrip witness at the
  actual client boundary).
- MAP-R30-13 (Lean): inherited `lake build` green only (leg-6); content owned
  by Reactivegas lanes.
- MAP-COLD-FINAL: cold `just build` 1B (COLD/WARM logged; first compile =
  entire cost) + final `just ci` 1B + tracked-clean before/after + `Trivial`
  presence-only + founding-mismatch guard. Serves all rows.
- MAP-SLIM: identical-envelope 3B (legs 1,2,2b,3,4,6,7 analog).
- MAP-AUDIT: exact table pre-dispatch (mandate rows + reliances +
  conditional R30-9 rebind check); fresh FULL Codex-or-Grok audit, clean
  detached worktree at candidate SHA, argv-pinned model+effort, START,
  hash-bound report. Auditor kills verify, never count toward owner fit.

## Discovery bounds

Extent quantified over observed Lean equation sites: `placeBallot` 2 arms
(`Vote/Fold.lean:53-56`); `sweepStep` 2 arms (`:65-66`); `effectedState` 3
arms + 2 sub-arms (`:89-101`); `validateVoteEvent` 3 arms + 2 sub-arms
(`Vote/Validate.lean:57-70`); `verdictOf` 2 kind-arms
(`Vote/State.lean:87-96`) + `closureCause` 3 arms (`:111-113`);
`sweepClosures` shared filter+filterMap step (`Vote/Fold.lean:72-76`). 12
mutants cover the 12 distinct behavioural sites (M4: two witnesses, one
run — stated). Later Lean ctors break exhaustive Haskell matches at compile
time ⇒ new site, new mutant, never silent pass. Inventory + registered +
execution legs fail closed on empty/truncated sets; the guard is falsified
by the RED-equivalence absence run.

## Fit (frozen with this map)

RED 2 + GREEN 15 (cold 1 + full-test 1 + M1–M12 12 + final CI 1) + SLIM 3 =
20 builds; probes ≤24 counted; charge-0 recon free per the enumerated list.
PROPOSED owner ceiling 20/24; PROPOSED auditor ceiling 12/24 (A1 recon 0B;
A2 cold 1B; A3 test 1B; A4 CI 1B; A5–A9 ≤5 disputed-kill reruns 5B; A10–A11
boundary reruns 2B; A12 conditional R30-9 check 1B; 1B repair-verification
reserve). Both PROPOSALS pending fit-proof at freeze + authorization; gap
returns exact workload/cost, never trimmed scope.
