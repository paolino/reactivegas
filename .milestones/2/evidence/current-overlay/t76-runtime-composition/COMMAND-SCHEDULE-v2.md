# T76 command schedule v2 — prospective dependency repair, no ceiling raise

Amends COMMAND-SCHEDULE.md v1 after A03 reproduced stale-import setup failure. Original bytes and A01/A02/A03 spend remain retained. Shared ceiling20 author-pool12 initial-inspectors6 delta2; submissions0, max2; one adjudicated repair. Actual used: A01 baselineCI, A02 RED, A03 failed setup =3. A03 is not a semantic kill. No free readiness builds.

The accepted execution unit remains exactly the ASK e5017ee rule. Reject `bash -c 'lake build && lake env lean ...'` as a newly bundled single unit: distinct ad hoc invocations cannot be made one merely by a shell wrapper. A direct invocation of the ALREADY EXISTING repository `just lean` recipe is one unit INCLUDING ITS EXISTING MANDATORY stages; unchanged recipe bytes are bound by v1 source hashes. The permanent oracle must be in its registered compiled/test extent and its intended assertion must execute. If that cannot be established without another command, block before that command. Syntax/import/proof setup failures or unrelated theorem failures cannot be semantic kills.

## Remaining actual schedule

L = `nix develop --quiet -c just lean`, from the exact candidate or frozen one-fault mutation worktree. Full ci C remains `nix develop --quiet -c just ci`.

- A09 (reassigned to author, formerly redundant T.O. final C): provenance-mutant retry, L. Preserve single-edit/oracle hash and prove actual producer/root assertion is reached.
- A04: target-mutant run, L. Same exact-one-fault and actual loaded source requirements.
- A05: submission1 GREEN, C, all mandatory stages and registered permanent tests.
- A06: reserve repair RED, L (prospectively replaces stale F).
- A07: reserve repaired submission2 GREEN, C.
- A08: complete FINAL exact-SHA C, run ONCE by T.O. after owner final-commit and mechanical tree equivalence. Owner's final handback includes its mechanical commit/tree receipt and delegates this one CI execution to parent. The parent cannot push before A08 passes.
- A10: `scripts/release/check-release-version .release-please-manifest.json reactivegas.cabal` final release-version stage.
- A11: `scripts/release/check-release-wiring` final release wiring.
- A12: `scripts/release/check-release-wiring --self-test` final release control.
- I01: first fresh inspector exact candidate C.
- I02/I03: independent polarity/consumption single-fault L runs, both consumers.
- J01: second fresh inspector exact candidate C.
- J02: independent open-verdict single-fault L run plus suite's positive controls.
- J03: frozen added-constructor structural command unchanged from v1.
- D01: reserved fresh delta inspector exact repaired candidate C.
- D02: reserved single-finding L instead of stale F.

L does not wrap independent experiments or actors. One mutation per invocation. Build output/actual module ownership must bind the relevant candidate. Never relabel compile failures as an economic witness. Oracle hashes may change only for disclosed mechanical interface adaptation or added CNE-row coverage, with original RED evidence retained; no weakened expected property. Every changed oracle requires its actual relevant falsification evidence before acceptance.

## Why final verification remains complete

The v1 A08 author final CI and A09 T.O. final CI were repeats on the same final tree. Keep the actual independent T.O. run at A08; remove the duplicate author run. The owner still mechanically proves final tree = audited candidate plus exactly the task stamp and passes the commit gate; T.O. independently verifies that identity and runs full C at the actual final SHA, then A10-A12, before push. The developer instruction to avoid redundant reruns on unchanged passing inputs takes precedence over the generic role-skill duplication. This neither removes a CI stage nor an independent semantic inspection, nor makes the task stamp change semantic. Author-pool spend remains capped12, including all T.O. executions; total20.

Ordering may place A05 before L mutations if the owner needs its already-budgeted whole-candidate build first. This does not create an extra GREEN rerun: a later mutation must restore the candidate exactly, and final A08 still runs from the actual final SHA. Each slot is used at most once. A failed actual product build still consumes its slot and may make the remainder unaffordable. If corpus regeneration, proof compilation, clean controls or the discovered mandatory finite atom inventory needs extra units beyond this schedule, return the exact missing branch and cost proposal before spending. The schedule does not assert that unknown source is compile-valid or that the prospective mutants suffice.
