# T76 campaign schedule v1 — frozen before first substantive execution

Authority: brief.md (ASK POINTER-T76-20260906T0950), ceiling 20, author pool 12 INCLUDING owner-operated baseline/final checks, initial inspectors 6 combined, delta reserve 2. At most submissions 1 and 2 and one adjudicated repair. Historical campaigns excluded, never reset by renaming. Metadata reads, source reads and file writes are not substantive executions. EVERY compile, experiment, retry and no-op build counts; there are no free readiness compiles. Setup failures retain actual work and consume the reserved slot conservatively, never a semantic kill.

C = `nix develop --quiet -c just ci`, CWD exact candidate worktree. It invokes the existing repository recipe and ALL mandatory internal stages. Its future composition tests must be registered in the existing Lean build/CI extent. No wrapper may combine independent experiments into one unit.
F = `nix develop --quiet -c bash -c 'cd lean && lake env lean Reactivegas/CompositionTests.lean'`, CWD exact candidate or isolated mutant worktree. One F invocation = one execution. Owner authors the permanent complete regression oracle at that path. New test files must be visible to the Nix source without changing the shared index merely for visibility (commit RED normally).

| Slot | Pool/actor | Exact command | Purpose/input |
|---|---|---|---|
| A01 | author / T.O. | C | fresh accepted base product CI, no semantic inspection |
| A02 | author / GLM | F | complete RED regression on base, semantic unbacked refusal failures for both consumers; pre-interface rows explicitly could-not-evaluate |
| A03 | author / GLM | F | isolated compile-valid producer provenance bypass mutant, both consumers |
| A04 | author / GLM | F | isolated compile-valid target binding bypass mutant, collection and share w |
| A05 | author / GLM | C | complete submission 1 GREEN incl regression, proofs, registered tests, CI |
| A06 | author / GLM | F | reserve: one adjudicated repair RED on rejected candidate |
| A07 | author / GLM | C | reserve: submission 2 complete GREEN |
| A08 | author / GLM | C | post-acceptance final local commit |
| A09 | author / T.O. | C | independent mechanical final exact-SHA pre-push |
| A10 | author / T.O. | scripts/release/check-release-version .release-please-manifest.json reactivegas.cabal | final remote-CI version stage |
| A11 | author / T.O. | scripts/release/check-release-wiring | final remote-CI wiring stage |
| A12 | author / T.O. | scripts/release/check-release-wiring --self-test | final remote-CI wiring control |
| I01 | inspector 1 | C | fresh detached submission CI, provenance/proof trust and both-consumer statements |
| I02 | inspector 1 | F | independent polarity mutation, distinguish positive permission/negative refund and no negative backdonation |
| I03 | inspector 1 | F | independent consumption mutation, both consumers and economically observable reuse |
| J01 | inspector 2 | C | fresh detached submission CI, failure/value/inversion extent |
| J02 | inspector 2 | F | open-verdict authorization mutation and positive reachability controls |
| J03 | inspector 2 | nix develop --quiet -c bash -c 'cd lean && lake build Reactivegas.Composition' | added constructor: wildcard-free classification must reject at intended elimination; distinct structural result, not semantic kill |
| D01 | delta inspector | C | reserve: repaired exact SHA with clean Lean artifacts |
| D02 | delta inspector | F | reserve: named finding delta, frozen single fault |

All units preserve raw output, command and source/oracle hashes, stage exits and elapsed time in the actor root; journal before invocation and after result. The T.O. reconciles each actor ledger into campaign-ledger.tsv. Mutant edit/oracle/source hashes must be frozen before each assigned command; no independent actor or fault bundled into one invocation. The permanent suite may exercise both consumers and distinguishing controls under one single-fault experiment. If the authored implementation cannot support this finite schedule, if an extra fault class needs another execution, or if baseline CI fails, STOP before consuming extra units and return the exact unexecuted branch and concrete budget/scope proposal. Unspent slots do not license a third submission. This schedule is not a claim that six single-fault experiments suffice; acceptance requires the actual finite atom inventory be covered.

Baseline source: efef604de87b2a1efae51e84d1a9150e585c1db0; justfile and .github/workflows/ci.yaml source hashes in evidence/source-hashes.sha256. Rebase is changed input and requires new evidence, never inherited acceptance. Initial proof instrument signatures are prospective until the owner authors them; no missing-file failure counts as RED or gate falsification. The ticket gate is C, immutable runtime wrapper gate-v1.sh; full acceptance additionally includes A10-A12.
