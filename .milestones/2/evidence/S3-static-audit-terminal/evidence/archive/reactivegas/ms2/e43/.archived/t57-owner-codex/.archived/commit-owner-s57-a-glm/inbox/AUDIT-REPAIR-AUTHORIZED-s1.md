# AUDIT-REPAIR-AUTHORIZED — submission 1, F-001 only

Submission 1 is rejected for one accepted blocking finding. One and only one
repair submission is authorized under the commit-owner contract.

Fresh audit:

- report: `/tmp/reactivegas/ms2/e43/t57-owner-codex/auditor-s57-a-s1-codex/handoffs/audit-report.md`
- report SHA-256: `6a4985eeb95c440dfbf891c86bb49dce06ee928aeacd636141c755deb4e813ba`
- verdict: `FINDINGS`
- finding: `F-001`
- audit builds: `6/20`

Read the report in full and verify its hash before editing.

## Accepted finding

`lean/KelGroups/Vote/Fold.lean:89-96` retains a cast-specific
`isResponsabile signer gs` guard inside `effectedState`. This is redundant on
the current production path but violates the frozen single-boundary
architecture: `Validate` must own all signer authorization, while event-local
effect code assumes an already-admitted event and contains no independent
standing decision.

## Immutable repair gate v3

- gate: `/tmp/reactivegas/ms2/e43/t57-owner-codex/gate/gate-s57-a.sh.v3`
- gate SHA-256: `96fba04a558994d57d6bb12a5ee9fbfcdab442f460e46ab9ccaeee45124f2997`
- manifest: `/tmp/reactivegas/ms2/e43/t57-owner-codex/gate/frozen-manifest-v3.txt`
- manifest SHA-256: `b628b5673f031671393e6fe5ef30d53b4c1034bdd44e0aa3b3ee6a6172f52a9a`
- frozen F-001 RED receipt:
  `/tmp/reactivegas/ms2/e43/t57-owner-codex/evidence/gate-s57-a-f001-red.log`
- RED receipt SHA-256:
  `0fa6d82c5f30613314e0099f220f5aa9c2d3576953e6df8d23448d3c083810b0`
- exact RED reason: `effectedState retains event-local authorization guard`

Gate v3 adds a permanent structural check for this property and moves the
existing focused build before external probes so it works on a pristine audit
worktree. All prior semantic checks, instruments, mutants, proof checks, and
full CI remain required.

## Repair fence and terminal protocol

Resume from clean candidate
`400f5b2829eeae27faeb0994ba8cfcc03c37dd3d`. Repair only F-001:

- remove the cast-local standing guard from `effectedState` so the admitted
  cast effect proceeds directly to question lookup/ballot placement;
- update only directly stale Fold documentation if needed;
- adapt proofs/tests only if compilation genuinely requires it;
- do not alter validator semantics, gates, specs/tasks, other modules, or any
  already-passing invariant surface.

Append a timestamped `AUDIT-REPAIR-AUTHORIZED submission=1 finding=F-001`
acknowledgement before editing. Run focused Lean verification and the exact v3
gate through `run-receipt`. Freeze a repair delta handoff/manifest, create a
clean local repair candidate commit, and submit exactly once as:

`PROOF-COMPLETE submission=2 base=bb3ac41a... prior=400f5b2... candidate=<sha> ...`

Include compact hashes, changed paths, gate receipt, and the F-001 structural
result. Then park write-idle. No final squash, push, PR, or task stamp yet.
