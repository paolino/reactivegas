# Re-cut mandate: publish first, harden second

Issue: https://github.com/paolino/reactivegas/issues/51

This directory inherits R001–R010, I001–I008, and the M/D/F models copied from
the exhausted #49 campaign. The milestone owner's ruling changes execution
order, not the intended release line:

1. S001 establishes the carried candidate as publication-ready, merges only
   with milestone-owner authorization, and closes only when an actual
   provisional GitHub Release asset is downloaded into a clean directory and
   `bin/server --help` succeeds. The immutable stranger receipt is O001.
2. S002 starts only after O001 and hardens the two carried audit findings.

For S001, C001 and C002 below are explicit `DEFERRED-S002`, authorized by the
milestone owner. They may not prevent existence of the first artifact. All
other inherited requirements and invariants remain blocking.

## C001 — exact artifact identity (S002 BLOCKING)

Exact equality must hold among requested tag, filename tag, and exactly one
complete declared-tag record. Missing, extra, prefix/suffix-colliding, or
mismatched identity must fail closed, leaving no uploadable producer artifact.

Frozen predecessor evidence:
`/tmp/reactivegas/ms2/t-release-pipeline/.archived/auditor-2/evidence/b2-identity.sh`
SHA-256 `f12f57422823484a19b2c1afe10364a0607846f510cae6270023ceedf0f07db1`.

## C002 — non-vacuous PVP proof (S002 BLOCKING)

Permanent domain tests must distinguish malformed PVP rejection from drift
and kill a PVP-only broad-validator mutant while retaining `0.1.0.0`.

Frozen predecessor evidence:
`/tmp/reactivegas/ms2/t-release-pipeline/.archived/auditor-2/evidence/b3-pvp-mutation.sh`
SHA-256 `d8097ee406f081f3c84f47a881a8e79065e6a306a229b2dadb1652f7af6e5c42`.

## S001 acceptance

- carried candidate `ab52f23b08d000bf4f4d682b570960e79120b875` and tree
  `9eb4054123a27a50b53cb76c874f953cf55fd1ef` are exact;
- full frozen gate v2 passes and Release Please, manual recovery, exact-tag
  checkout, Nix packaging, prerelease marking, smoke, and upload are reachable;
- no forbidden path changes;
- after authorized pipeline and release-PR merges, `gh release download` from a
  clean outside directory obtains the artifact, freezes its SHA-256 and command
  output, and smokes extracted `bin/server`;
- C001/C002 are recorded, never mistaken for closed, and begin S002 after O001.

Merge authority remains exclusively with the milestone owner.
