# Specification: provisional milestone release pipeline

Artifact ceiling: 6 KiB / 140 lines.

## P1 user story

As a stranger with no source checkout, I download the milestone's published
provisional server artifact and observe that the packaged executable runs its
smoke command.

## Requirements

- R001: `.release-please-manifest.json` is the sole release-version authority.
  Its initial value is `2021.11.5`; the Cabal representation is
  `2021.11.5.0`.
- R002: Release Please runs in manifest mode on `master` and can also be
  invoked manually. Created GitHub releases are visibly marked prerelease for
  the milestone line.
- R003: a release-please pull request synchronizes the manifest version into
  `reactivegas.cabal` and CI permanently rejects version drift.
- R004: the drift assertion prints both observed versions on success and is
  exercised with a deliberate mismatch that must exit non-zero.
- R005: bot-created release pull requests have an explicit no-secret recovery
  path: CI supports `workflow_dispatch`, and release documentation names the
  exact dispatch command for `release-please--branches--master`.
- R006: a release created by Release Please checks out the emitted tag, builds
  Nix `.#default`, creates a versioned Linux x86_64 tarball, smoke-tests the
  staged `server --help`, and uploads the asset to that GitHub release.
- R007: packaging used in CI is also runnable locally and yields exactly one
  expected asset whose archive contains `bin/server`.
- R008: after authorized release creation, the ticket gate downloads the
  asset from GitHub into a newly created clean directory outside the worktree,
  runs its smoke command, and freezes command output in `handoffs/`.
- R009: no release or tag is created merely by merging the automation branch;
  release publication still requires the separately reviewed Release Please
  PR to be merged with milestone-owner authorization.
- R010: the existing dev-shell CI boundary remains exercised by a
  representative Cabal build; packaged Nix checks do not substitute for it.

## Invariants

- I001 Version seam: normalized manifest version equals the Cabal version;
  mismatch is a non-zero result, not a warning.
- I002 Trigger seam: the bot PR can acquire CI through documented manual
  dispatch without a PAT, deploy key, paid account, or unavailable org App.
- I003 Provenance seam: the published asset is built from the Release Please
  action's exact emitted tag, not a mutable branch.
- I004 Provisional identity: the GitHub release and asset naming make the
  interim milestone status visible.
- I005 Artifact seam: the same packaged bytes uploaded to GitHub contain the
  smoke-tested `bin/server` entrypoint.
- I006 Stranger boundary: acceptance uses GitHub release download from a clean
  external directory; a workflow artifact or local Nix store path is not a
  substitute.
- I007 Safety: the pipeline cannot publish from pull-request execution and
  does not weaken repository protection.
- I008 Scope: forbidden #47/#45 design and Lean paths remain byte-identical to
  the slice base.

## Rejection behavior

The gate fails closed on malformed release JSON, manifest/Cabal drift, a drift
checker that accepts the seeded mismatch, invalid workflow YAML, missing
manual CI dispatch, an incorrectly shaped archive, a failing smoke command,
or an absent/unfetchable GitHub release asset.

## Non-goals

- Implement the future kelgroups coordinator binary or change product logic.
- Modify `docs/en/design/kelgroups-mapping.md`, `lean/Reactivegas/**`, or
  `docs/en/design/state-machine.md`.
- Publish Homebrew, container, AppImage, DEB, or RPM artifacts.
- Introduce a long-lived credential or install an organization App.
- Merge any pull request without milestone-owner authorization.
