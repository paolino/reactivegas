# Plan: provisional milestone release pipeline

Artifact ceiling: 5 KiB / 120 lines.

## Constraints

- Base: `771b3c0b7ed083cf5b3c7778a02df274ab7eab83` on `master`.
- Delivery mode: OWNER; release configuration needs semantic and credential
  review.
- Secret fence: Claude commit owner, then a fresh Codex auditor. Grok and GLM
  are excluded.
- Draft tool: NONE.
- No push authority for delegated seats. Ticket owner may push; only the
  milestone owner authorizes merges.
- The `lambdasistemi-ci` App is not installed for this `paolino` repository.
  R005 therefore uses `workflow_dispatch`, not unavailable secret material.

## Strategy

### S001 — Install the release line

Introduce the manifest-mode version authority, synchronization and drift
guards, tag-bound release packaging, and operator documentation as one
bisect-safe release-infrastructure slice. Reuse the current Nix default server
as the explicitly provisional artifact. Preserve the existing dev-shell build
gate.

Acceptance is `./gate.sh pipeline`, plus a fresh independent audit of every
I001–I008 invariant. The frozen implementation gate is
`/tmp/reactivegas/ms2/t-release-pipeline/gates/s001-pipeline.sh`.

### O001 — Cut and observe the first provisional release

This is an authorized post-merge operation, not another implementation slice.
After the automation PR and its Release Please PR receive milestone-owner
merge authorization, wait for the release job and run
`./gate.sh stranger-fetch <tag>`. Freeze the exact command/output and asset
hash under the ticket runtime `handoffs/` directory before declaring the
ticket complete.

## Live boundaries

- GitHub Release API and downloadable asset bytes are the publication
  boundary.
- `bin/server --help` is the current executable smoke boundary.
- Release Please's `tag_name` output is the provenance boundary.
- Manually dispatched CI on `release-please--branches--master` is the bot-PR
  recovery boundary.

## Verification order

1. Cheap JSON, workflow, path, and version checks.
2. Seeded manifest/Cabal mismatch must fail for the intended reason.
3. Package current Nix output and smoke the extracted archive.
4. Run full local CI inside `nix develop`.
5. After publication, fetch the exact release asset from a clean directory
   outside the worktree, smoke it, and hash it.

## Stop conditions

- Publishing requires a new credential, paid account, or repository transfer.
- Nothing fetchable can be built from `.#default`.
- A required change enters a forbidden path or changes milestone deliverables.
- A merge is required without a milestone-owner authorization record.
