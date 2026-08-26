# Tasks: provisional milestone release pipeline

Artifact ceiling: 3 KiB / 90 lines.

## S001 — Install the release line

- [ ] T001 Add manifest-mode Release Please configuration with authoritative version `2021.11.5` and provisional GitHub releases.
- [ ] T002 Normalize `reactivegas.cabal` to `2021.11.5.0` and add reusable compare/sync interfaces.
- [ ] T003 Add release-PR Cabal synchronization and permanent CI drift enforcement.
- [ ] T004 Preserve manual CI dispatch and document the bot-PR-no-CI recovery command.
- [ ] T005 Add exact-tag Nix packaging, staged archive smoke, and GitHub release upload.
- [ ] T006 Document the authorized release sequence, provisional scope, asset shape, and stranger-fetch command.
- [ ] T007 Prove the drift checker fails on a seeded mismatch and the local package boundary yields a smokeable `bin/server`.
- [ ] T008 Keep all forbidden paths unchanged and pass the frozen pipeline gate plus full local CI.

## Post-merge ticket operation (not part of the implementation commit stamp)

The ticket remains open after S001 until the milestone owner authorizes the
automation and Release Please merges, and the ticket owner freezes the O001
stranger-fetch receipt in the external runtime handoff.
