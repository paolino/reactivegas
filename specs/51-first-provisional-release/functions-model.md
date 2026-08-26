# Functions model

Artifact ceiling: 3 KiB / 80 lines.

| ID | Interface | Arguments | Result / effect | Constraints |
| --- | --- | --- | --- | --- |
| F001 | `check-release-version` | `manifest_path: Path`, `cabal_path: Path` | exit status plus observed normalized versions | zero only for valid equality; mismatch and malformed input are non-zero |
| F002 | `sync-cabal-version` | `manifest_path: Path`, `cabal_path: Path` | updated Cabal version field | changes only the package version and is idempotent |
| F003 | `package-release-artifact` | `tag: String`, `output_directory: Path` | one versioned `.tar.gz` path | packages Nix `.#default`; archive contains `bin/server`; staged smoke is mandatory |
| F004 | `stranger-fetch` | `repository: owner/name`, `tag: String`, `destination: Path` | downloaded asset, SHA-256, smoke receipt | destination starts clean and outside the worktree; uses GitHub release download |

Names are command-level contracts; placement within the owned release tooling
surface is an implementation decision so long as workflows and the gate invoke
the same interfaces.
