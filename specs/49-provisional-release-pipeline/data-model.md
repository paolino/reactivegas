# Data model

Artifact ceiling: 3 KiB / 80 lines.

| ID | Data | Fields / relationship | Validation |
| --- | --- | --- | --- |
| D001 | Release version pair | manifest SemVer; Cabal PVP | manifest `x.y.z` maps only to Cabal `x.y.z.0`; both parse; equality is mandatory |
| D002 | Provisional asset | release tag; platform `linux-x86_64`; archive name; `bin/server`; SHA-256 | tag and name agree; exactly one archive; entrypoint exists and runs `--help` |
| D003 | Release action result | `release_created`; `tag_name`; release URL | packaging runs only when creation is true and checks out `tag_name` |
| D004 | Stranger-fetch receipt | repository; tag; clean directory; downloaded filename; SHA-256; smoke exit | directory is outside worktree and initially empty; download and smoke exit zero |
| D005 | Release PR recovery | branch `release-please--branches--master`; workflow `CI` | manual dispatch targets exact bot branch and produces the normal CI checks |

I001 governs D001, I003–I005 govern D002/D003, I006 governs D004, and I002
governs D005.
