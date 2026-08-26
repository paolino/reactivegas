# Modules model

Artifact ceiling: 3 KiB / 80 lines.

| ID | Component | Responsibility | Dependencies |
| --- | --- | --- | --- |
| M001 | Release manifest/config | Own release version and provisional release policy | none |
| M002 | Version tooling | Compare and synchronize manifest SemVer with Cabal PVP | M001, D001 |
| M003 | CI workflow | Run dev-shell build, formatting, HLint, Lean, and version drift checks; permit manual branch dispatch | M002 |
| M004 | Release workflow | Run Release Please on `master`; on release creation build and upload the exact tag's provisional server bundle | M001, M005 |
| M005 | Artifact packager | Turn Nix `.#default` into one versioned archive and prove its staged entrypoint | D002, F003 |
| M006 | Release operator documentation | Explain bot-PR recovery, authorized release sequence, artifact naming, and stranger fetch | M001, M003, M004 |
| M007 | Ticket gate | Reconcile declared versions, workflow wiring, packaged contents, and remotely observed asset | M001–M006 |

Dependency direction is manifest → version tooling → CI/release; packaging
depends on the Nix output but Nix does not depend on release automation.
