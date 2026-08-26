# Tasks: first provisional release, then hardening

Artifact ceiling: 3 KiB / 90 lines.

## S001 — Publish and fetch the first artifact

- [x] T001 Carry the #49 candidate and archived audit evidence exactly into the re-cut.
- [x] T002 Recheck Release Please, manual bot-PR recovery, prerelease marking, exact-tag checkout, Nix packaging, smoke, and upload reachability.
- [x] T003 Pass frozen gate v2 and full local CI without forbidden path changes.
- [x] T004 Push the accepted publication commit and obtain green required checks.
- [x] T005 Obtain milestone-owner authorization before each merge.
- [x] T006 Merge the pipeline PR, recover and merge the Release Please PR, and publish the provisional GitHub Release.
- [x] T007 Download the release asset into a clean directory outside the worktree, record its SHA-256, extract it, and smoke `bin/server --help`.
- [x] T008 Freeze the stranger-fetch receipt as O001; do not claim C001/C002 closed.

## S002 — Harden after O001

- [ ] T009 Make requested tag, filename tag, and one complete internal declaration exactly equal; reject missing, extra, and prefix/suffix collision cases.
- [ ] T010 Make producer identity failure leave no uploadable artifact and make the stranger verifier reject unrelated extra assets.
- [ ] T011 Make PVP-domain failure distinguishable from drift and kill the frozen PVP-only broad-validator mutant.
- [ ] T012 Re-audit the inherited mandate, retain live stranger fetch, and publish the hardened follow-up under milestone-owner merge authority.

## Post-merge ticket operation

S001 remains open until O001 exists. S002 cannot begin before O001. The issue
remains open until S002 and its final external receipt pass.
