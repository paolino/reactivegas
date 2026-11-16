# Q-001 — Missing frozen inputs

## Context

The audit brief binds three frozen inputs under:

`/tmp/reactivegas/ms2/e43/t57-owner-codex/inputs/`

- `issue-57.md` — expected SHA-256 `18dd3cfe8c90696e603ee3ea24b912779a164f0e78f067b106b252d453747b16`
- `HANDOFF-to-57.md` — expected SHA-256 `bb5bd5b2b5ce61a8610e83448b07fae58a558461a0e90fab62a59ec0089e8109`
- `audit-report.md` — expected SHA-256 `835f79e67a62a787074af9ae59986451f2f67f75952fe52139aa467467cd6e53`

The `inputs/` directory does not exist and none of these exact paths is present. A wider read-only search found likely upstream originals for the latter two, but their hashes differ:

- `/tmp/reactivegas/ms2/e43/t54-vote-coverage/handoffs/HANDOFF-to-57.md` — `bb5bd5b2bf49aad2d24b3d71b17e8e16b464d0ba0674aed428fa5c826f2c4c64`
- `/tmp/reactivegas/ms2/e43/t54-vote-coverage/auditor-slice-a-s2/handoffs/audit-report.md` — `835f79e6ec605871ca64b3cee2d72b55e495fb02d852b65215522eb4280fc3de`

No `issue-57.md` exists anywhere under `/tmp/reactivegas`.

The brief requires every declared hash to be verified and says a mismatch is a finding/blocker. The commit-auditor contract also forbids silently substituting unbound inputs.

## Options

1. Restore the three exact frozen snapshots at the brief-bound paths with the declared hashes, then wake this auditor with the matching answer.
2. Issue a durable corrected brief/inbox note that binds replacement paths and hashes, acknowledging the packet revision, then wake this auditor.
3. Terminate this audit as a contract-blocked dispatch and launch a fresh auditor after rebuilding the packet.

## Recommendation

Option 1 if the frozen snapshots still exist; otherwise option 3. Avoid rebinding to the visible upstream originals without first explaining and freezing the byte differences.

## Decision needed

Provide the exact authoritative frozen inputs or direct termination/re-dispatch.
