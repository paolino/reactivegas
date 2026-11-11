# A-001 — corrected frozen audit packet; resume submission 1

Q-001 is accepted. The ticket-owner audit dispatch contained stale/truncated
path and hash transcriptions. This is a dispatch-packet defect, not a candidate
or evidence mutation. You spent `0/20` builds and stopped before substantive
audit work, so resume the same fresh process, detached worktree, candidate, and
submission after verifying this correction.

This note supersedes **only** the following path/hash declarations in the
original audit brief. Treat this table as authoritative:

| Input | Authoritative path | SHA-256 |
|---|---|---|
| issue #57 frozen body | `/tmp/reactivegas/ms2/e43/artifacts/issue-slice-a-structural-validation-recut.md` | `18dd3cfe9ae6f42a5ca1324419436893f87106e17f449034a1fc2791b21cedf9` |
| resurrection handoff | `/tmp/reactivegas/ms2/e43/t54-vote-coverage/handoffs/HANDOFF-to-57.md` | `bb5bd5b2bf49aad2d24b3d71b17e8e16b464d0ba0674aed428fa5c826f2c4c64` |
| final #54 audit | `/tmp/reactivegas/ms2/e43/t54-vote-coverage/auditor-slice-a-s2/handoffs/audit-report.md` | `835f79e6ec605871ca64b3cee2d72b55e495fb02d852b65215522eb4280fc3de` |
| final #54 campaign ledger | `/tmp/reactivegas/ms2/e43/t54-vote-coverage/auditor-slice-a-s2/handoffs/campaign-ledger.md` | `9667b9f048dbb02fc2a9aa09c40139d3674b340005efd113c95c0c267df33d98` |
| #57 campaign ledger | `/tmp/reactivegas/ms2/e43/t57-owner-codex/campaign-ledger.md` | `9455ecdaa892393c59a4c0bdc809e459907335d773bddb358753873f57137999` |
| spec.md | `/code/reactivegas-issue-57-audit-s57-a-s1/specs/57-structural-vote-validation/spec.md` | `92a00ef4e36cdbebdfe76bf6196c48998e3a95399b0004bd3498dc2ec75654cd` |
| plan.md | `/code/reactivegas-issue-57-audit-s57-a-s1/specs/57-structural-vote-validation/plan.md` | `b5d41adbbf8305ce7199062634b20b267a2fda9b08c358e2b9593bd0931b207b` |
| modules-model.md | `/code/reactivegas-issue-57-audit-s57-a-s1/specs/57-structural-vote-validation/modules-model.md` | `d089349a1e19562c088ae4205be6289283703beac082720b4dbd2e7a05d0ee9e` |
| data-model.md | `/code/reactivegas-issue-57-audit-s57-a-s1/specs/57-structural-vote-validation/data-model.md` | `8b1604dd0e1d7dd066611c419b3439e8091aa5b1ff5e310108f56cadc9454720` |
| functions-model.md | `/code/reactivegas-issue-57-audit-s57-a-s1/specs/57-structural-vote-validation/functions-model.md` | `b6f8a8b42d6c6476f1766b631e0da0d2b3017c338bdf0cfb994ba6c680e9913c` |
| tasks.md | `/code/reactivegas-issue-57-audit-s57-a-s1/specs/57-structural-vote-validation/tasks.md` | `51aad9dfa2e2eca2d87fe43bf9217325a5ac21784bdf00f14b98155704ca005d` |

The authoritative paths and hashes above are copied from the fully read
ticket-owner brief and independently re-hashed immediately before this answer.
The visible upstream files are the intended frozen inputs; do not create or
copy replacement snapshots.

All other original audit-brief bindings remain unchanged, including candidate,
base/tree, submission receipt, RED/GREEN handoffs, gate v2, manifest v2, and
gate evidence hashes.

After verifying this table, append a `RESUMED` event naming A-001 and continue
the full audit. The report must disclose this packet correction and state that
it occurred before any audit build (`0/20`) and did not change candidate or
gate semantics.
