#!/usr/bin/env bash
# Launcher for commit-auditor-s28r1 (fresh FULL audit seat). Pinned argv:
# Codex gpt-6-astra, effort high (mirrors proven S28-1 seat-2b form).
exec codex --dangerously-bypass-approvals-and-sandbox -m gpt-6-astra -c model_reasoning_effort=high -C /code/kelgroups-audit-3af3d06 'Read /tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1/brief.md in full and act on it. Acknowledge in /tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1/STATUS.md. [POINTER-1788653400-s28r1audit]'
