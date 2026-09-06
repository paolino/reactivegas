#!/usr/bin/env bash
# Launcher for commit-auditor-s28r1b (replacement FULL audit, same ticket window).
# Pinned argv: Codex gpt-6-astra, effort high.
exec codex --dangerously-bypass-approvals-and-sandbox -m gpt-6-astra -c model_reasoning_effort=high -C /code/kelgroups-audit-3af3d06 'Read /tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/brief.md in full and act on it. Acknowledge in /tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r1b/STATUS.md. [POINTER-1788653600-s28r1b]'
