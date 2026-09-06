#!/usr/bin/env bash
# Launcher for commit-auditor-s28r2 (fresh FULL audit, S28-R2 FINAL, same ticket window).
# Pinned argv: Codex gpt-6-astra, effort high.
exec codex --dangerously-bypass-approvals-and-sandbox -m gpt-6-astra -c model_reasoning_effort=high -C /code/kelgroups-audit-ab25cd1 'Read /tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r2/brief.md in full and act on it. Acknowledge in /tmp/reactivegas/ms2/e-kelgroups-substrate/t28-app-api/commit-auditor-s28r2/STATUS.md. [POINTER-1788664000-s28r2audit]'
