# Identity and operation receipt

The current session is 01a0756f-c8cd-7581-b8cd-48b798c0b8c6. Its own
turn_context/collaboration settings are snapshotted in session-model.jsonl.
The latest current-turn record independently says gpt-6-astra/high; no model
claim is inferred from /proc alone.

The accepted live host read was exactly:

```
tmux display-message -p -t %576 '#{session_name}:#{window_name}.#{pane_index} #{pane_id} #{pane_pid}'
ps -o pid,ppid,pgid,lstart,args -p 3437463
ps -o pid,ppid,args --ppid 3437463
tr '\0' ' ' < /proc/3437463/cmdline
```

Observed pane: reactivegas:rg-s4b-final-audit.1 %576 3435517.
Observed PID/PPID/PGID: 3437463 / 3435517 / 3437463.
Observed launch timestamp as printed by ps: Sun Sep 6 07:37:18 2026
(host local time; not mislabeled UTC).
Observed executable/argv:
`/nix/store/nqhk0522q8ncygwwx054iq76ckcjll82-codex-0.153.2/libexec/codex -m gpt-6-astra -c model_reasoning_effort=high`.

Immediate children were the session's MCP transports, code-mode host and the
read-only identity shell. The listing was not extended to other host processes
or other agents. A preceding request with host-wide ps was explicitly declined
before execution. The restricted read was approved. The sandbox-only initial
read saw its isolated wrapper, and is not credited as the host launch identity.

Live UTC was available through date -u. The owner's supplied 06:39:41Z was not
used as a fabricated observation time. The first START is appended at its actual
UTC time in STATUS.md; the pre-START interval included input reading, the
interruption/amendment, manifest path correction and required identity approval.

Operation classes:

* Project builds, elaborations, probes, mutants, checkers, LSP and Nix: ZERO.
* Read-only Git identity/tree/blob/diff/log operations and filesystem reads.
* sha256sum manifest checks, and Node byte parsing/hashing instruments.
* static-boundary.mjs first hit sandbox EPERM at a Git child read; unchanged
  approved retry passed. Synthetic parser controls ran, not project mutants.
* evidence-reconcile.mjs verified 20 admitted files, 665 prior files, all 82
  command receipts and 164 raw streams; it executed none of those commands.
* Source-sensitive toolchain files were read from the local Nix store only.
* Reports and evidence were written only under this runtime root. No source,
  admitted file, reference worktree or other lane was changed.

No new audit allowance was derived from superseded grants. No prior failure,
model incident, source hash or execution date was erased or reclassified.
