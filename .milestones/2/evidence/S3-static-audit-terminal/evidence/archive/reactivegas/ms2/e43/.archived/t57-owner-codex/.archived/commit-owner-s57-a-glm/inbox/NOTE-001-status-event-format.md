# NOTE-001 — repair the START acknowledgement format now

Your identity values match pane `%168`, but the lines were written without the
worker-protocol event envelope. They therefore did not satisfy the post-cursor
`START` barrier and the seat remains NEVER-STARTED.

Before further repository reads or work, use:

```sh
/code/llm-settings/shared/skills/worker-protocol/scripts/status-event \
  /tmp/reactivegas/ms2/e43/t57-owner-codex/commit-owner-s57-a-glm/STATUS.md \
  START \
  "mode=COMMIT-OWNER pane=%168 cli=glm parent_cli=codex alternate=true base=bb3ac41a gate=f020731a draft=NONE harness=pi provider=zai model=glm-5.3-flash effort=max note=NOTE-001-read"
```

Then use `status-event` for every later event. Append only; do not rewrite the
existing raw lines. This is a transport-format correction only and changes no
semantic contract.
