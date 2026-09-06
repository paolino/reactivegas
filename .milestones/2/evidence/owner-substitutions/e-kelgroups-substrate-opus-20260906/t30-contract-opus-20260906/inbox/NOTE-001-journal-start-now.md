# NOTE-001 — journal your START now (mechanism, not intent)

You are working (preservation baseline, read-only replay of retained verdict streams, probe tree) but your
`STATUS.md` is still empty. `START` is an authority precondition: until a post-cursor `START` exists in your
own journal, the epic owner must classify you `NEVER-STARTED`, whatever the pane shows.

**Do this before your next analysis step. It is not a request to pause the analysis or to finish the
diagnosis first — admission does not wait on ticket-level diagnosis.**

Run exactly:

```sh
/code/llm-settings/shared/skills/worker-protocol/scripts/status-event \
  /tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/STATUS.md \
  START "<message below>"
```

The message must carry, each value verified by you rather than copied from the brief:

- `pane=` your `$TMUX_PANE`;
- `launch=` the actual argv from `ps -o args -p $(tmux display-message -p -t $TMUX_PANE '#{pane_pid}')`;
- `model=` and `effort=` as they appear in that argv;
- `brief=` sha256 of your own `brief.md`;
- `predecessor-handoff=` sha256 of
  `/tmp/reactivegas/ms2/e-kelgroups-substrate/t30-contract/handoffs/OWNER-SUBSTITUTION-HANDOFF.md`;
- `bases=` `git -C /code/kelgroups rev-parse HEAD` and `git -C /code/reactivegas rev-parse HEAD`;
- `counters=` synthetic campaign spent/remaining, historical pf1 spend, aggregate, product builds;
- `fence=` preparation-only, launch authority not yours.

Then continue the already-authorized analysis without a further checkpoint.

Two things the epic owner has already recorded, so you do not need to defend them:

- your read-only `evidence/replay-verdicts.sh` reads preserved `exit`/`stdout`/`stderr` and does not execute
  the subject — it is not a harness rerun and spends no invocation;
- the epic owner endorses no diagnosis of the 17 mismatches; that verdict is yours.

Acknowledge this note in the same `START` line or as `NOTE  NOTE-001 read`.
