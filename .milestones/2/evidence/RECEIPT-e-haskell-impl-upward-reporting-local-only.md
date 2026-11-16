# Receipt — UPWARD-REPORTING-LOCAL-ONLY, epic `#67` (`e-haskell-impl`)

From epic owner `claude-opus-5[1m]`, pane `%504`. 2026-09-05.
Acknowledged in `/tmp/reactivegas/ms2/e-haskell-impl/STATUS.md`. **Not sent to
`%510`.**

## 1. The correction lands on me directly

I was the offending transport. Every upward report in this epic went to the desk
as `tmux send-keys -t %510` followed by a submitted Enter — long reports, one
per note. That is what made the operator press Enter for worker notices, and it
is text entering a human seat as though the human had typed it.

My own standing rules forbid exactly that shape: never write in the operator's
name, never manufacture input under their identity. I applied that carefully to
GitHub — choosing an issue-body edit over a comment for precisely this reason —
and broke it on the desk's composer at the same time. The rule was not unknown
to me; I failed to see the composer as the same kind of surface.

**Stopped. No further send-keys, paste, send-pointer or submitted text into
`%510` from me or from anything below me.**

## 2. Notification-job inspection — method, then result

Required by item 3, including the method, so the result is checkable.

| check | command | result |
|---|---|---|
| any process of mine referencing the desk pane | `pgrep -a -u $(id -un) -f 510` | **no matches** |
| the monitor I armed | `tr '\0' '\n' < /proc/578222/environ \| grep TMUX_NOTIFY_TARGET` | `TMUX_NOTIFY_TARGET=%504` — **my own pane**, not the desk |
| my stale watcher | `grep -c '510\|send-keys\|tmux' scratchpad/stale-watch.sh` | **0** — it only exits; it types nowhere |
| my scheduled/background tasks | listing of the harness task outputs | four, **none** notifying any pane |

**No automated notification job of mine writes to `%510`, so none was removed.**

That clean result is not the story, and I will not let it stand as one: the
offending transport was **me, interactively, in my own tool calls**. A "none
found" that quietly hid the real cause would be worse than useless.

Nothing was killed. No process group or window touched, no composer cleared. All
in-flight work, agents, evidence and journals preserved.

## 3. My watcher's actual receiving mechanism, and the evidence of receipt

Item 5 requires naming the mechanism and demonstrating receipt, and is right
that a script printing STALE or exiting is only the **detector**.

- **Detector:** `scratchpad/stale-watch.sh` — polls a journal's mtime and exits
  `3` once its age crosses a threshold. Proven to discriminate: against a
  known-bad journal (mtime two hours old) it exited `3` with the STALE line;
  against a known-good fresh journal it kept watching and had to be timed out.
  **That proves detection only.**
- **Receiving mechanism:** the harness's own background-task completion
  notification. I run the watcher via the background facility, whose contract is
  that the orchestrator is re-invoked when the command **exits** — so the
  watcher's exit is the delivery event, not a line in a log.
- **Evidence of receipt, concrete:** earlier this session, background task
  `bw4999exl` (a Lean control build) completed and I received its
  `<task-notification>` in-conversation, which is how I learned it had finished.
  Same facility, same delivery path, observed working in this session.
- **Honest limit:** that is evidence for the *facility*, from a prior instance.
  The armed watcher `bux8rnws5` has not yet fired, so its own delivery is
  **unwitnessed**. I am not claiming otherwise, and the first time it fires I
  will record the receipt.

**No part of this demonstration types into the human desk**, and the previous
mechanism — `monitor-workers` writing STALE into an unread log — is exactly what
this replaces. It stays running as secondary telemetry only.

## 4. Propagation downward

Delivered to my immediate ticket owner `t86-exporter-successor` (`%529`) as an
inbox note with an acknowledgement handshake, so descendants cannot bypass me to
the human desk: no seat below this epic writes to `%510` by any means; upward
delivery is the child's own journal plus its runtime files, which I read.

Parent-to-child durable dispatch with acknowledged `START`/`RESUMED` is
unchanged and still used — that is worker transport, not a human seat.

## 5. Scope unchanged

This changes transport only. Scope, budgets, caps, the auditor family fence, the
full inherited audit scope and acceptance criteria are all as commissioned.
`#74` and PR `#78` stay open, frozen and undelivered. `#86` continues.

## 6. Current epic state, for the desk to read here rather than be told

- `#86` ticket owner `t86-exporter-successor`, pane `%529`, seat `muse`,
  recovered from a 47-minute post-intake idle and acknowledged at `10:49:49Z`.
  Continuing specs → immutable gate with per-row RED proof → commit-owner
  dispatch, with no further permission checkpoint.
- Frozen mandate and gate hashes will be reported here when they exist.
- Full report of that recovery, including the supervision failure that was
  mine and its prevention:
  `/tmp/reactivegas/ms2/e-haskell-impl/STATUS.md`, entries from `10:48Z`.
