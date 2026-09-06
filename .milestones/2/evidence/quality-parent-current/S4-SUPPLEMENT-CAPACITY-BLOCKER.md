# S4 supplement — capacity blocker, returned as a decision

## The fact

`%578` (`gpt-6-astra`, effort `high`) was launched and **STARTed at
2026-09-06T07:18:19Z**. Its own START event records that it verified the live
model and effort, the candidate and tree clean, and all 11 `INPUTS-MANIFEST`
entries. It then hit, verbatim from its live pane:

```
■ You've hit your usage limit. Visit https://chatgpt.com/codex/settings/usage
  to purchase more credits or try again at Sep 7th, 2026 8:28 AM.
```

Its boot banner earlier reported:

```
• You have 3 usage limit resets available. Run /usage to use one.
```

**Current state:** `STATUS.md` contains the `START` line and nothing else. **No
findings, no report, no verdict.** The AMENDMENT-1 pointer sits unconsumed in the
composer. Nothing is lost — the frozen inputs, the amendment and the brief are all
on disk in its root.

## Why this is not mine to resolve

The commission itself is authorized and needs no further checkpoint. **Spending
one of the three usage-limit resets is a different thing**: it consumes an
account-wide resource shared by every codex seat on this host, across the
`reactivegas`, `keri`, `wallet` and `treasury` lanes. NOTE-072 grants the
commission; it does not address that resource, and an authorization in one context
does not extend to it. Waiting for the natural reset costs **~25 hours**.

I am not spending a reset on my own judgment, and I am not killing or restarting
`%578` — its context, START and verified inputs are intact and worth preserving.

## What the options actually are

| | effect |
|---|---|
| spend one reset on `%578` | it resumes from its verified START with its full context; 2 resets remain for the other lanes |
| wait for the natural reset | ~25 h; `%578` resumes intact; costs nothing |
| stand the seat down | the supplement is not delivered; FS-01 stays unadjudicated; the conditional push/draft-PR preconditions stay unmet |

## What is unaffected and already done

- **FS-02 CLOSED** — closure map repaired and, under NOTE-073, brought current.
- **FS-01 assessed** at my altitude with its four stated limits.
- **S3 successor author `%580`** dispatched and STARTed 07:25:19Z on a different
  provider; **it is not blocked by this.**

No merge, no push, no PR, no `#66` closure is proposed or possible from this
state.

---

## RESOLVED — 2026-09-06T07:27:42Z, not by me

`NOTE-USAGE-LIMIT-NUDGE-20260906.md` was written directly into `%578`'s inbox
from outside this lane: *"The operator requested one nudge of downstream Codex
workers stopped by usage limits."* It preserves the model, candidate,
zero-project-execution scope, budget and independent verdict, and states that
**no new scope or repeated retry loop is authorized**.

`%578` acknowledged both AMENDMENT-1 and the nudge at **07:27:42Z** and resumed
work on its unchanged mandate. **I spent no usage reset, killed nothing and
restarted nothing**, and I will not issue a second nudge — the note authorizes
one, and a retry loop is explicitly excluded.

The options table above is therefore closed: option 1 was taken, by the operator.
