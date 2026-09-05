# Handover — milestone owner, Reactivegas milestone 2

You are the **milestone owner** for `Reactivegas on kelgroups`. Your pane is the
operator's desk for this outcome. Read this whole file, then
`/tmp/reactivegas/ms2/artifacts/PLAN-TO-MILESTONE-2.md`, then `STATUS.md` from
the end backwards until it stops telling you new things.

**Your model:** `gpt-6-astra`. The operator said "codex astra-6"; `astra-6` is
rejected by the API — I probed it. `gpt-6-astra` is the real id and is the
codex config default. Verified working before this handover.

## Your goal — inherited verbatim

> finish lean quality and simulation. Prepare the full plan, epics and tickets
> to reach milestone 2

Plan, epics and tickets are **done** (below). Lean quality and the simulation
are **running**. Drive them to landed, not to commissioned.

## The outcome, and the thing to keep saying out loud

> A stranger obtains the published coordinator artifact, runs it, and drives a
> gruppo through election → collection → pledge → assenso → purchase → refund
> without touching source.

Operator ruling 2026-09-05: **the milestone is effectively the Haskell
implementation of the Lean model using kelgroups as backend.**

The specification is substantially done — 163 theorems, zero `sorry`, one
membership store, 14 constructors. **The implementation has not started**: 94
Haskell modules, zero mentioning kelgroups; the shipped artifact is the 2021
legacy server. Do not report Lean or simulator progress as milestone progress.
The outcome test touches neither.

## Live lanes

| lane | pane | seat | state |
|---|---|---|---|
| `e-lean-compliance` (#66) | `%503` | opus | assessing `system-design` compliance |
| `e-haskell-impl` (#67) | `%504` | opus | D1 done; **D2/D3 BLOCKED on #73** |
| `t74-corpus-exporter` (#74) | `%505`/`%506` | muse | commit owner running |
| `t-simulator-fable` (#70) | `%313` | opus | re-bind onto `e6c5924` |

Master is `e6c5924`. Four things landed this session: toolchain contract,
release-identity hardening, one-membership model (#62), inversion coverage (#65).

## Issues

#72 epic · #67 Haskell **(critical path)** · #66 Lean quality · #68 proposer
doesn't assent · #69 pledge sovereignty · #70 simulator · #71 design record ·
#73 kelgroups upstream blocker · #74 corpus exporter.
Closed with reasons: #47, #48, #59.

## Blockers you inherit

1. **#73 — 0 of 26 substrate names exist in `paolino/kelgroups`.** Its
   `applyEvent` has no refusal path, so there is nowhere to put a single Lean
   guard. `kelgroups#28`/`#30` are open and unstarted. The milestone spans two
   repositories now; I escalated that to the operator and he has not ruled.
   **A kelgroups lane needs opening — I did not open it.**
2. **No corpus exercises a vote.** `step` returns `none` for
   `openQuestion`/`cast`/`renounce`. The outcome test names **assenso**. The one
   step a stranger must drive has no oracle.

## Standing rules — enforce these, they were each bought with a defect

- **Verify, do not relay.** Check claims at the commit. I was wrong twice by
  generalizing from one checked fact to an unchecked whole (a worktree cut from
  stale `master`; "purely subtractive" when the trace producers weren't in the
  arithmetic). Both times a lane caught me.
- **Pin every model explicitly and verify live argv** before admitting a START.
  Three seats launched on an alias or account default this week.
- **Quantify over the discovered extent, never a list of members.** A manifest
  I commissioned listed one file, and the composition pin drifted silently
  through #62 with the gate green.
- **Every commit gets a STATUS line**, whoever initiated it. Verify by diffing
  `git log` against STATUS — see `JOURNAL-VERIFICATION.md`. Eight commits once
  landed unjournaled.
- **A cleanup instruction must exclude the executor's own pane.** Mine didn't,
  and it killed epic #43.
- **Never `gh issue comment` on your own initiative.** An issue body is an
  artifact and carries no attribution; a comment lands under the operator's
  name. I got this wrong twice in one hour — the second time after being
  corrected by a lane.
- **No lane writes `docs/en/design/` while #71 is open.** Content routes to you.
- Seats: muse for ticket and commit owner; codex or grok interleaved for
  auditing only, never muse, never glm. Merge authorization is **yours alone**.

## Open operator decisions

- Does `Voci/` — 21 modules the group used for years — belong in the model?
- The milestone now spans two repos. Ratify or redirect.

## Do not

Do not respawn epic #43 unless a lane needs it; standalone tickets are working.
Do not report the milestone as nearly done. It is roughly a third done, and the
third that remains is the part the outcome test measures.
