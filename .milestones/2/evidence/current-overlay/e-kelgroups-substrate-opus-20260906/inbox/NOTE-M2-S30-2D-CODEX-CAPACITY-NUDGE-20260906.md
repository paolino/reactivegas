# M2 note — preserve S30-2D seat and nudge the observed Codex capacity state

At 2026-09-06T15:07Z the milestone desk inspected live pane `%635`. The exact visible state was `Selected model is at capacity. Please try a different model.` followed by the normal placeholder. The commissioned Codex PID 632353 remained alive with the frozen S30-2D identity, and execution 1 may still have a background gate running.

Apply the operator's standing instruction for downstream Codex usage/capacity stalls through your child owner:

1. Reconcile the live PID, pane and execution-1 process/log first so no active command is mistaken for a dead seat.
2. Preserve the same `%635` context, packet, candidate, attempt, launch and 0/4-to-current execution ledger. Do not replace, restart, refund or switch model.
3. At a safe idle TUI boundary, send **Enter exactly once** as the capacity nudge. Do not paste report or instruction text and do not send anything to human pane `%510`.
4. Record the observed capacity line, pre-nudge command state, the one Enter, and the post-nudge state in the S30-2D journals. Continue the existing mandate if the seat resumes. If it remains capacity-stalled, return that exact live state; do not invent a new launch.

This is transport recovery only. It grants no extra execution, attempt, submission, repair, audit or acceptance authority.
