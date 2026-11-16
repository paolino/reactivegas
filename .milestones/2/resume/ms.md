# Resume brief — milestone 2 owner (desk)

Load: orchestrator-contract, milestone-orchestrator, context-compiler, worker-protocol, tmux-orchestrator, debrief.
Seat: tmux session `reactivegas`, window `reactivegas-ms2-kelgroups`, one pane, cwd /code/reactivegas.
Desk root: /tmp/reactivegas/ms2/desk-20261108 (STATUS.md via status-event; inbox is the father channel).
Register the pane on taking the seat: worker-protocol/scripts/status-process <desk root> --pane $TMUX_PANE.
Read: ../ledger.md, ../session.md, ../registry.md, then the desk STATUS tail.

State at 2026-11-16T08:25Z: no lane is live. The vote machine landed in kelgroups. One decision is
parked with the operator: how the membership contract is restated for member-owned logs (ledger,
"Parked decision"), which also settles what happens to reactivegas #73 and kelgroups #30 and #29.

Next action, in order:
1. If the operator has answered: record it, and staff what it implies as one ask per lane (launch lines
   in session.md; ask the auditor model in one line first).
2. If not: ask once, as a story with the recommendation, and stay parked. No watch while no lane is
   live; when a lane starts, arm a harness Monitor running a loop of wait-channels on
   /tmp/reactivegas/ms2 filtered to that lane's lifecycle lines (multiple spaces after CHILD) plus
   ANSWER and NOTE, re-arm it every 30 minutes, and read the lane's journal tail by hand at each re-arm.
3. #67, #82 to #84 and #51 are staffing questions for the operator.
Sweep the ledger and the wiki at each landing and at the end of each desk turn.
