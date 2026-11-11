# NOTE-022 — verified impossible wait, not incomplete exporter work

At 11:26 UTC desk read actual child STATUS: coverage promoted, 38c6d06 gate 9/9 GREEN 11:13:42, PROOF-COMPLETE submission1, write-idle. Ticket owner %529 instead runs wait-status on that file with cursor **999999**, parent shell1185575 and exact wait PID1185578 under915875. Pane shows that active command; current completion is below the chosen cursor and can never satisfy it. This is a concrete supervision defect, not absence of implementation or an audit need.

Through your owner role, verify exact current process identity and safely terminate ONLY that obsolete read-only waiter if still running (timeout may finish meanwhile). Do not kill the agent, its process group, a build, or the child. Read existing terminal event before creating any new wait. Require current receipt verification and fresh FULL audit dispatch under existing authorization, then actual verified argv+post-cursor START, no new checkpoint barrier. Record the defective cursor and recovery; fix the wait pattern so snapshot cursor is taken BEFORE dispatch or existing terminal event is handled immediately.

Local files only upward. No candidate change, no spend reset, no merge. Desk has not touched the grandchild process.
