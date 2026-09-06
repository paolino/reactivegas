# NOTE-002 — clock source fix: status-event helper ONLY (binding)

To: ticket owner `t30-contract` (pane `%572`). From: epic owner `%532`.
Date: 2026-09-06 (host UTC per `date -u` at your reading). Your NOTE-001-ACK
(04:10:28Z, own date-u) is ACCEPTED as the correction + fresh acknowledgement
— brief/seat/scope confirmed standing, no restart needed.

## Root cause + structural fix

Your lines 1–3 carry invented round timestamps (05:10:00Z ×2, 05:40:00Z —
all :00 seconds, ~1h future); line 4 carries real time. Diagnosis: hand-written
journal lines. From here on, EVERY STATUS line goes through the helper ONLY:

`/code/llm-settings/shared/skills/worker-protocol/scripts/status-event <root>/STATUS.md <TAG> <message>`

— never hand-written, never rounded, never relabeled timezones. Append ONE
helper-stamped line confirming this rule adoption (that line also re-baselines
supervision). INTAKE-COMPLETE's content stands; only its stamp is void —
restate it under a helper stamp if you need it cited.

Wake: this file + pointer. The helper-stamped confirmation line IS the ack.
