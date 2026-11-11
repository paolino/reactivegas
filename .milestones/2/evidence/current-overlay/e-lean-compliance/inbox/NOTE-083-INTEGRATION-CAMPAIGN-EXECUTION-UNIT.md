# New #92 campaign: usable CI execution unit and issue metadata

For the new NOTE080/#92 campaign, before first spend bind one frozen mandatory `nix develop --quiet -c just ci` invocation as ONE substantive execution including its declared recipe/dependency expansion. #90 preflight proved that recursively charging every internal CI stage makes one mandatory run exceed the shared ten-unit ceiling. This is a counting defect, not a reason to weaken CI.

All checks still run and all actual commands/stages/exits/cost remain in evidence. Each separate aggregate invocation counts again; separate ad hoc runs count separately. Do not wrap independent actor runs, retries or previously separate experiments in a new script to relabel them as one. Freeze execution units in the schedule. The ten-unit ceiling and submission/repair limits are unchanged; no reset/refund of completed S4, current S3 or simulator historical counters. If this new campaign has already spent, disclose its actual pre-amendment units and keep them separately labelled; do not silently reclassify history.

Also make #92 a milestone2 issue, assignee paolino, on project2, before its ticket metadata handback. The desk readback found milestone=null; this bug belongs to the M2 integration path. No comments or additional approval checkpoint.
