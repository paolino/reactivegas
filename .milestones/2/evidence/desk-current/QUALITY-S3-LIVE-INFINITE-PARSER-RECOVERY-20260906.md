# S3 static preparation: live parser loop recovery

Desk observation at 2026-09-06 ~04:56 UTC; recover through quality owner %503 only.

The new S3 Muse is not merely thinking or ignoring its queued correction. Direct child PID 3226574, PPID 3215777, PGID 3226574 is a node -e static source parser, running over nine minutes at 99.1% CPU. Its actual argv contains `let j=i+1; while(j<L.length && !...test(L[j]) || ... && false){}`: the body is empty and neither j nor L changes. A true initial condition never terminates. Pane showed the same command elapsed 561.8 seconds and the correction queued behind it. This is an instrument hang, not a Lean build or a missing START inference.

Recheck that exact PID/parent/argv and its descendants before acting. You are authorized to interrupt/terminate ONLY that hung source-parser command at its own command boundary, preserving the Muse session, runtime, source worktree and evidence. Do not kill the worker/pane or touch the S4 seat. Retain argv, actual duration and any output, and have the owner journal the instrument failure and valid fresh UTC correction/ACK already owed. No product compile or capped audit count inferred from a Node text parser.

Then continue the full static mandate. Any replacement mechanical parser needs bounded termination and explicit treatment of unsupported syntax; regex/text search remains a review aid, not a complete Lean declaration or call-graph oracle. Do not weaken the required semantic assessment to a token-hit inventory. No new project-execution authority, reset, scope change or extra submission.

Separately: S4 restored-model observation is credited; its own full instrument/command revalidation remains the existing prerequisite. Do not interrupt its current revalidation to obtain another redundant acknowledgment.
