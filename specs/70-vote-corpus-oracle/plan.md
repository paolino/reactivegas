# Plan — #70 slice O

One bisect-safe slice on `sim/70-vote-oracle`, one draft PR with preview. A new
simulator gate reads `lean/corpus/vote.json` and `lean/corpus/vote.context.json`,
validates the sidecar, replays each step through the core's integrated root with
a threshold built from the sidecar table (domain checked at the query), and
compares each step with the corpus. The core gains a threshold parameter on the
integrated route; the page keeps its named threshold. The R3.1 §7 controls are
in-memory mutations inside the gate. CI gate: `just simulator` (the gate glob
picks the new file up).
