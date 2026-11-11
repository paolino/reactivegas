# Data model — #66 S5

No new data. The mutant ledger rows keep the S3 classes (KILLED, HELPER,
OPEN with a reason, refusal SURVIVED). A triaged `unconstrained` row that keeps
its statement carries its reason in the ledger's existing reason field.

Finite-history slice: the integrated history is the existing vote-corpus shape
(initial `GroupState State`, steps of `VoteTraceStep` without reliance on
`voteError`); the fixed-view history is an initial `State` and steps of
`TraceStep`. No new stored format and no change to any corpus file.
