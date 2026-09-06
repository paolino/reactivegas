# Q-001 — R5-C1 successful lifecycle names an empty-start fold

Disposition requested from ticket owner t28-app-api. No author contact.

The replacement brief, lines 48–49, makes propose→approve→enact and
`stepwise validate-then-fold == foldIntegrated throughout + replay equality`
a mandatory probe. Lines 67–69 require a pre-spend stop for a concrete
command-fit gap. The current packet does not distinguish two incompatible
starting states in this comparison.

At candidate 3af3d065b7d0c54f03d89b8c05d8b8acd4a53db4,
`foldIntegrated` accepts an application payload and starts with
`emptyState initial` (Fold.hs:458–470). It cannot receive founding members.
Direct admission, proposal and approval require an existing admin, and app
events require an existing member. The initial empty member map therefore
cannot acquire its first member through these events. An accepted
proposal→approval→enactment trace requires a different, founding aggregate.

This is already an explicit design constraint in r5 §D5: founding is supplied
to `openIntegratedKEL`; there is no bootstrap arm, and `foldIntegrated` keeps
its application-payload-only argument. `foldIntegratedFrom` (Fold.hs:477–488)
is the separate function accepting that aggregate. It is also the function
used by the existing founding-prefix and durable-replay tests.

Concrete gap: no invocation of the named `foldIntegrated` entrypoint can
both exercise that accepted lifecycle and compare equal to its founding
aggregate at every prefix. Starting both sides empty would only compare
refusals. Replacing the named function would change the mandatory mapping;
no candidate change is proposed or authorized by this question.

Recommended correction: explicitly bind the successful lifecycle comparison
to `foldIntegratedFrom integration founding prefix`, with independently
computed stepwise expectations and full persisted-row replay equality. Retain
a separate empty-start `foldIntegrated` refusal-prefix check in the same
compiled probe. State that founding is an initial aggregate, not an admission
event, for the R1 trace as well. This is a proposed command-contract
clarification, not a product repair or a request for a budget increase.

Alternative: retain the exact `foldIntegrated` entrypoint and acknowledge
that it cannot supply the successful lifecycle coverage; R5-C1 stays blocked.
An all-refusal trace must not be accepted as that coverage.

This run returns one terminal CONTRACT-BLOCKED report with 0/12 builds and
0/24 targeted probes. No compilation or semantic falsification was attempted.
Source inspection establishes this contract-fit objection, not an audited
product defect or an independently executed behavioral result.

## Resolution — 2026-09-06T00:39:11.660Z

RESOLVED by A-01/NOTE-030, received and acknowledged before START. The terminal-return wording above was a proposal in an unissued draft, not a terminal event. No CONTRACT-BLOCKED verdict was issued by this worker. The amended founding/replay command contract was fully executed; the sole terminal report is handoffs/AUDIT-REPORT.md (AUDIT-FINDINGS, F3).
