# audit-s3 FULL replacement campaign — terminal assessment

Candidate 9c8756a252c46bc5745badafcc9126abd3e5d9a7; accepted S1 base 4a6cd87.
Submission 2/2. All inherited row verdicts were reopened, then independently assessed.
Original denominator: five rows. NOTE-001 added D1, D2, ratified CI-74-A (D3),
and an explicit D4 residual disposition: nine assessment rows in total.
Builds: audit-s1 1 + invalid audit-s2 1 + audit-s3 1 = 3/3; ceiling raises 0/2.
Campaign CLOSED by SET-POINT: four KILLED, two RESIDUAL, three BLOCKED.
BLOCKED here means known unmet requirement in the frozen candidate, not missing
experiment or need for another cold build. Audit verdict: FINDINGS, blocking=3.

| Row / disposition | Severity | Verdict | State | Independent evidence |
|---|---|---|---|---|
| G74-CALLS-EXISTING | BLOCKING | PASS | KILLED | M01 empty, M02 same-size value (40→41; unchanged 14494 bytes), M06 truncated, M07 last-element changes, M08 type changes; compiled checker detects intended defects on both arrays; normal 5/32/7 passes. |
| G74-ENVELOPE-CLOSED | BLOCKING | PASS | KILLED | M03/M04/M05 added top, added nested and deleted top keys fail the verbatim shipped jq programs on both wrappers. Normal shape passes. |
| G74-VERIFY-FAILS-CLOSED (declared byte/manifest mechanism) | BLOCKING | PASS | KILLED | B01/B02 byte 1→0 and B03/B04 manifest 1→0 with SHA-identical restore; full gate supplies initial 0. B05 stub exit-0 killed by exact frozen v4. CI wiring and clean-shell availability assessed separately below. |
| G74-RECORD-HONEST | ADVISORY | residual | RESIDUAL | docs identical under three bases; counts/hole/provisional accurate; coverage handoff retains stale UNPROVED current-tense claims. RA74-RECORD, ticket owner T7403→#71. |
| G74-ADDITIVE-ONLY | BLOCKING | PASS | KILLED | B06 forbidden docs path killed before build. Provenance validates 259 tracked files, six implementation paths, six unchanged planning specs, model byte identity to ordered S1. |
| D1 / R74-03 CI wiring | BLOCKING | FAIL | BLOCKED | B07 corrupted corpus rejected by real verifier, B08 exact CI Lean command passes on same bytes and leaves mutation intact. |
| D2 / clean dev-shell deliverable | BLOCKING | FAIL | BLOCKED | clean-shell.log JQ-ABSENT, real verifier reaches live-bound then exits 127 on missing jq. |
| D3 / CI-74-A whole-wrapper binding | BLOCKING | FAIL | BLOCKED | S01 view-key, initial-members, economic-auth, integrated-auth: four distinct changed values all survive compiled check and shipped shape check. Independent live-context comparison detects view/initial defects and passes current values. |
| D4 / public CLI arity | ADVISORY per explicit residual option | residual | RESIDUAL | IO08 check ONE_PATH exits 0 and writes both files. RA74-ARITY, ticket owner; no shipped caller reaches this malformed form. |

Finite byte fault set: 18 fixture mutants, 18 applied, 18 executed; 14 killed by
at least one owning shipped check, four context survivors. These are 31 checker
invocations including clean/separability controls, plus eight compiled IO probes
(39 total); boundary harness contributes nine executions, including repeated
byte controls and one CI survivor. Do not add these overlapping totals into a
fictitious unique-mutant denominator. Setup attempt 1 (/dev/urandom failure)
was excluded and preserved. No syntax/import failure counts as a kill.

Evidence: byte-campaign.log 96b21029b030401b21fad3cf55e5fd67487607f56b4f71d6b72da47e30a5d6ce;
boundary-campaign-v2.log eebb0b328cd7a3402126cfff795aa0fc97b78b60fc79e1e9ec5c33f5152b2d51;
clean-shell.log 4d16d49a3c98459b73ec4dfbf09ee4657536cbf98bf84a04ab1e0893093270c6;
provenance.log 558a83047fc59d6641ffa619c81e8810c3073301de78053e56c9de87953f5dc4.
