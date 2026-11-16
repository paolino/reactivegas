# Campaign ledger — #71 S71-B submission 1

Fresh candidate 8e4cbb8b95ac5a2063ea39cf2d2ac6a4c1d15163; mandate 90dae99.
All twelve rows BLOCKING. Ended SET-POINT: 12 KILLED, 0 OPEN, 0 BLOCKED,
0 RESIDUAL. KILLED means a representative documentary mutant was rejected;
it is not a content PASS and does not discharge additional survivors/findings.
No prior seat's kill is inherited.

| Row | Severity | State | Own mutant | Exact rejection | Log SHA256 |
|---|---|---|---|---|---|
| R71-01 | BLOCKING | KILLED | B01-USERS | exit 1: still list users | `22fc5d4f611a3770e93e8c1ceea8d2a476bee417acd588271eedf3446654c2d8` |
| R71-02 | BLOCKING | KILLED | B02-EVENTS | exit 1: still claim 15 events | `6b0935869556cfb35c1b3f7e10d70d43a7b6401f896038ed58d4cd605259b2cd` |
| R71-03 | BLOCKING | KILLED | B03-STEP | exit 1: stale 2-arg | `8f2ba48d58f3b3f7e54c4871d1208d1ab6c064896cc9c82cb8b76387f65497b2` |
| R71-04 | BLOCKING | KILLED | B04-TENSION | exit 1: honesty-tension | `37b60b8b906f216508e39aa501a977706fe88ff36bb29bb1b52221631d4d216c` |
| R71-05 | BLOCKING | KILLED | B05-WITNESS | exit 1: no law/witness heading | `423d046f7e45655970f60e9a9985766daf6adb2c4e39d9946ef1ee767725e12c` |
| R71-06 | BLOCKING | KILLED | B06-LINK | exit 1: unbound reachability | `bfd8ec4a76d6fdd3f1a3618dcb70cc5d109011c442f23d38584fef6da815ffe4` |
| R71-07 | BLOCKING | KILLED | B07-THETA | exit 1: theta-open sentence missing | `3f9d2f539333d07916f7795bf9db8a8eea3e9ecd02343f20dd124ea69b26be19` |
| R71-08 | BLOCKING | KILLED | B08-VOCI | exit 1: stale twenty-modules count | `93adf9dbe4af06fc947db623c6910fadffe8c8be97441909058fcf0b338b732b` |
| R71-09 | BLOCKING | KILLED | B09-DATES | exit 1: lack dated operator rulings | `4defc44415a0632d8eb4f1683f10af0b994c145339ce984a8a1b66a5790adc5f` |
| R71-10 | BLOCKING | KILLED | B10-PENDING | exit 1: pending anchor #81 | `75a8396f72055bdb3459b2753b3733781b5396c12f2dda4a7dd1e61fff53dccb` |
| R71-11 | BLOCKING | KILLED | B11-UNCITED | exit 1: CLAIM-RED | `ec34bea114cea13f2f03a06d851864dcffa1930d5a4a5f7632437bc717d46696` |
| R71-12 | BLOCKING | KILLED | B12-CLOSURE | exit 1: invent a group-closure theorem | `d136b3dff036c4958d64f8f75a07145d83226b96cdaee04f9bd50bb74c29c35c` |

Mutants and complete output: `evidence/doc-controls/<id>.md` and `.log`.
`results.json` binds document hash, log hash, exit and timing per execution.
All edits checked applied; candidate identity, tracked cleanliness and gate hash
checked before and after each run. Defective controls ran before CTRL-ORIGINAL.

Two additional R71-11 probes survive: F11-WRONG-DECL and F11-SOLE-MARKER.
Their explicit association defects remain blocking, not accepted residuals.
CTRL-ORIGINAL succeeds. Exact differences and hashes appear in the report.

## Resource receipt

- T1: one cold static whole gate with SKIP_CI=1 REPLAYS=0 (legs 0-11,
  C/SYNTH, 13/15). Targeted 1.
- T2: one full frozen gate including just ci and leg 14. Builds 1/2.
- T3: R1-R26 whole block, including R15b, executed inside T2. Targeted 1.
- T4: twelve own representative mutants, two association probes, one
  unmodified positive. Targeted 15.
- Supplemental compiled citation/witness reader: two attempts, both exit 1
  on auditor-only instrument issues. Targeted 2. Both logs retained, neither
  relabelled GREEN. The original T2 live-witness leg passed independently.
- Synthetic plus real .lake exclusion read/control: targeted 1.
- Total: 20/20 targeted; 1/2 builds. Zero ceiling raises.
- Two initial Python launch/setup failures occurred before any subject gate
  invocation (PATH missing Python; mutation precondition caught non-unique
  text). Zero mutants executed by those launches; both raw logs retained.
- T5 source/ruling/Git/compiler-index reads and T7 reporting/hashing are free
  under the brief. No additional compiler or gate execution followed the cap.

Compiler indexes from T2 supply the final 117/117 definition associations and
the eleven ValidationError constructor identities. This is artifact reading,
not a retry of the failed #check driver or a new build.

This seat leaves the separately reserved submission-2 allocation of two builds
and twenty targeted commands untouched. Its unused one build is not transferred
or presented as an automatic ceiling increase. One verdict, then COMPLETE.
