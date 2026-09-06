# Executable command fit — A-01, before START/spend

Subject 368b596fef0b6d393c2ac7afc631d236c55d86d1 →
3af3d065b7d0c54f03d89b8c05d8b8acd4a53db4. Brief A-01 SHA256:
6f4fcd08370b81a57e4856cda6b6775c7b133608e05efc6ad1cdc0dbe6806c07.
Gate v9 bytes unchanged (full dcbc8c2b8eefa111b5b71873be8d87fa95de2369642e6224417f9544e5a8e815).

Exact probe argv arrays are frozen in probe-commands.json; complete programs
are TraceProbe.hs, StoreProbe.hs, Row4Probe.hs and the single-expression
row4-shadow/KelGroups/Fold.hs. Hashes: evidence/probe-inputs.json. No program
has compiled or run yet; fit is an executable plan, not a coverage verdict.

All commands run from the detached audit checkout. Wrapper invocations:
`node <handoffs>/run-audit.cjs gate` runs unchanged `./gate.sh` with own
G28_EVIDENCE_DIR/TMPDIR, exclusive fixed-path reservations and attempt/exit
journaling; `node <handoffs>/run-audit.cjs <id>` executes that ID's exact argv,
records real exit/output hash/free space, and refuses duplicate IDs.

| Obligation | Commands / concrete cases | Charge |
|---|---|---|
| Gate, registration, full local CI, M1–M7 | gate: legs 3,4,M1,M2,M3,M4,M5,M6,M7,6 | 10B |
| R1 A-01 | R1-compile, R1-run: founding aggregate/store open; direct admit; pending roles proposal; approval/enactment; hand-specified views and recorded hook arguments; nonmember error/state/row-byte equality | 2P |
| R3 | R3-compile, R3-run: recording hook returns distinguishable counter/log payload carrying exact pre/post views; success reopened; refusing hook returns its complete arguments as the error; exact seed/count/rows after refusal and reopen | 2P |
| R5 A-01 | R5-compile, R5-run: hand-built aggregate at every prefix of admission, proposal, duplicate refusal, approval/enactment and app add; integrated validator decisions checked independently; exact accepted persisted events; foldIntegratedFrom vs hand states; real reopen | 2P |
| MAJORITY | MAJ-compile, MAJ-run: pending vote at three admins; direct admission raises franchise to five; two votes remain pending; third vote enacts and exact membership/log output checked | 2P |
| R2/R6/STORE-STM, F1 | P1 compile + P2 run: eight specified concurrent pairs, real serialization rendezvous, both successes, exact ordered decoded events/seq_no, full replay/live equality, count conservation; seeded negative, sequential, SQL abort, domain refusal and subsequent lock-release controls | 2P |
| R4, F2 | P3 candidate compile, P4 shadow compile, P5 mutant witness, P6 original witness, P7 shadow shipped nine-example group | 5P |
| HIST-FOLD | leg4 historical suites plus full accepted-base historical diff review; beyond-suites UNJUDGED | inside gate / 0 |
| CESR | leg4 key suites; decoder-domain beyond them UNJUDGED | inside gate |
| APPFOLD-SHAPE | leg3/4 compilation and unchanged alias/caller review; semantics beyond suites UNJUDGED | inside gate / 0 |
| Fence/provenance/automation | full base→candidate diff, modes, ignored files, actual workflow/justfile wiring and historical boundary review | 0 |

Total planned 10/12 builds and 15/24 targeted (within the brief's ≤16 probe
suballocation). P2 optional second stress run remains one unspent probe slot;
no speculative retry or unrelated build is allocated. The two remaining
builds are only for named infrastructure-flake disambiguation. Every failed
or setup invocation spends its allocated attempt. Controls and expected
negative outcomes are inside the named programs/runs; syntax/setup failures
are not semantic evidence. Limits remain reportable, not residual waivers.

The pre-A01 R5 question/draft is superseded by the explicit parent amendment,
received before START or terminal status. It is retained solely as history.
