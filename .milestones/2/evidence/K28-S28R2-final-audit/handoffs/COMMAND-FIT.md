# Pre-spend executable fit — S28-R2

Candidate ab25cd11b554bcd5ba64ca56a050c2eb21432d3c, accepted base
368b596fef0b6d393c2ac7afc631d236c55d86d1; FULL scope, all R1–R6 and
five reliances. Mandate f97e0c55bb0462cbf1698486341e142dfd5fe5e3b94bab7020b76032e9952e74.
Gate v10.2 full c00b88a29989b11d09696d7afa164f7d9f93b59aee661a1b88a120c7a4934b75,
normalized 12f392b6fe691230269a70bf9588fa4c25f71330639d0b6a464ceb8c532d67b0.

Exact argv are in probe-commands.json; frozen sources/runner hashes are in
evidence/probe-inputs.json. Invocation: node handoffs/run-audit.cjs <ID>,
from /code/kelgroups-audit-ab25cd1. No invocation has executed yet.

| Obligation | Exact IDs / cases | Charges |
|---|---|---|
| Frozen gate | gate executes unchanged ./gate.sh: leg3 just build; leg4 cabal test all -O0 --test-show-details=direct; M1 cabal build all --enable-tests -O0; M4 cabal build all -O0; M2/M3/M5/M6/M7/M8 cabal test all -O0 --test-show-details=direct; leg6 just ci. All via nix develop .#ci --quiet -c. | 11B |
| R1 | R1-compile/R1-run: actual open/founding, direct admission, proposal/approval role change; exact views/payload/state from concrete member relations; exact nonmember refusal and byte conservation. Seed counter 17, hook increment 23 distinguish new fixture from retained predecessor source. | 2P |
| R3 | R3-compile/R3-run: recording hook exact views/payload/error, success and refusal real reopen, exact state/count/row restoration. | 2P |
| R5 | R5-compile/R5-run: integrated direct/mutation/approval validators; hand-computed states after admission/propose/duplicate-refusal/approve-enact/app-add; foldIntegratedFrom over founding at every prefix; exact persisted events and real reopen. | 2P |
| MAJORITY | MAJ-compile/MAJ-run: admin franchise grows 3→5 mid-vote; two votes pending, three enact removal; exact final state. | 2P |
| P2'' | P2-compile, P2-conservation, P2-codec, P2-lock. One narrow compile includes candidate API and explicit skew seed body. Eight specified value pairs, fixed 20 repetitions each on seed then candidate, same instrument/domain. Seed moves state/count/decision pre-lock; SQL and TVar effects unchanged. External start gate precedes both calls; codec only increments observation TVar and emits identical JSON, never waits or delays. Semantic seed mismatch required, timeout/exception NOT a kill. Faulting accepted/refused exact tuples; real SQL abort and subsequent append. | 4P |
| F2 | P3/P4 compile candidate/shadow, P5 shadow absent-insertion witness RED, P6 candidate witness GREEN, P7 shipped nine-example direct-only group on shadow RED. One-expression mutation freshly generated from final Fold source. | 5P |
| TYP' | TYP-Event and TYP-Historical: separate compiler-negative files, first keeps proposal type correct and varies only event; second rejects DemoEvent at historical DemoState boundary. | 2P |
| HIST-FOLD | leg4 historical suites + complete base→FINAL historical-body diff; beyond suites UNJUDGED. | included |
| CESR | leg4 actual key suites; decoder-domain beyond suites UNJUDGED. | included |
| APPFOLD-SHAPE | leg3/4 compile and alias/caller review; semantics beyond tests UNJUDGED. | included |
| STORE-STM | P2'' + M6; finite schedules, no all-interleavings claim. | included |

Total 11/12 builds + 19/24 targeted. Remaining 1B/5P usable only after
mandatory floor, for relevant independent work; no outside-cap permission.
Every attempt is charged, including setup failure. Unmet mandatory floor
returns a named blocker; it is not silently waived. Concrete source exists
for every row: fit confirmed as PLAN, never execution/coverage success.

Prior TraceProbe/Row4Probe/runner are retained instrument source inputs,
reviewed and revised under this fresh root; no predecessor binary, process,
context, verdict or ledger reused. The new P2 instrument and its skew seed
are authored here. Candidate tracked files stay unchanged except the exact
gate-authorized leg5 transient edits and hash-verified restores. Gate fixed
/tmp staging names must be absent and exclusively reserved before launch.
