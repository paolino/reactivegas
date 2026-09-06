# #48 S48-I audit campaign ledger — submission 1

- Candidate: `4898e55ec6ee1ff516b835f621a0b51373684f4c`
- Parent ledger: `/tmp/reactivegas/ms2/t48-inversions/campaign-ledger.md` (`9757d58c192624ffcaae979db55dbd9c3644a1c6d4974f24b1b36530d10dfa75`)
- Builds: `1 / 3` (this audit: one cold build, then warm reuse)
- Ceiling raises: `0 / 2`
- Campaign: `OPEN`; stopped=`none`; killed=`4`; open=`6`; residual=`0`; blocked=`0`
- Instrument manifest: `handoffs/evidence/instrument-sha256-manifest.log` (`0dcf6370756fe5d8476540bb92d86dbb61adca3a708d46a5e738646d4d80ae8a`)

| Invariant | Severity | State | Mutant / evidence |
|---|---|---|---|
| INV-48-I-SURFACE | BLOCKING | OPEN | Simple theorem removal killed; comment-plus-custom-axiom survivor reported 14/14 (`afdacbc8…`). |
| INV-48-I-GUARDS | BLOCKING | OPEN | False conclusion killed, but weakened guard-to-`True` survived coverage and elaboration (`66334616…`). |
| INV-48-I-CANFAIL | BLOCKING | OPEN | Removal, withheld coverage, and unwiring killed; composed missing-proof bypass survived (`afdacbc8…`). |
| INV-48-I-AXIOMS | BLOCKING | OPEN | `sorry` killed; custom axiom survived six-report/`sorryAx=0` acceptance (`afdacbc8…`). |
| INV-48-I-REGRESSION | BLOCKING | OPEN | Simple declaration removal killed; raw-text count stayed 163 for commented theorem plus axiom (`afdacbc8…`). |
| INV-48-I-FENCE | BLOCKING | KILLED | Added `lean/Reactivegas/Step.lean` path rejected (`2da5c149…`). |
| INV-48-I-EVENT-SYNTAX | BLOCKING | KILLED | Unparsed `|  donate` marker rejected (`66334616…`). |
| INV-48-I-INV-HYP-SYNTAX | BLOCKING | OPEN | Alternative spelling and wrong association killed; raw commented hypothesis survived (`afdacbc8…`). |
| INV-48-I-STEP-ITE | BLOCKING | KILLED | Removing `step` unfolding made elaboration fail (`2da5c149…`). |
| INV-48-I-STEPEVENT-DELEGATE | BLOCKING | KILLED | Removing `stepEvent` unfolding made elaboration fail (`2da5c149…`). |

No claim of exhaustiveness or zero survivors is made.
