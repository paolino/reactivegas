# #48 inversion coverage campaign ledger

- Builds: 2 / 3 (submission-2 fresh audit reserved)
- Ceiling raises: 0 / 2
- Submission: 2 / 2 under fresh audit; candidate `a408e0920491a67ef9ccf8625a089191289ebff3`
- Audit report: `/tmp/reactivegas/ms2/t48-inversions/.archived/commit-auditor-s48-i-s1/handoffs/audit-report.md` (`e26e75bf539a51a4c68dda63d821aad2e25f2c32a42d241fb9a085954e0c4b94`)
- Frozen instrument manifest: `/tmp/reactivegas/ms2/t48-inversions/.archived/commit-auditor-s48-i-s1/handoffs/evidence/instrument-sha256-manifest.log` (`0dcf6370756fe5d8476540bb92d86dbb61adca3a708d46a5e738646d4d80ae8a`)

| Invariant | Severity | State | Evidence |
|---|---|---|---|
| INV-48-I-SURFACE | BLOCKING | OPEN | Commented theorem plus custom axiom survived (`afdacbc8…`); active-declaration binding required. |
| INV-48-I-GUARDS | BLOCKING | OPEN | Weakened guard to `True` elaborated and survived (`66334616…`); exact live guard/successor binding required. |
| INV-48-I-CANFAIL | BLOCKING | OPEN | Direct removal/unwiring killed; composed missing-proof bypass survived (`afdacbc8…`). |
| INV-48-I-AXIOMS | BLOCKING | OPEN | `sorry` killed; custom axiom survived six-report acceptance (`afdacbc8…`). |
| INV-48-I-REGRESSION | BLOCKING | OPEN | Direct removal killed; comment-backed lexical count survived (`afdacbc8…`). |
| INV-48-I-FENCE | BLOCKING | KILLED | Forbidden `lean/Reactivegas/Step.lean` edit rejected (`2da5c149…`). |
| INV-48-I-EVENT-SYNTAX | BLOCKING | KILLED | Unparsed constructor marker rejected (`66334616…`). |
| INV-48-I-INV-HYP-SYNTAX | BLOCKING | OPEN | Alternate spelling/wrong association killed; raw commented hypothesis survived (`afdacbc8…`). |
| INV-48-I-STEP-ITE | BLOCKING | KILLED | Removing live `step` unfolding made elaboration fail (`2da5c149…`). |
| INV-48-I-STEPEVENT-DELEGATE | BLOCKING | KILLED | Removing live `stepEvent` unfolding made elaboration fail (`2da5c149…`). |
