# #48 S48-I audit campaign ledger — submission 2

- Candidate: `a408e0920491a67ef9ccf8625a089191289ebff3`
- Parent ledger: submission-1 `campaign-ledger.md` (`75a7a3d2207924acc44437c981e7d0934d6d80400f4f97f3b78a6823279707a1`)
- Builds: `2 / 3` (this audit: one cold build, then warm reuse)
- Ceiling raises: `0 / 2`
- Campaign: `OPEN`; stopped=`none`; killed=`6`; open=`4`; residual=`0`; blocked=`0`
- Submission cap: `2 / 2`; no further repair/audit in this ticket; carry OPEN rows into a re-cut
- Instrument manifest: `handoffs/evidence/instrument-sha256-manifest.log` (`79bc3494e6aac6f31101c8ca6037c997fab94f3143f40755c117f794d96c8d94`)

| Invariant | Severity | State | Mutant / evidence |
|---|---|---|---|
| INV-48-I-SURFACE | BLOCKING | OPEN | Comment/string/private/non-theorem/removal/duplicate-declaration/phantom killed; complete required-name/event swap survived (`fba84ff0…`). |
| INV-48-I-GUARDS | BLOCKING | OPEN | `True`, dropped conjunct, and wrong successor killed; complete required-name/event swap survived with all tightness reports green (`fba84ff0…`). |
| INV-48-I-CANFAIL | BLOCKING | KILLED | Removed live inversion and checker unwiring returned nonzero after a RED preflight; normal and final baselines passed (`273c8b65…`). |
| INV-48-I-AXIOMS | BLOCKING | KILLED | Disguised custom opaque dependency and `admit` returned nonzero; candidate six reports are exactly `[propext]` (`c30bd8a5…`). |
| INV-48-I-REGRESSION | BLOCKING | OPEN | Active indented theorem survived ordinary gate; environment theorem constants increased `517 → 518` while checker printed 163/163 (`d57c155c…`). |
| INV-48-I-FENCE | BLOCKING | KILLED | Fresh forbidden `Step.lean` path mutant rejected (`e408a627…`). |
| INV-48-I-EVENT-SYNTAX | BLOCKING | KILLED | Elaborated environment supplies constructors; phantom event constant in inversion rejected (`5a0d5bda…`). |
| INV-48-I-INV-HYP-SYNTAX | BLOCKING | OPEN | Wrong last hypothesis killed; duplicate identical successful-step hypothesis survived (`3ecb6b54…`). |
| INV-48-I-STEP-ITE | BLOCKING | KILLED | Fresh no-`step`-unfold mutant failed at `split` (`4ebb7324…`). |
| INV-48-I-STEPEVENT-DELEGATE | BLOCKING | KILLED | Fresh no-`stepEvent`-unfold mutant failed elaboration (`196738e4…`). |

No claim of exhaustiveness or zero survivors is made.
