# Commit Audit

- Submission: `2` (FINAL)
- Base: `97f9684c0db344dd449e7a0a405ebc06c2e5cb74`
- Rejected candidate: `757dac98aecce705e44eda6c9283a5da01b02827`
- Candidate: `c433ff769fc35329050411054324c19b5b100fdb`
- Mandate: `spec.md` `bbdaf1a344be2695c4808d38ad24af627a5fa5bda55781421e0c1d5d881e5bc6`; all six supplied artifact hashes match
- Scope: FULL `97f9684..c433ff76`, explicitly authorized by NOTE-009 `928ed415808fd6a0f61c7c001edb6be18fa859b7ddce599208fd1cf0791dfb67`; repair delta `757dac98..c433ff76` plus findings 1-3 and `INV-54-NOEXPIRY`
- Verdict: FINDINGS
- Audit loop: submission `2/2`; next submission `FORBIDDEN`; blocking result requires ticket re-cut
- Ceiling raises: `0/2`
- Campaign: OPEN — `stopped=none`; ledger `handoffs/campaign-ledger.md` sha256 `9667b9f048dbb02fc2a9aa09c40139d3674b340005efd113c95c0c267df33d98`
- Builds: `16/20` this ticket; this audit `5` charged builds (focused +1, two gates +2 each), first cold then warm; direct Lean typechecks unmetered
- Toolchain in every execution receipt: `Lean 4.25.0`; Q-002 remains answered/closed and was not re-litigated
- Provenance/fence: clean detached exact candidate, five-commit ancestry from base, no remote branch contains the candidate; full slice changes only `lean/KelGroups.lean` plus `lean/KelGroups/Vote/**`; repair changes exactly `Vote/{Fold,Invariants,Tests}.lean`; ordinary `100644` files, no links; seven Slice-1 modules blob-identical to `ccdda83`; gate hashes unchanged

## Invariant matrix

| Invariant | Severity | Verdict | Row state | Proof / evidence |
|---|---|---|---|---|
| INV-54-PARTITION | BLOCKING | PASS | KILLED | `questions_partition` now preserves every ID present at any production-fold prefix into exactly one final container and retains non-open closure verdicts. Independent silent-deletion mutant source `ea9882087e28a20a3e47532a4b5081419161ee9189df7d5ec3b188dbf350cf42`, RED log `7846a04f598486abce852bb2f3f9dfe073045db2e4d8c20e53fe94e1c07baf64`. |
| INV-54-DISJOINT | BLOCKING | PASS | KILLED | Carried terminal from submission 1; full candidate gate reprinted its clean axiom set. |
| INV-54-NOSTALE | BLOCKING | PASS | KILLED | Carried terminal from submission 1; full candidate gate reprinted its clean axiom set. |
| INV-54-FRANCHISE | BLOCKING | PASS | KILLED | `franchise_of_tallies` identifies `(k, cast qid ballot)` and its immediately preceding production-fold state; `unfranchised_cast_noop` is complete state identity. Independent recast mutant source `08ccdb22771d9a6b8746bbb3e8d1441fc95c0b70217fdcf27c49532240c6db19`, RED log `a045991ca9d26e7f5c8223a54d87dcdbb686ff2143f1e70eff66e74174889b84`. |
| INV-54-NOEXPIRY | ADVISORY | FAIL | OPEN | `no_expiry` is production-prefix based, but `EventPreservesQuestion` excludes every member event. A non-admin admission preserves franchise, proposer, and target ballots while its theorem premise is false: source `1f146f5b889d8990af499d1d57791a1dd083a738119b3aa6001fa6c8477859b7`, log `b71f1ac28b0b84bd4c11a95600c5e951109fb0de59c74f5c1c69cd419d51bf86`. |
| INV-54-POLICYFREE | BLOCKING | PASS | KILLED | Carried terminal from submission 1; `verdictOf_threshold_congr` reprinted with only `propext`. |

## Residuals

None. `INV-54-NOEXPIRY` remains OPEN rather than being silently converted to a residual; this round has a blocking finding, so tail-stop is unavailable and no filed follow-up ID was supplied.

## Candidate invariants

None.

## Onward discoveries — outside this ticket

None. The R-45 failure is inside T5423 / Slice A, not an onward discovery.

## Blocking findings

1. [R-45 validation/franchise boundary] `lean/KelGroups/Vote/Validate.lean:52-54`, reached through `lean/KelGroups/Vote/Fold.lean:114-120` — validation accepts `admitMember`, `removeMember`, and `setRoles` from every signer. In a production-fold state with three responsabili, question `q` open with one assent, signer `stranger` is not a responsabile; nevertheless `("stranger", removeMember "b")` validates, lowers the franchise threshold from 2 to 1, and closes `q` positive. This violates R-45's Slice-A rule that no non-responsabile path influences a verdict. Property class: every signer-authorized franchise-changing event must validate against the current state before its effect, and every unauthorized member/franchise event must preserve the complete production vote state; validation coupling is insufficient when the validator admits the event. This does **not** claim Slice-B R-66/R-67 admission-shape requirements. Preflight source `89d49bfaa14ac868101d9cc49d1347fd3db37bc681e7c5d7005a83785f5b3150`, seed RED `b222b723a170b6ff5f88ea61287bf11a2de2c3ce166bfb047c31e9a3b25703dd`; candidate oracle source `d91fe846783a38dda02f39bfd419583c9c93b136449b947fc309046d3ca4c0fc`, RED `04dac10a8e05ad2c89c57f52b62c505da1a8107745d27843bb2db26b1b5a59a7`; observed-impact source `16316e4de3ee4810ec7ab12bb1ddcb0c8917a4f63148b65dbc4536e38641927e`, GREEN `9f185f6de80fd871b7e5031a712b9ad9ab0d6fee8fdb1d31e27377bdd15f7e86`.

## Verification receipts

| Command | Exit | Duration | Evidence |
|---|---:|---:|---|
| Lean 4.25.0 focused `lake build KelGroups.Vote.Invariants KelGroups.Vote.Tests` | 0 | 6.038s | `evidence/focused-vote-build.log` `c62514c2b680afa7027271af59a8cb5905bfcc760abeb861a059a0f93e31e12b` |
| Lean 4.25.0 `./gate-slice-a.sh` | 0 | 83.941s | `evidence/frozen-slice-gate.log` `ff6c3e0618e89f54365949844ba3a345cc50ccf22ddaa091920bf82424d8f453` |
| Lean 4.25.0 `./gate.sh` | 0 | 25.113s | `evidence/ticket-gate.log` `aed9073bf046ac55eed17e0a4adb530df9c16513f68c97c950533a77b0e452cf` |
| Lean 4.25.0 partition mutant typecheck | harness 0 / Lean 1 expected | 2.383s | `evidence/partition-mutant-red.log` `7846a04f598486abce852bb2f3f9dfe073045db2e4d8c20e53fe94e1c07baf64` |
| Lean 4.25.0 franchise mutant typecheck | harness 0 / Lean 1 expected | 2.374s | `evidence/franchise-mutant-red.log` `a045991ca9d26e7f5c8223a54d87dcdbb686ff2143f1e70eff66e74174889b84` |
| Lean 4.25.0 R-45 seed / candidate / impact typechecks | 0/1 expected; 0/1 expected; 0 | 2.328s; 2.343s; 2.334s | `b222b723a170b6ff5f88ea61287bf11a2de2c3ce166bfb047c31e9a3b25703dd`; `04dac10a8e05ad2c89c57f52b62c505da1a8107745d27843bb2db26b1b5a59a7`; `9f185f6de80fd871b7e5031a712b9ad9ab0d6fee8fdb1d31e27377bdd15f7e86` |
| Lean 4.25.0 no-expiry coverage witness | 0 | 2.370s | `evidence/no-expiry-gap-green-v2.log` `b71f1ac28b0b84bd4c11a95600c5e951109fb0de59c74f5c1c69cd419d51bf86` |

All contractual theorem names printed axiom sets limited to `propext`, `Classical.choice`, and `Quot.sound`; no `sorryAx`, `Lean.ofReduceBool`, `sorry`, `admit`, custom axiom, or `native_decide` was present. The frozen and full gates both reached the Vote tests and full local CI.

## Advisories

- INV-54-NOEXPIRY remains narrower than R-54: replace the event-constructor whitelist with a premise that actually states preservation of the target ballots, franchise, and proposer standing, including member events that satisfy those facts. The retained green witness uses three distinct responsabili, a non-empty assent tally, and a distinct non-admin admission, so the comparison is not a shared-empty/default pass.
