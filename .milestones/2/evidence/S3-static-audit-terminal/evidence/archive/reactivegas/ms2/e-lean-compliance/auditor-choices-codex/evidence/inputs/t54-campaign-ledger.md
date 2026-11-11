# Campaign ledger — #54 Slice A

One row per declared invariant. Carried across submissions.

| Row | Severity | State | Killing mutant / evidence |
|---|---|---|---|
| INV-54-PARTITION | BLOCKING | KILLED | Independent silent-deletion mutant verified its edit, then lost opened ID `q`; source `instruments/partition-silent-deletion-mutant.lean` `ea9882087e28a20a3e47532a4b5081419161ee9189df7d5ec3b188dbf350cf42`, RED log `7846a04f598486abce852bb2f3f9dfe073045db2e4d8c20e53fe94e1c07baf64`. The shipped `questions_partition` now preserves every prefix-observed ID through the production fold. |
| INV-54-DISJOINT | BLOCKING | KILLED | C2 omitted the opposite-tally erase; test and proof went red, `203c8a3e3cfbe8952afece5166405296e21ac33edab16cf19f64550e73e9c462` |
| INV-54-NOSTALE | BLOCKING | KILLED | C4 swept only ballot events; witnesses and carrier proof went red, `736207103c130c59f36d8dfe2b82a94b712526e0671ecba0bf7ff8e77a27d446` |
| INV-54-FRANCHISE | BLOCKING | KILLED | Independent unfranchised-recast mutant verified the post-standing-loss tally switch, then the complete-tally no-op oracle went RED; source `instruments/franchise-unfranchised-recast-mutant.lean` `08ccdb22771d9a6b8746bbb3e8d1441fc95c0b70217fdcf27c49532240c6db19`, log `a045991ca9d26e7f5c8223a54d87dcdbb686ff2143f1e70eff66e74174889b84`. The shipped theorem names the cast event and immediate prefix, and `unfranchised_cast_noop` is complete state identity. |
| INV-54-NOEXPIRY | ADVISORY | OPEN | The theorem is now over a production-fold prefix but `EventPreservesQuestion` categorically excludes every member event. A non-admin admission preserves the franchise, proposer, and non-degenerate target tally yet the theorem premise reduces to `False`; source `instruments/no-expiry-member-event-gap-v2.lean` `1f146f5b889d8990af499d1d57791a1dd083a738119b3aa6001fa6c8477859b7`, log `b71f1ac28b0b84bd4c11a95600c5e951109fb0de59c74f5c1c69cd419d51bf86`. |
| INV-54-POLICYFREE | BLOCKING | KILLED | hard-coded `legacyThreshold` mutation made the zero-policy witness red; source `9e738cfb0d4e7794e0c11b6a9bdab88e33c4469f59a406d1595a17317fc28cc5`, log `e0b3703a2462d0116945a809485fc18a72356f5d07247084262116331c6248a6` |

Builds: `builds_spent=9` `builds_budget=20` after submission-1 audit.
(3 at dispatch; focused Lean 4.25 +1; exact Lean 4.27 +1; frozen slice gate +2;
ticket gate +2. Failed harness-launch/readiness probes were not cited and were
uncharged.) Campaign remains `OPEN`: 3 KILLED, 3 OPEN; `stopped=none`.

## Carried forward to submission 2

Rows `KILLED` at submission 1 (DISJOINT, NOSTALE, POLICYFREE) are **terminal**
and are not reopened. Rows carried `OPEN` into submission 2:
`INV-54-PARTITION` (BLOCKING), `INV-54-FRANCHISE` (BLOCKING),
`INV-54-NOEXPIRY` (ADVISORY).

Builds at submission-2 dispatch: `builds_spent=11` `builds_budget=20`
(9 after audit 1, plus 2 ticket-owner frozen-gate runs on `757dac98` and
`c433ff76`). The commit owner's own repair builds are inside its seat and were
not separately charged here.

## Submission 2 final audit

Builds: `builds_spent=16` `builds_budget=20`.

- focused Vote build: +1, cold;
- frozen slice gate: +2, warm;
- full ticket gate: +2, warm;
- direct Lean 4.25.0 instrument typechecks: unmetered under the contract's
  typecheck-only rung.

Campaign remains `OPEN`: 5 KILLED, 1 OPEN; `stopped=none`. The round found a
separate blocking R-45 boundary violation, so tail-stop is unavailable. This
is submission 2/2; the ticket must be re-cut rather than repaired again.
