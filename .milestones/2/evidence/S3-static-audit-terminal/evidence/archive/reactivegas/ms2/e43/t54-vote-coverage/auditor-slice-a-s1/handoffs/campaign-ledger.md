# Campaign ledger — #54 Slice A

One row per declared invariant. Carried across submissions.

| Row | Severity | State | Killing mutant / evidence |
|---|---|---|---|
| INV-54-PARTITION | BLOCKING | OPEN | C3 `closed := gs.closed` went red, but `questions_partition` does not quantify opened history; owner log `42beba4a09c86eb3d94188a858873b260d15ffca310040c960c3cbf464a55b8c` |
| INV-54-DISJOINT | BLOCKING | KILLED | C2 omitted the opposite-tally erase; test and proof went red, `203c8a3e3cfbe8952afece5166405296e21ac33edab16cf19f64550e73e9c462` |
| INV-54-NOSTALE | BLOCKING | KILLED | C4 swept only ballot events; witnesses and carrier proof went red, `736207103c130c59f36d8dfe2b82a94b712526e0671ecba0bf7ff8e77a27d446` |
| INV-54-FRANCHISE | BLOCKING | OPEN | `franchise_of_tallies` exposes no cast event at its responsible prefix, and `unfranchised_cast_noop` observes only global key membership |
| INV-54-NOEXPIRY | ADVISORY | OPEN | C5 field mutation went red, but `no_expiry` covers only a cast on one distinct question and is not an event-list theorem; `1c9a019b840e1a2e5ccb8256d428860c8b66922160cb1d01952405b7c4f11e40` |
| INV-54-POLICYFREE | BLOCKING | KILLED | hard-coded `legacyThreshold` mutation made the zero-policy witness red; source `9e738cfb0d4e7794e0c11b6a9bdab88e33c4469f59a406d1595a17317fc28cc5`, log `e0b3703a2462d0116945a809485fc18a72356f5d07247084262116331c6248a6` |

Builds: `builds_spent=9` `builds_budget=20` after submission-1 audit.
(3 at dispatch; focused Lean 4.25 +1; exact Lean 4.27 +1; frozen slice gate +2;
ticket gate +2. Failed harness-launch/readiness probes were not cited and were
uncharged.) Campaign remains `OPEN`: 3 KILLED, 3 OPEN; `stopped=none`.
