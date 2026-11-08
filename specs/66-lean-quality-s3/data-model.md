# Data model — #66 S3

Field names are binding at the level of meaning; encoding (JSON, Lean data)
is the commit owner's.

## E-1 Census (produced by M-1, not tracked)

| field | meaning / validation |
|---|---|
| module | elaborated module name |
| theorem | fully qualified constant name, `private` mangling resolved to a stable identity |
| span | file, start line/col, end line/col of the declaration |
| class | AUTHORED or EXCLUDED; EXCLUDED only by the single printed rule (R-2) |
| statementClosure | set of production definitions reachable from the theorem's type |
| refusal vocabularies | for each error type: its constructors |

Invariant: census identities are unique; every CI-built/elaborated module is
covered (a module that is built but absent from the census is an error).

## E-2 Mutant (tracked, M-2)

| field | meaning / validation |
|---|---|
| id | stable, unique |
| target | one production definition identity present in the census |
| patch | single-atom change confined to that definition's span |
| guard | refusal constructor whose guard it mutates, when it is a guard mutant |
| claimedKills | theorem identities; each must contain `target` in its statementClosure |
| decoy | true only for the neutral control mutant; claimedKills must then be empty |

## E-3 Ledger row (tracked, M-3)

| field | meaning / validation |
|---|---|
| theorem | census AUTHORED identity; one row each, no extras |
| class | KILLED / HELPER / OPEN |
| killedBy | non-empty iff KILLED; every listed mutant observed killing it |
| attempted | for OPEN: mutants tried that did not kill it |
| reason | required for OPEN; for HELPER, the computed fact (empty production closure) |

## E-4 Guard outcome (rendered)

Per refusal constructor: mutant ids, observed kills, `SURVIVED` when none.

## State invariants

- KILLED ⇒ observed error inside the theorem span under each `killedBy`
  mutant, and the mutant target ∈ statementClosure (R-7).
- HELPER ⇔ statementClosure contains no production definition (R-4).
- Baseline: zero errors in every AUTHORED span on the unmutated head (R-8).
- Production definition = a non-theorem constant defined in a library
  module that is not a test, fixture, corpus, oracle or checker module; the
  rule is stated once in M-1 and its classification printed.
