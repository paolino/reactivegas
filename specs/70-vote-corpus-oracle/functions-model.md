# Functions — #70 slice O

| ID | Signature | Constraint |
|---|---|---|
| F-VALIDATE | `validateContext(contextBytes, corpusBytes) -> refusal \| context` | pre-replay only; reads no provenance field |
| F-TABLE | `tableThreshold(table) -> (n -> threshold)` | throws a domain abort carrying `n` and the domain when `n` is outside it |
| F-REPLAY | `replayCorpus(corpus, theta) -> outcome` | one call of the core's integrated root per step |
