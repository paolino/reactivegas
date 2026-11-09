# Functions — #70 G1

| ID | Signature | Constraint |
|---|---|---|
| F-DISCOVER | `discoverVocabularies(repo, pin) -> [{ vocabulary, constructors }]` | the one discovery; claim gate and generator call it |
| F-GENERATE | `generateTrace(seed, config, surface) -> { steps, report? }` | pure given seed and config; drives the production core only |
