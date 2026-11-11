# S62-C A013 campaign ledger — auditor copy after submission 1

- Auditor: `commit-auditor-s62-c-a013-grok` (family=grok, harness=grok-build,
  model=grok-4.6)
- Candidate: `76f952dbc859115bc14838ff91d1a93ef107d7d5`
- Tree: `f0f795906e4e26aa68830622055f70be0e42527c`
- Source initial ledger SHA-256:
  `3b73cf0a403b72bbd8d68bf0d3e9ca1727f3f5b4883fc95819b7f2c0ea9af3c9`
- A011 final ledger SHA-256:
  `5423fa5664733442eb01e6303d1c3ee80a32e0c238d1987122daec1260462ade`
- Rows: 7 blocking; residual forbidden
- Terminal state: killed 7, open 0, blocked 0, residual 0
- Stopped: SET-POINT
- Builds: `34/40`; this audit `32`–`34`; ceiling raises `0/2`

| Row | Severity | State | Evidence |
| --- | --- | --- | --- |
| `G62-C-THEOREMS` | BLOCKING | `KILLED` carried | base blobs unchanged; `GATE-ROW G62-C-THEOREMS PASS` in `evidence/a013-gate.log` `716afe5dd56b90772a556aa1f2d299a6f9279b95ec85d84ffee4bcab0ded415d` |
| `G62-C-ECONOMY` | BLOCKING | `KILLED` carried | same; `GATE-ROW G62-C-ECONOMY PASS` |
| `G62-C-EXHAUSTIVE` | BLOCKING | `KILLED` carried | same; `FALSIFY-OK constructor-seeds count=6`; `GATE-ROW G62-C-EXHAUSTIVE PASS` |
| `G62-C-TRUST-CI` | BLOCKING | `KILLED` carried | same; `GATE-ROW G62-C-TRUST-CI PASS`; shipped `just ci` re-run inside A013 and ticket gates |
| `I57-01-BOUNDARY` | BLOCKING | `KILLED` carried | `Step.lean` blob `06b2d12eb3dc09a060f99f88297290ac776c13dc` identical to base; single `applyVoteEventChecked`; `checkI57Boundary` still present; `GATE-ROW G62-C-INHERITED57 PASS` |
| `G62-C-INHERITED57` | BLOCKING | `KILLED` carried | KelGroups blobs identical to base; includes DISJOINT; `GATE-ROW G62-C-INHERITED57 PASS` |
| `G62-C-TRACE` | BLOCKING | `KILLED` | production/value-level carried (Invariants blob `f14bbd7614fe29d1680c7f97c6f84a3df7e8eaa7`); wiring killed by throwaway `false &&` mutant `3e6349021426a31ebdf402ab145d9f202b1906c056a889ae5971a856ce5095b8` → tracked `just ci` exit 1 at `lean-corpus-gate` with 0 Lean elaboration errors (`evidence/mutant-ci.log` `a0318b2098112f32d4e3d51ea2ea3105ecb7797c26886f20f2fececb98eb0f85`); restored candidate GREEN via frozen A013 + ticket gates |

Stop condition: SET-POINT (every row terminal). Residual forbidden; none recorded.
