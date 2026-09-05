# Receipts — invocation binding for S13..S20 + focused controls (submission 1)

Candidate `0c3c1e93af97603bb69d5bd43e532cec45934bc4`, worktree
`/code/reactivegas-sim-fable` (clean at every run). Commands as executed
from that directory unless noted. `exit=` is the measured process exit
(`echo` on the same line, no pipe). Charge: S13/S14/S15/S18/S19/S20
substantive; S16/S17/sentence/flip/mutant-probe targeted.

| row | exact invocation (incl. env) | exit | sha256 of log | log file |
|---|---|---|---|---|
| S13 | `node economics-simulator-ui-gate.mjs` | 0 | `05fffc93…ce0ebc` | `S13-full-green.log` |
| S14 | `node economics-simulator-ui-gate.mjs --omit K-2` (RG_OMIT_NOOP unset) | 1 | `73f15d8b…84866` | `S14-omit-K2-red.log` |
| S15 | `RG_OMIT_NOOP=1 node economics-simulator-ui-gate.mjs --omit K-2` | 0 | `05fffc93…ce0ebc` | `S15-noop-omit-green.log` |
| S16a | `node economics-simulator-ui-gate.mjs --derive-only` | 0 | `de18048c…72a0b` | `S16-derive-prod.log` |
| S16b | `node economics-simulator-ui-gate.mjs --derive-only /tmp/S16-added.html` (scratch, `dataset.vip` confirmed landed before run) | 1 | `acd4085e…fced8` | `S16-derive-added-red.log` |
| S17a | `node economics-simulator-ui-gate.mjs --html /tmp/S17-banned.html --vocab-only --expect-red pledge` (scratch sentence confirmed landed) | 0 | `919263be…55acb` | `S17-banned-fires.log` |
| S17b | `node economics-simulator-ui-gate.mjs --vocab-only --expect-red pledge` (clean page) | 1 | `b63c0e36…35611` | `S17-clean-nofire.log` |
| S18 | `node economics-simulator-ui-gate.mjs --html /tmp/S18-mutant.html` (scratch `Number()` coercion confirmed landed) | 1 | `2ef50e36…d77e4` | `S18-mutant-red.log` |
| S19 | `bash …/handoffs/gate-v14-one-membership.sh /code/reactivegas-sim-fable` | 0 | `41cc3279…235e8` | `S19-gate-v14.log` |
| S20 | `nix develop --quiet -c just ci` | 0 | `4ac975bb…07b0d` | `S20-just-ci.log` |
| sent-p | `node economics-simulator-ui-gate.mjs --sentence-only` | 0 | `951a5d32…65bf` | `sentence-provato-0c3c1e9.log` |
| sent-e | `node economics-simulator-ui-gate.mjs --html /tmp/flip3.html --expect-enunciato --sentence-only` | 0 | `a8c11248…42ec7` | `sentence-enunciato-0c3c1e9.log` |

S13 and S15 are byte-identical by design — a neutered discard must behave
exactly like an ordinary run. **No datum in the artifacts distinguishes them.**
Their distinctness rests on the recorded invocation in this file, which is
owner testimony, corroborated only weakly by two distinct writes 22s apart
(`13:18:00`, `13:18:22`). It is closed by the auditor's independent re-run of
S15, not by this evidence.

Note on a retired claim: `grep -c "witness K-2"` = 1 in all three logs
(S13, S14, S15) because the S14 discard line
`omit: witness K-2 (…) scartato` contains that substring — it counts a
different thing in each file and separates nothing. The datum that separates
S14 is `grep -c "omit: witness K-2"` = 1 (S13/S15: 0), which is sound and
is the recorded S14 discriminator.

Pre-repair rows (superseded, retained): `omit-K2-red.log` (`5738becd…`,
old flag-branch shape), `mutant-caught.log` (`1fbc0018…`, focused probe),
`flip-red.log`/`flip-green.log`, `gate-v14-final.log` (`323cec17…`, S11),
`just-ci-final.log` (`4ac975bb…`, S12 — identical to S20, expected: the
`ed3220e→0c3c1e9` delta is the harness file alone, which `just ci` does
not process).
