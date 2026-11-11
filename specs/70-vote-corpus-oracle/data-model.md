# Data — #70 slice O

Read: corpus `{auth, initial, steps[{signer, event, accepted, state, change, voteError}]}`
and sidecar per R3.1 §5 (#75 `data-model.md`). Outcome: one of PRE-REPLAY REFUSAL
(rule), RUNTIME ABORT (queried `n`, domain `{0…N}`, checked prefix length),
CONFORMANCE MISMATCH (step index, field), or PASS (steps replayed). No new state.
