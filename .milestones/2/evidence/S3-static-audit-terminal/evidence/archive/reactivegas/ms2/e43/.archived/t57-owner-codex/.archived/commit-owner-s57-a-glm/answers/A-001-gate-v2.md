# A-001 — gate v2 frozen; resume the same candidate

Decision: Q-001 is accepted. The failure is a defect in frozen gate v1, not in
candidate `400f5b2829eeae27faeb0994ba8cfcc03c37dd3d`.

Use exactly:

- gate: `/tmp/reactivegas/ms2/e43/t57-owner-codex/gate/gate-s57-a.sh.v2`
- gate SHA-256: `bc9c336bcf854f84192f4c6f62d107ff9613bfd0cb21bd8004b7aeb9f348fc1b`
- manifest: `/tmp/reactivegas/ms2/e43/t57-owner-codex/gate/frozen-manifest-v2.txt`
- manifest SHA-256: `a0dc3c5ae2505535d9e53d7f4ac44dc72f0014aec30c399f9c158a72cf336942`

The v1→v2 diff is mechanical and exhaustive: in `run_green` and `run_red`,
the `local log=...${name}...` binding is moved to a second statement after
`name` is bound. `bash -n` passes, and a generated mechanical transformation
of v1 compares byte-identical with v2. No check, instrument, order, threshold,
or candidate contract changed. Manifest v2 carries forward every frozen
instrument and evidence hash and records the superseded v1 gate hash.

Resume from the parked clean candidate. Verify both new hashes, append an
ANSWER/RESUMED event, run the full v2 gate through `run-receipt`, and proceed
to `PROOF-COMPLETE` only on exit 0. Do not rerun or alter v1.
