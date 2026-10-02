# Tasks — #86 exporter successor

Only the ticket owner checks behavior-task boxes, after a fresh FULL
`codex`|`grok` audit passes the exact candidate. Commit owner never stamps.

## Planning and gate

- [ ] **T8600** Freeze mandate (this dir) + immutable slice gate + per-row
      RED proof on unrepaired `9c8756a` (A–E RED, 4 inherited GREEN).
- [ ] **T8601** Context binding: `view`/`initial`/both-`auth` live-bound in
      `check` path + bounded claim comment + 4 mutants killed. (R86-C)
- [ ] **T8602** Arity fix: `check`-one-path refuses nonzero + zero writes
      (sentinels + dircmp). (R86-D)
- [ ] **T8603** CI wiring additive + local-committed-path execution +
      removal detection; remote CI green at clean SHA recorded. (R86-A)
- [ ] **T8604** `jq` declared + clean-env success + omission control with
      correct attribution. (R86-B)
- [ ] **T8605** Coverage handoff current (hashes + zero UNPROVED) with dated
      history preserved; stale comment routed, no model edit. (R86-E)
- [ ] **T8606** Full acceptance: 9/9 rows GREEN, emitter bytes identical to
      `9c8756a`, `nix develop --quiet -c just ci` GREEN, fresh FULL audit
      per candidate, draft PR provisional (no merge/comments). (R86-F)
