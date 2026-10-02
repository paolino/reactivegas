# Spec — #86 exporter successor (full acceptance + CI/tooling/context/arity repairs)

Issue: https://github.com/paolino/reactivegas/issues/86 (under #67, #72, milestone 2).
Successor to #74 — **not a third #74 submission**. #74/PR#78 stay open, frozen, undelivered.
Base: accepted S1 `4a6cd87`. Inherited candidate: `9c8756a` (frozen, unaccepted).
Audit of record: `audit-s3` report `654f14ce…73853866`, verdict AUDIT-FINDINGS, blocking 3.

Authoritative inputs: issue #86 body (complete); ticket brief
`/tmp/reactivegas/ms2/e-haskell-impl/t86-exporter-successor/brief.md`
sha `659c37be…0405a5f4`; `RECUT-74-PROPOSAL.md` sha `bee72ccc…08bd1c`;
`audit-s3/report.md` + `onward-discoveries.md`; `.archived/audit-s1+s2`.

## Observable outcome

A stranger from a clean checkout can: run one command to get both corpus
files; run one command proving checked-in files match live Lean emission,
seeing non-zero on any byte drift; confirm the committed CI workflow invokes
that verifier (local execution of the committed path + real remote CI green
at the clean SHA, neither substituting); reproduce in a clean dev shell
(`jq` declared); trust every wrapper value is live-bound (4 mutants killed);
see malformed `check` arity refuse without writing; read a coverage handoff
describing today with dated history preserved.

## Requirements

- **R86-A — committed CI path runs the verifier.** `.github/workflows/ci.yaml`
  invokes corpus verification additively (loses nothing: `just lean` keeps
  dependency direction, inversion coverage + negative control, trace-coverage
  agreement, `lake build`). Evidence: (1) local execution of the committed
  CI job/step command on corrupted fixtures in an isolated env, wiring +
  command identity bound to the candidate, removal/bypass detected, reported
  as local execution; (2) real remote CI green at the clean final SHA.
  No mutant push authorized or required. Do not weaken existing steps.
- **R86-B — declared reproducible tooling.** `jq` (+ any other recipe tool)
  declared in the dev shell (`nix/`). Evidence: success in a clean
  reproducible env (`nix develop --ignore-environment` or equivalent), not
  via host binary; omission control (remove declaration → fails, attributed
  correctly to the missing declared tool). Failing closed necessary, not
  sufficient. No "portability advisory" label.
- **R86-C — every approved wrapper value live-bound.** `view`, `initial`,
  `auth` as well as `traces`/`steps`. All four survive-mutants killed: (1)
  economic `.view.members[0].key` → `"ZZZ"`; (2) integrated
  `.initial.members` → `[]`; (3) economic `.auth` refusing → permissive;
  (4) integrated `.auth` refusing → permissive. Bounded claim only: **the
  selected live-call / derived-`ToJson` method does not establish
  serializer-instance independence** (an independent encoder could differ;
  none required here). Exact byte-level key-set checks stay mandatory for
  their shape scope. No wrapper/schema widening.
- **R86-D — malformed check arity refuses without writing.**
  `corpusExport check <one-path>` exits non-zero, no output mutation —
  proven by pre-placed sentinels + unchanged-directory comparison, not exit
  code alone. Scope: malformed-arity only; no broader atomic two-write
  promise; declared limit (failed second write leaves first) stands.
- **R86-E — coverage handoff describes today.**
  `handoffs/CORPUS-COVERAGE.md` reflects current hashes + zero `UNPROVED`,
  preserving dated older evidence (pre-S1 measurement stays, dated). Stale
  `Trace.lean` comment routes desk → #66/#71 as quality/docs follow-up, no
  model-edit grant, no exporter semantics change.
- **R86-F — inherited rows re-established, all open to falsification.**
  `G74-CALLS-EXISTING`, `G74-ENVELOPE-CLOSED`, `G74-VERIFY-FAILS-CLOSED`,
  `G74-ADDITIVE-ONLY` each re-run on the successor candidate. Prior KILLED
  receipts are inputs, never acceptance; auditor may challenge any PASS.

## Corpus content fence

Emitter output stays **byte-identical to `9c8756a` inputs**
(`economic.json 14494B 73a077fc…`, `integrated.json 7673B 1f173aec…`)
unless a separately accepted upstream integration forces a separately
recorded re-emission. Do not anticipate #68/#69. No wrapper/replay-schema
widening. If accepted master advances, integrate + re-establish complete
acceptance at that final base, caps + identities preserved.

## Rejection behavior

- Any row without an executable check + demonstrated-failing control rejects.
- Source-text grep alone never closes a row (lead, not evidence).
- Green gate ≠ mandate: rows ARE the mandate.
- Wrapper widening, schema widening, `docs/en/design/` write, Lean
  theorem/guard/step/state/`Trace` change, or `paolino/kelgroups`
  implementation rejects even if green. Fence crossing → stop + question.
