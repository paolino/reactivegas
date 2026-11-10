# Data model — #75

## vote.json (wrapper)

Exact top-level keys `["auth","initial","steps"]` (same discipline as
`integrated.json`; no threshold in the wrapper — it lives in the sidecar).
Each step: exact keys `["accepted","change","event","signer","state","voteError"]`.
`voteError` is a `VoteError` constructor name or `null`.

Invariants:
- `accepted = false` ⇒ `state` equals the previous post-state (or `initial`).
- vote-routed `.app` step with `accepted = false` and a vote-machine refusal ⇒
  `voteError` is that refusal; `accepted = true` ⇒ `voteError = null`.
- every value equals the live emission, element for element, nonzero extent.

## vote.context.json (sidecar, R3.1 §5)

Exact keys and nesting as R3.1 §5; `corpus.path` = `corpus/vote.json`;
`corpus.contentSha256` = SHA-256 of the raw bytes of `lean/corpus/vote.json`;
`policies.threshold.table` = `[[n, legacyThreshold n]]` for `n ∈ {0..N}`,
`N` ≥ every threshold query generation made; `label` = `legacyThreshold`;
`policies.backdonateAuth.semantics` = `always-refuse`, `label` = `probeAuth`;
`generatedFrom.exporter` = `Reactivegas.CorpusExport`.
Canonical table validation: R3.1 §3 rules 1–5; no monotonicity.

## sourceInputDigest (concretized encoding)

- Projection = `lean/lakefile.lean`, `lean/lean-toolchain`,
  `lean/lake-manifest.json`, and the `.lean` file of every module in the
  transitive import closure of `Reactivegas.CorpusExport` (module `A.B.C` →
  `lean/A/B/C.lean`; the exporter's own file included; Lean core/`Init`/`Lean`/
  `Std` modules excluded as toolchain-pinned).
- Path form: repository-relative, `/` separators, no `./`, no trailing `/`.
- Order: ascending bytewise (`LC_ALL=C sort`).
- Record: `<path>` 0x00 `<lowercase hex SHA-256 of file bytes>` 0x0A.
- Digest: lowercase hex SHA-256 of the concatenation of all records.
- Excluded: `lean/corpus/**`, `lean/.lake/**`.
- Reproducibility: every projected path's working-tree bytes equal its `HEAD`
  bytes, and none is untracked; otherwise the gate fails.
