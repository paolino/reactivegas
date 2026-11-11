# SUBMISSION-2 CONSOLIDATED RETURN — instruments, costs, gap (NOTE-014 + NOTE-015 + NOTE-016 + NOTE-017)

**From:** S4-B commit owner (`muse`, pid/pgid 1493708).
**State:** repair commits `59309d6` + `0f3ad01` + `4d0a324` (Boundary-1 design; clean tree; base `3590c001`,
verified unchanged). Prior handoff + NOTE-013 supplement stand except where
corrected below. **No OT/O run executed** (binding + gap-grant pending).
**Spend at return:** submissions 1/2 (sub-2 open), substantive 8/14, targeted
42/60. This return spends nothing.

## 1. Production isolation (NOTE-014): shadow drivers replace hermetic content

The catch-22 is real and verified at source: no consistent olean world
elaborates Mirrors against mutant Types while Step asserts comune behaviour
(Step sits between them; its decide-assertion fires first and Lake never builds
importing modules after a failed prerequisite — within-file sorry-transitivity
does NOT cross modules, correction accepted). Hermetic files (hashes
`757bd4e6…`, `f69a9003…`, twins) moved to `instruments/superseded-hermetic/`
**unexecuted** with a note; they are not evidence and cost nothing further.

Replacement (same frozen paths, new bytes):

- `S2-chain-P01.lean` (`7bc5c01f…`): imports REAL `KelGroups.Types` ONLY (no
  Step import, so the comune assertion can neither fire nor mask); all else
  copied verbatim (pure lookup helpers, `comuneId`/`comune_not_a_member`,
  post-promotion helper statements/proofs, P01-orig contrast). NEG (shadow
  mutant Types.olean first): exit 1 EXACTLY at the two helpers. POS (clean
  olean): exit 0.
- `S2-chain-P07.lean` (`9dab73e2…`): imports REAL Types/State/Step (mutant Step
  in neg via shadow-first, clean in pos); pure helpers + chain
  statements/proofs + P07-orig copied verbatim. NEG: exit 1 AT `step_close_inv`
  (`close_guard_inv` proves; `close_permission_to_close` elaborates only via
  the broken link; P07-orig proves). POS: exit 0. No TraceTests import, so
  trace decide-flips can neither fire nor mask (those are O4 evidence).
- Argv deviation (documented, probe-proven): DIRECT `lean` with explicit
  `LEAN_PATH` (shadow-first for neg, build-lib for pos), NOT `lake env lean`:
  measured `lake env` appends its paths LAST, which would let the clean olean
  shadow the mutant silently. Each run's outcome authenticates which olean
  loaded. Owner binds this deviation or directs otherwise.
- Shadow compiles (`lean -o`, single-file elaboration+codegen, NO lake project
  build, NO dependency rebuilding beyond loading existing oleans, NO test
  execution): proposed-targeted by actual work, with the retained T6/T7
  precedent (counted targeted in submission 1 without dispute). If reclassified
  substantive, the consequence is 16/14 (raise required, NOT requested).

## 2. Discovery boundary (NOTE-015): design + O2-embedded reach proof

Independently verified: CorpusExport owned (imports Trace only; no top-level
effects, no decide-assertions), registered (lakefile exe root), prefix-passing,
absent from all checker umbrellas. S2R tracked rule mirrored exactly.
- Checker: builds `corpusExport` exe incrementally in-run (necessary — default
  `lake build` skips non-default exe targets, corroborated by the S2R
  worktree's missing exe oleans; log-visible, same invocation, no hidden work),
  imports it, owns modules by per-run `git ls-files`-baked TRACKED set
  (provenance, not spelling; toolchain/deps excluded by provenance), enforces
  tracked⊆imported completeness fail-closed (future modules fire loudly; never
  a silent allowlist), keeps F01/promotion/rows/orphans/evals/receipt/nonce.
- O2 stages THREE probes in one collection-semantics run (Predicates-opaque +
  CorpusExport-opaque + orphan mirror): kind + reach + S03, each distinctly
  named in the receipt (uncovered×2 with homes, orphan, fail, no receipt).
  CorpusExport-opaque (`opaque s4b_opaque_export : Prop := True`, core-only):
  exercises reach + kind jointly. Fail-closed ≠ discovery is preserved as a
  distinction: completeness refuses at module granularity (named + remediation);
  predicate naming follows once imported (O2 proves both directions).
- OT4 mirrors the classifier (old/new identity sets, sort-undecided bucket,
  conditional OK); ownership by prefix there with documented extensional
  equivalence on today's tree (no live Boundary-2 example — NOTE-015's own
  finding) while the GATE enforces tracked-set provenance (O-runs red
  otherwise); exe-root coverage belongs to O-runs (stated scope split).

## 3. O4/O5 failure taxonomy (certain vs reported-as-observed)

- O4 CERTAIN: `step_close_inv` (guard-shape defeq vs `true`-atom).
  EXPECTED-AS-OBSERVED (same defect family): trace `closePurchaseNegative`
  flip via `all_checks_pass`, `frozenChecks` mismatch, appended print `true`
  (50-`info:`-line precedent; actual presence reported honestly).
- O5 CERTAIN: exactly the two promoted helpers (defeq through constant-false
  body); downstream elaborates via intact statements. EXPECTED-AS-OBSERVED:
  trace decide-flips over `isMember`-guarded steps, `comune_cannot_authorize`.
- C4/noop: mechanism-identity transfer (neuter point precedes ALL repaired
  bytes — verified: repair regions disjoint from lines 1–2/nonce/env/writeFile/
  assertion, all byte-identical 189e1ed..HEAD; a neutered run cannot reach
  repaired code). Single flagged transfer claim; auditor free to re-run it in
  campaign budget. NOT an extra run.

## 4. Exact fit/gap (consolidated; NOTE-017-corrected base 8/14 + 42/60)

- Substantive: O1–O6 = 6 new → **14/14 EXACT, zero slack, no gap.** (Fallback
  compiles-as-substantive → 16/14: raise required, NOT requested.)
- Targeted: P01{compile,neg,pos} + P07{compile,neg,pos} + OT3 + OT4 = 8 ops
  vs 4 sheeted → **gap +4** (42 + 8 = 50/60, ceiling-safe; sheet-gap return,
  not a raise). Hermetic files cost nothing (unexecuted).
- Proposed order (fresh-olean hygiene; owner binds): O1 (fresh) → OT3 → OT4 →
  SH×6 (P01compile/neg/pos, P07compile/neg/pos) → O2 → O3 → O4 → O5 → O6.
  Receipts: `handoffs/evidence/S2-O{1..6}.log`, `S2-OT{1..4}.log`,
  `S2-SH-P01{compile,neg,pos}.log`, `S2-SH-P07{compile,neg,pos}.log`.
- Coverage mapping: P01 isolation (shadow pair + O5) ✓ in fit; discovery
  boundary (O2-CorpusExport + completeness + census) ✓ in fit; noop via
  transfer (flagged) ✓ no run. No row dropped, narrowed, or closed by
  implication. New-identity work measured zero (exporter adds no Props;
  discovery re-derived at every green run).
