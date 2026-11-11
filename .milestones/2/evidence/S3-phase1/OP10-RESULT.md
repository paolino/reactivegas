# OP10-RESULT — authorized bounded discovery operation (charged)

Grant: NOTE-008. Entry from repo root, exactly as granted:
`nix develop --quiet -c bash scripts/check-lean-axioms`. No mutation campaign,
no Phase-2 execution. A failed setup would have spent the operation and
returned a gap — no retry was needed.

## Pre-run confirmations (all verified before the run)

- Worktree `/code/reactivegas-66-s3-phase1`: `git status --porcelain=v1` empty,
  HEAD `3590c0015b84fd58004bf6fb44dd18b107304c48` (the exact base).
- Pin `lean/lean-toolchain`: `leanprover/lean4:v4.25.0`.
- Script identity: `scripts/check-lean-axioms` unmodified at base (`git diff
  HEAD -- scripts/` empty), sha256
  `4fb40d5089e908d16f91ae35cfb479d9d454b2a6fd6f337627789a40851bc01b`.
- Driver: `nix` 2.31.3 present.

## Run record

- Wall: 35 s (23:29:36Z–23:30:11Z UTC 2026-09-05). Exit code: 0.
- Charging: internal `lake build` = +1 substantive (spend now 5v5 prospective:
  4 historical overrun + 1 granted); driver elaboration = +1 targeted.
  Targeted history recorded, never invented: 2 pre-grant elaborations
  (`lake env lean Reactivegas/CorpusGate.lean`, `.../TraceTests.lean`) + this 1
  = 3 total. No probes ever run.
- Layer honesty: `.lake` held 25 warm oleans, so the internal build was an
  incremental-tree build — NOT a cold run and NOT the lost cold log (which
  stays lost). The 35 s is wall for build+elaboration+gates at this layer only.
- Preserved: `handoffs/OP10-stdout.txt` (`e2770204…`, 138765 B),
  `handoffs/OP10-stderr.txt` (`28cff59b…`, 48 B),
  `handoffs/OP10-identities.txt` (`8fa4cc7c…`, 1213 distinct names, one per
  line, derived by `grep ^axiom-theorem` + sort -u).

## Observed outputs (read from retained stdout, not projected)

- `axiom-sources tracked=27 built=27`; 27 `axiom-module` lines (module set
  equals the S roster); `axiom-theorems walkOcc=1214 distinct=1213 fold=1213`;
  `axiom-duplicate-names=1 KelGroups.setInsert.eq_1`; `axiom-theorems
  count=1213`; `axiom-gate: ok`. (1214/1213 vs historical figures: comparison
  only — neither is or was a quota.)
- 1213 `axiom-theorem` lines AND 1213 `axioms {n} = [...]` lines (the output
  contains the compiled identity set as NOTE-007 settled). Gate exit 0 entails
  every identity depends only on the permitted set — no separate axiom claim is
  made here beyond the gate's own verdict.

## Reconciliation: 239 source vs 1213 compiled (neither a quota)

- All 163 non-private qualified source identities present VERBATIM in the
  compiled list (exact-line audit: 163/163, 0 absent).
- All 76 private source identities present via private-name mapping:
  `_private.<Module>.<idx>.<qualified-source-name>` (substring audit: 76/76, 0
  absent; e.g. `_private.KelGroups.Invariants.0.KelGroups.assocAdjust_keys`).
- Remainder 974 = compiler-generated/internal-detail: 95 `.inj`, 9
  `ofNat_ctorIdx`, eq-family/`injEq`/`sizeOf_spec`/`match_*.eq`, `_proof_*`,
  `inst*`, deriving outputs (incl. 133 generated `_private` entries beyond the
  76 source-private: 209 `_private` total). The 15 short-name pairs appear as
  distinct qualified identities (count-2 suffixes), confirming the R2
  intra/inter-file resolution in the compiled environment.
- Unexpected names: NONE — every compiled identity is either a source identity
  (239, via the mappings above) or a recognizable generated kind. Missing
  source identities: NONE (0/239). Findings of the unexpected/missing kind: none
  to file; the composition above is the account.

## Envelope effect

OP-10 (the only execution static work could not replace) is CLOSED with the
record above. Remaining Phase-1 work is static-only and complete in R5 +
CORRECTIONS-008 except future kill execution, which needs its own grant.

*End of OP10-RESULT.*
