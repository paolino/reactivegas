# S2R Submission — ONE submission under this grant

**Candidate:** `714cb2a8536b24bf735295137e8f907782228380`
(`chore/66-s2r-ownership`, worktree `/code/reactivegas-66-s2r`, committed-clean)
**Base chain:** accepted `4a6cd87` (S1) → accepted `d670323` (PR #87 exporter)
→ this candidate = `d670323` + 6-file diff (`+467/−65`, no model/semantics change).
**Fence (exact, all permitted):** `scripts/check-lean-axioms` (new, 100755),
`scripts/check-reactivegas-inversion-coverage`, `justfile` (1 line),
`lean/Reactivegas/Invariants.lean` (Row B renames only),
`lean/Reactivegas/TraceTests.lean` (dead re-exports only),
`lean/Reactivegas/Predicates.lean` (doc path only).
Pre-rebase evidence (logs `01`–`09`, `probe-01`–`probe-22` at `586d4dd`-era
content) is preserved as evidence at its SHAs and is NOT inherited as
acceptance — every row below is re-established at `714cb2a`; no confounded
old-base runs are counted.

## 1. The five required elements, each with executed evidence

### Element 1 — resolver / ownership authority and canonicalization

The authority deciding project-vs-dependency is the **loader's own search path**
(`Lean.searchPathRef`: `LEAN_PATH` order plus the toolchain builtin), resolving
each environment module to the olean artifact it actually loaded via
`findModuleWithExt` (existence-checked, first-package-root rule). Both the repo
root (`REACTIVEGAS_ROOT`) and each resolved artifact are canonicalized with
`IO.FS.realPath` — relative entries, `.`/`..`, symlinked and aliased vendor
dirs — before a trailing-separator prefix comparison. No name or namespace
list (grep-verified absent), no `import Lean` closure, no `B := S`
(S from git+env-var, B from env+provenance — distinct layers, both
`S \ B` and `B \ S` representable and both fire). Code:
`canonRoot`/`artifactInsideRoot`/`resolveProjectModules` in both gate scripts.
**Not a spelling test:** the `11`/`13`/`14` logs are byte-identical
(sha `2df7bc69…`), proving no path spelling leaks into any output.

### Element 2 — actual project/dependency source-output relation

The relation is the loader resolution itself, derived per module at runtime: a
tracked source counts as built exactly when the environment's module resolves
to an artifact canonicalized inside the root. Derived, never asserted —
P33 proves it: `Std.Data.DHashMap` imported through the symlinked vendor entry
in FIRST search position still resolves (realpath) to the Nix store and is
excluded (`tracked=27 built=27`, all 262 `Std.*` out). R2 proves the converse:
the registered outside-root clean module IS built into the project dir and
swept (`axiom-module AuditS2R.StdClean`).

### Element 3 — equivalent-path controls (identical classification)

| leg | command | observation |
|---|---|---|
| symlinked invocation | R3 `just lean` via `/tmp/s2r-rel/wt` symlink | byte-identical log to R1 (sha `2df7bc69…`): 27/27/27, 1214/1213/1213, 163/163 |
| relative entry | P34 `LEAN_PATH=.lake/build/lib/lean:<toolchain>`, direct `lean` (lake would absolutize) | B=27 T=1213 ok identical (the F-004 B=0/T=0 shape) |
| aliased vendor | P33 `LEAN_PATH=vendor:build:toolchain`, direct `lean`, Std imported | B=27, Std excluded (the F-004 B=288/T=15707 shape) |
| alias presence | R4 `just lean` with vendor dir on disk + appended entry | identical to R1 |

### Element 4 — independent source omission (both directions, project-owned)

- `S \ B`: P26 — imports-minus-`Reactivegas.TraceTests` (zero-cost grep proves
  nothing imports it: genuine leaf) with S=27 full → EXIT 1 naming exactly
  `Reactivegas.TraceTests`. Same run covers A5 removed-module + A2′c4.
- `B \ S`: P27 — full 27 imports with S-var minus `KelGroups.Tests` → EXIT 1
  naming exactly `KelGroups.Tests`. Layers explicitly distinct.
- CONTAMINATION (named explicitly per NOTE-007; binding): both P26 and P27 logs
  carry, beside their intended S/B findings, one extra axiom-policy finding
  (`s2rAuditOnlyUse` + `s2rAuditOnlyAxiom`) from the stale R6-poisoned
  `CorpusGate` olean (source restored before the runs; that olean never
  rebuilt until R7 — restored-source-mtime-newer-than-R6-olean). The logs prove
  the S/B branches EXECUTED and named their identities, but their aggregate
  exits are NOT unmasked necessity tests for the S/B checks (the extra failure
  would persist with the S/B check disabled — the A-003 masking rule). R7's
  later rebuild does NOT retroactively clean these earlier inputs or outputs.
  The fresh FULL auditor must re-execute both controls on its own clean
  candidate artifacts, requiring the intended identities with no unrelated
  policy findings.
- Mapping note (flagged, not silent): genuine B-Lean`\`S through `just lean`
  is unreachable by construction — the driver imports exactly S (transitive
  closure covers drops, as P12-redundant run demonstrated on Tests), and any
  file-level omission fires first at the bash S-agreement or the build. No
  theatrical mislabeled run was staged; the row stands on P27 at its honest
  no-build probe class with this construction proof.

### Element 5 — missing-authority behaviour (exactly one lawful outcome evidenced)

**RETAINED** the empty/unset `LEAN_PATH` guard — the authority genuinely reads
`LEAN_PATH`, so the branch is live (no dead code, no retirement to record).
P24 (`LEAN_PATH=`) and P25 (unset), minimal `import Lean` driver (the only form
reaching the guard — project imports need a search path): BOTH EXIT 1 with the
exact single finding `ownership authority missing: LEAN_PATH empty or unset`,
no panic, fail-closed before any provenance claim. The G-001 branch unfired
across three campaigns now fires.

## 2. Every original row, re-established at `714cb2a`

| row | evidence (final candidate; `EV`=runtime `handoffs/evidence/`) |
|---|---|
| A1 S/B/T + reconciliations | R1: S git-vs-walk agree 27/27, B provenance-filtered 27, T walk/fold agree 1214occ/1213dist, identities printed (1213 `axiom-theorem` lines), S∖B=B∖S=∅ |
| A2′ no quota + 8 constraints | code grep clean (no `expectedDeclarations`, no `== 163`); R2 164/164 valid-add passes; P26/P27 omit-by-identity; R5/R6 fail FOR DEPENDENCY (below); counts+identities printed; six `requiredInversions` independent (R1/R7 `axioms=`+`bound=`+`tight=` lines); no model/statement change (diff) |
| A3 cold provenance | R1 after `rm -rf lean/.lake` (olean-count 0 pre-run, journaled) |
| A4 nonzero | S-zero P32 (first finding operative) · B-zero P31 (own stage, Lean-only driver) · T-zero P35 (sole finding, A-003 route) |
| A5 added-module | R2: new tracked module through `just lean`, swept, gate-ok |
| A5 truncated inventory | P28/P29: one-sided skips of `conservation_preserved` fire from the exact side (1212v1213, identity named) |
| A5 removed module | P26 (import-drop rejected by identity) |
| A5 sorry | R5: existing `i57_noexpiry_holds` → `by sorry` through `just lean` → names theorem + `sorryAx`, census steady 163/163 |
| A6 policy + using shape | permitted = `propext,Classical.choice,Quot.sound` (the inversion `:101` set, extended — both scripts' headers); R6 `axiom s2rAuditOnlyAxiom : False` + use → `depends on axioms outside…: s2rAuditOnlyAxiom` with `axioms s2rAuditOnlyUse = [s2rAuditOnlyAxiom]`; transitivity rides `collectAxioms` (documented; direct-use executed per precedent) |
| A7 mandatory path | `just lean` runs dep-direction + inversion + inversion-neg + **axiom gate** (line 63) + agreement + `lake build`; `just ci` adds toolchain-contract/build/format/hlint/corpus-gate/verify (R7 green) |
| A8 totality | axiom gate has no partial calls (`!`/`getString!`/`panic!` absent — code fact); inversion pre-existing partials covered by BOTH wrappers' `PANIC at` checks on BOTH streams (code) + fixture demos hit both streams + zero PANIC in all evidence logs + required markers present |
| Row B (9 renames) | old→new table below; P30: 12 absent-ok (9 + 3 re-exports) + 9 present-ok in compiled env; docs rewritten accurately (trust→axiom-gate, direction→dep-script, toolchain→pin-contract; no impossibility claims) |
| Row B4 (3 dead re-exports) | deleted from TraceTests (diff); P30 absence + source grep clean |
| Row C doc path | `docs/design/` → `docs/en/design/` (one comment line); `docs/` untouched |
| §5 driver rule | NO new tracked Lean driver (embedded `/tmp` drivers only); nothing to declare; no #70 overlap; transient `AuditS2R` registration added+removed within R2 (final diff has no lakefile change) |
| AMENDMENT-1 quota-blindness | R5: sorry leaves census 163/163 while the axiom gate kills for `sorryAx` |
| F-001 prefix hole | no name list anywhere (grep); R2 outside-root swept |
| F-002 T truncation | dual derivations + P28/P29; skip-both stays advisory (below) |
| F-003 import-Lean closure | no closure used; Std excluded at default (R2) and vendor-first (P33) shapes |
| F-004 lexical vs resolved | realpath both sides; R3/P33/P34/R4 (log-sha identity 11=13=14) |
| G-001 empty/unset guard | P24/P25 exact single finding |

Row B rename table (all value-preserving, `= true := by decide` theorems intact):
`i57TrustNoSorry→admissionPreservationReachable`,
`kelGroupsHasNoReactivegasImport→productionWellFormedAndComuneExcluded`,
`leanToolchainMatchesPin→comuneIdentityAndThresholdSanity`,
`checkI57Trust→checkAdmissionPreservation`,
`checkI57Direction→checkProductionWellFormed`,
`checkI57Toolchain→checkComuneThresholdSanity`,
`i57_trust_holds→admissionPreservation_holds`,
`i57_direction_holds→productionWellFormed_holds`,
`i57_toolchain_holds→comuneThresholdSanity_holds`;
deleted: `TraceTests.checkI57Trust/Direction/Toolchain`.

## 3. Ownership authority + fail-closed behaviour

Authority: §1 above. Fail-closed branches, each with status: empty/unset
`LEAN_PATH` → executed (P24/P25) · unresolvable ROOT → present, fires on
missing env (code; not separately executable in green builds — honest limit)
· module with no loadable olean → present (same limit) · zero S/B/T →
executed (P32/P31/P35) · S∖B/B∖S → executed (P26/P27) · T-disagreement →
executed (P28/P29) · extra axiom → executed (R5/R6) · `PANIC at` either
stream → wrapper rejects (fixture-demonstrated) · missing `*-ok` marker →
wrapper rejects (code + every green log carries markers). A directory or
module-name assertion appears nowhere as authority.

## 4. Exact enumerated spend (failed/warm included, no double-count, no mislabels)

Substantives 17/17 — pre-rebase base (`01` INV-1 Row-B `=`/`:=` syntax fail;
`02` INV-2 `Prod.toString` fail; `03` baseline green; `04` Std-root green;
`05` symlink green; `06` alias green; `07` sorry RED-by-design; `08` axiom
predicate-shape wrong-layer fail; `08b` axiom RED-by-design; `09` full ci
green) + final base (`11` R1 cold green; `12` R2 green; `13` R3 green;
`14` R4 green; `15` R5 sorry RED-by-design; `16` R6 axiom RED-by-design;
`17` R7 full ci green). Warm-cache gate runs counted per NOTE-008 precedent.
Probes 35/35 — pre-rebase `probe-01`–`probe-22` (driver preflights incl. two
vacuous-target lessons kept, extent census, loader forms, guards, omissions,
T-skips, B4, S-zero, axiom-block validation) + final base `probe-23`
config-load, `probe-24/25` guards, `probe-26/27` omissions (each carrying the NAMED P26/P27 contamination: extra
stale-R6-olean axiom finding alongside the intended S/B finding; clean
single-failure provenance explicitly DISCLAIMED; auditor re-execution on clean
artifacts required — see Element 4),
`probe-28/29` T-skips, `probe-30` B4, `probe-31` B-zero (B-zero evidence
only — T-zero co-finding disowned per A-003 masking), `probe-32` S-zero,
`probe-33/34` loader forms, `probe-35` isolated T-zero (sole finding;
variant+2-line diff preserved in evidence). Failed setups included.
Zero-cost (never counted — no Lean compilation): all reads/greps, git
rev-parse/fetch/diff/status, version interrogations, file writes, journal
edits, A8/PANIC + S-agreement fixture demos, mtime/hash stats, commit +
clean rebase + conflict-free merge verification. Evidence:
`handoffs/evidence/` (substantive logs `11`–`17`, probe logs, variant
files; representative sha256: `11/13/14` identical `2df7bc69…`,
`17-final-ci` `e467941f…`, `probe-35` `60281fbb…`).

## 5. Bounded advisories (not requirements, not smuggled)

**CI-T-SHARED-FILTER:** both T derivations share `thmInfo`+B membership — two
views of one inventory. One-sided truncation detected (P28/P29); skip-both and
T-side B-shrink survive by construction (accepted, unchanged). Kept strictly
apart from A4 empty-T (P35): nonzero-subset agreement ≠ empty-inventory
rejection. Shadow-name invariant: advisory, untouched. Transitivity:
`collectAxioms` semantics (direct-use executed).

## 6. Honest limits and mapping notes (for the auditor, not hidden)

- B-Lean∖S through `just lean` is unreachable by construction (driver imports
  exactly S; file omissions fire first at bash S-agreement/build — demonstrated
  by the vacuous-then-redundant Tests-drop lesson); element-4 B∖S stands on
  P27 at its honest no-build class with the construction proof. No theatrical
  mislabeled run was staged.
- Isolated T-zero (B≠∅,T=∅) is unreachable in production inventory (zero-thm
  modules `{Reactivegas, CorpusGate, KelGroups.Tests, KelGroups.Vote.Fold,
  KelGroups.Vote.Tests, KelGroups}` each import theorem-carriers — shown);
  P35 tests the broken-derivation form like the truncation controls do.
- Pre-rebase logs are evidence at SHAs only.
- The three rejected candidates (`5745a2c`, `561347d`, `b0c2cdb`) were not
  consulted for ownership code; this gate is an independent re-cut (loader
  search-path + canonicalized artifact identity).

## 7. Rebase appendix (A-001 provenance — why full re-establishment ran)

Incoming `4a6cd87..d670323` (+767/20): `justfile` +37 (new export/verify
recipes + `ci` wire; `lean` recipe lines untouched — conflict-free rebase),
`lakefile` +3 (`lean_exe corpusExport`), `ci.yaml` +6 (verify step),
`CorpusExport.lean` +186 (10 defs, 0 theorems/examples/lemmas/inductives —
S 26→27, census/T steady), corpus/specs/nix docs+data (outside all extents);
no existing `.lean` modified, no toolchain/manifest change, no name
collisions, umbrella unchanged. Per-row verdicts: R1 MUST (S/build-graph/CI
changed) · R2 MUST (registration graph changed) · R3/R4 MUST (outputs claimed
identical over changed extents) · R5/R6 MUST as invocations (kill mechanisms
provably disjoint but full-output evidence is S-dependent) · R7 MUST (`ci`
recipe + `ci.yaml` + nix inputs changed). All re-ran green above.

## 8. Remote leg — parent-owned, parent-executed (this seat executes nothing outward)

The remote leg is authored and executed by the ticket owner/parent under desk
grant. Required shape (NOTE-008): `--draft` PR against `master`; body from a
file with real newlines carrying the result, the evidence and the residuals in
its own reviewer-facing text (no local-runtime-path references, no closing
keywords); assignee `paolino`; label `chore`; milestone 2;
`closingIssuesReferences` verified empty by readback. Branch
`chore/66-s2r-ownership` verified never-pushed, no upstream, no PR (PR #85
belongs to the superseded S2 branch).
Status: EXECUTED by the parent (this seat executed nothing outward).
- PR: https://github.com/paolino/reactivegas/pull/88 — DRAFT, OPEN, base
  `master`, head `714cb2a8536b24bf735295137e8f907782228380` (exactly the
  candidate; the branch had no prior upstream).
- `closingIssuesReferences`: `[]` — verified by readback, not by intent.
- Assignee `paolino`, label `chore`, milestone 2.
- PR #85 untouched (still OPEN draft on the superseded branch); issue #66 OPEN.
- Body parent-authored, not lifted from the removed draft text: the six-file
  result; the loader/canonicalization authority with its Std-exclusion and
  outside-root converse evidence; the printed S/B/T reconciliation and the
  removed quota; the nine renames tabled against the three decoy checks with
  the real mechanisms now carrying those properties; the three deleted
  re-exports; the doc path; `just ci` exit 0 with the 2812-line log hash; a
  residuals section (audit-pending/not-accepted, P26/P27 contamination with
  required fresh reruns, shared-T-filter and shadow-name advisories, honest
  unreachable branches); S3/S4/S5 unfinished and #66 open.
- Remote CI at last readback: Build artifacts SUCCESS, Sync Cabal version
  skipped, main CI job in_progress — recorded as RUNNING, not green.
- Fresh FULL independent audit underway in parallel (Codex gpt-6-astra, effort
  high, both pinned in live argv, own post-cursor START, own cold detached
  worktree at 714cb2a with zero oleans) — not this seat's to contact.

## 9. Unclosed rows — stated without erasing the retained limit

- No KNOWN production blocker.
- Clean unmasked omission-control evidence REMAINS TO BE ESTABLISHED by the
  independent auditor.
- The named branch executions and contaminated aggregate exits of P26/P27 carry
  exactly the limitations stated in Element 4 and nothing more. They are NOT
  promoted to full clean necessity controls anywhere in this packet.
- The fresh auditor must execute both controls on its OWN clean artifacts and
  require the intended identities with no unrelated axiom or policy finding.
  Failure there, or a budget gap preventing the run, is a FINDING — never
  closed by owner-acceptance inheritance.
Owner for any follow-up: the commissioning ticket owner. Next state:
SUBMITTED → fresh FULL independent audit (owner-pinned model+effort,
post-cursor START) → exact final remote CI → explicit desk merge grant.
Required auditor shape for dispatch: fresh FULL Codex with model and effort
pinned in argv and its own START; scope all original rows plus final-base
integration at `714cb2a` with NO prior candidate or auditor PASS inherited;
must re-execute P26/P27 on clean artifacts (contamination note above); owner
reconciles every control row against a named invocation and a demonstrated
observation before auditor START.
