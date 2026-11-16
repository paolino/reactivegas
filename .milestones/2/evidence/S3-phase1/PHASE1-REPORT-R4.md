# S3 Phase 1 report — REVISION R4 (one coherent updated packet)

Local only. No push, PR action, comment, gist, publication, deployment or merge.
No `docs/en/design/` writes. No coverage claim. No Phase-2/build/test/
elaboration/auditor grant exists; static assessment and repair only. Spend stays
**4 against ceiling 3** (explicit overrun, no refund). Mandate not narrowed:
239 qualified source identities are the floor; the environment extent is
unenumerated (G-B1).

- History preserved: original (`dbc2cb68…`), R2 (`32c06530…`), R3 (`9b5bf6c2…`).
- Base/seat/contract: HEAD `3590c0015b84fd58004bf6fb44dd18b107304c48`, #66 OPEN,
  seat PID/PGID `2401092`, contract rev 3. Worktree clean at R4.
- R3 content incorporated by reference where unchallenged (43-file roster, D1b
  entry points, ownership annex Families A–E, helper witnesses, cost layers).
  This revision repairs what NOTE-005 found and completes what it required.

## §1. Roster repair — phantom identities removed, counts re-derived

NOTE-005's finding verified from source: `grep -rn -E "(theorem|lemma)
assoc[A-Za-z_']*property" lean/` returns exactly three sites, all in
`KelGroups/Invariants.lean` (`:79 assocErase_property`, `:87
assocInsert_property`, `:109 assocAdjust_property`); zero occurrences in
`KelGroups/Vote/Invariants.lean`. The R3 roster annex printed three
`KelGroups.Vote.assoc*_property` entries that name nothing — phantoms created by
mis-qualification, and the R3 "count check" line compounded it (H-some 24 vs 23,
H-bool 5 vs 6). `P1A-qualified-classified.txt` already qualifies the three real
rows correctly and is unchanged. This was a document/measurement defect; no proof
is implicated.

Corrected witness groups, re-derived from the classified file (81 rows):

- H-none 40 · H-prop 3 (`KelGroups.assocErase/Insert/Adjust_property`) ·
  H-some 23 · H-mem-cons 4 · H-neq 5 · H-bool 6
  (`bool_not_true`, `bool_and_left`, `bool_and_right`,
  `demand_eq_true_of_some`, `demand_none_of_ne_true`, `eq_nil_of_isEmpty`).
  Sum 81. **81 helper / 158 authored / 239 total stand.**
- Corrected literal roster: the R3 annex minus the three
  `KelGroups.Vote.assoc*_property` phantoms; every other entry verified present
  in the classified file. (The annex is not reprinted in full here to avoid a
  third copy drifting: the machine-readable file governs, the counts above are
  derived from it, and the phantom-removal diff is exactly those three lines.)

## §2. Receipt binding after bounded retrieval (reuse classified only now)

Retrieval first: mutant sources, RED logs, audit reports, instrument manifests,
and gate scripts were read in the archive BEFORE the markings below. Full
receipts were located for the decisive rows; where they were not, the marking
says so. Line-total diffs (R3 §2 D1–D4) are retained as evidence that context
changed, but no receipt below rides on them alone — each carries its own
subject/checker inputs.

**t54 slice-a s2 (file 27; base lineage `757dac98`; D3 demonstrates subject
re-cut):**

| row | subject · mutation (retrieved) | checker / fixture / toolchain / command (retrieved) | kind (rev-3) | marking | re-key target |
|---|---|---|---|---|---|
| INV-54-PARTITION | archived `instruments/partition-silent-deletion-mutant.lean`: own `dropSweep` + `mutantFold` (read: replaces sweep, drops closed) — production files NOT mutated | RED log `evidence/partition-mutant-red.log`: `lean` exit 1 on unevaluated `#eval`-style expression (tail read); toolchain Lean 4.25.0 bound in log; driver per s1 method: per-module `lean` with LEAN_PATH overlay | FIXTURE (instrument shows the check reads its input) — reclassified from production kill: no production definition was mutated | STALE-DEMONSTRATED (D3 subject re-cut; fixture also uses removed `.admitMember` vocabulary — D5) | check-sensitivity for `KelGroups.Vote.questions_partition`; kills NO theorem row |
| INV-54-FRANCHISE | archived `instruments/franchise-unfranchised-recast-mutant.lean` (read: `badEffectedState` recast + own fold) + `evidence/franchise-mutant-red.log` (`lean` exit 1, 4.25.0) | same harness | FIXTURE — same reclassification | STALE-DEMONSTRATED (D3 + D5: trace uses `.admitMember`/`.setRoles`, absent from current `VoteEvent`) | check-sensitivity for `franchise_of_tallies`/`unfranchised_cast_noop`; kills NO theorem row |
| INV-54-POLICYFREE | s2 carried terminal from s1 (ledger states it; no s2 mutant) → s1: hard-coded `legacyThreshold` production mutation, witness RED, source/log full hashes in ledger | s1 per-module `lean` harness (script read: `franchise-policy-mutants.sh`, candidate pinned `757dac98`, clean-tree + olean preflights) | PRODUCTION-DEFINITION | STALE-DEMONSTRATED (D3: `Vote/Types`+`Fold`+`Invariants` re-cut) | threshold/verdict rows |
| INV-54-DISJOINT / NOSTALE (s1 C2/C4) | mutated full-module copies under `instruments/run.*/src/KelGroups/Vote/` (present in archive): opposite-tally erase omitted / ballot-only sweep | per-module `lean` elaboration REDs (`compile_one`: `(cd src && LEAN_PATH=lib:candidate_lib lean source -o out)`); candidate `757dac98` asserted in-script | PRODUCTION-DEFINITION | STALE-DEMONSTRATED (D3) | `ballots_nodup_disjoint` / sweep rows |
| INV-54-NOEXPIRY | gap instrument `no-expiry-member-event-gap-v2.lean` + GREEN logs (survival, read) | same harness | FIXTURE-gap (survival = assurance-gap evidence re member events, never a kill) | STALE-DEMONSTRATED (D3 + D5) | `KelGroups.Vote.no_expiry`, single-question scope kept |
| R-45 | boundary record | — | NON-MUTANT record | STALE-DEMONSTRATED (machine re-cut); disposition outside S3 | R-45 (preserved) |

**t48 (files 31–34, 29):** full instrument manifest with 64-hex hashes retrieved
(`instrument-sha256-manifest.log`, heads read); campaign/survivor/row logs
present. Checker script demonstrably rewritten (D1: 467+/247+ line diffs at the
recorded candidates `4898e55e`/`a408e09`). Markings: SURFACE/GUARDS/CANFAIL/
AXIOMS/INV-HYP rows STALE-DEMONSTRATED (checker rewritten; six-report path
superseded by the axiom gate); REGRESSION retired instrument, no counterpart at
base (D2); FENCE/EVENT-SYNTAX/STEP-ITE/STEPEVENT-DELEGATE STALE-DEMONSTRATED
(checker + `Step.lean` re-cut). Re-key targets as R3. Full-hash expansion
available in the archived reports; not transcribed here because checker
identity, not hash length, settles status.

**t57 (file 12):** auditor evidence + build-ledger + probe logs present in
archive; gate base `bb3ac41` recorded; toolchain 4.25.0 bound per receipt. D4
demonstrates the whole vote machine + `Step.lean` re-cut. All 10 rows
STALE-DEMONSTRATED with footprint note: per-row mutant↔source pinning is prose-
level ("fresh … mutant"), so re-execution needs new instruments (§5) — context
demonstrably changed AND exact old footprints unbound; both stated, neither
leveraged into reuse.

**t62 (file 22 + archived seats):** instruments/evidence/report (+sha256
sidecar) present in archive, but the main ledger's audit ref `3a7b355a` is NOT
in the object DB and no candidate is recorded → re-verification provenance
unrecoverable: the three KILLED rows are UNESTABLISHED-REUSE (evidence text
preserved, nothing disproved). The six OPEN rows are CORRECTED to open
obligations (R3's "STALE in the narrow sense" withdrawn — a resolvable blocker
is not a stale kill): DIRECT-ONLY/ATOMIC-HOOK/V3-BASE/CLOSED-SUMS awaited S62-B,
whose sealed vocabulary is now present at base — re-key to `KelGroups.*`
substrate rows owed; HISTORICAL/PROOF-TRUST remain pending with no new claim.

**D5 asymmetry kept:** only the t54 FRANCHISE/NOEXPIRY/PARTITION instruments
(and any instrument naming removed constructors) carry the stronger removed-
vocabulary claim. Nothing else is levelled up to it.

**All other files:** R3 §1C dispositions stand (each file's rows transcribed
with states; non-Lean subjects UNUSABLE for D2; evidence-NONE rows
UNESTABLISHED-REUSE as open obligations). **REUSABLE-BOUNDED: 0.**

**REUSABLE-BOUNDED: 0.**

## §3. Row map status (no wildcards; deferred rows stay visible)

- D1a/D1b extents, 43-file roster, ownership annex Families A–E, and the 239
  literal roster stand per R3 as corrected by §1 above (phantoms removed;
  counts H-none 40 / H-prop 3 / H-some 23 / H-mem-cons 4 / H-neq 5 / H-bool 6).
- One reclassification from retrieval: t54 s2 PARTITION/FRANCHISE KILLED rows
  are FIXTURE instruments (own `mutantFold`, production untouched) — they
  establish check sensitivity for `questions_partition`/`franchise_of_tallies`
  and kill no theorem row. The ownership annex is otherwise unchanged; the
  solvent/alias DEFERRED rows stay on the map with reasons.
- No execution is substituted for binding anywhere in this packet: new runs
  would speak about today, never about what a past receipt covered.

## §4. Helper satisfiability (corrected counts; instantiations per hypothesis)

R3 §4 stands with corrected group sizes (H-none 40, H-prop 3, H-some 23,
H-mem-cons 4, H-neq 5, H-bool 6 = 81) and the phantom-free roster. Each
witness maps hypothesis to concrete value (empties with cited nil-arms,
singletons with verified constructor shapes, literals by decide, vacuous
properties). No compiler testing of helpers is invoked; the argument is the
instantiations, which are all exhibited. The footprint operation is nowhere
relied on.

## §5. Numbered operation-to-requirement plan (static ops done; future ops exact)

Completed static operations (this packet; no execution): OP-01 qualified
inventory (239, §1/R3) → D1a extent; OP-02 classification (158/81, §1) → D1a
kinds; OP-03 receipt retrieval + binding (§2) → D3 inventory; OP-04 ownership
annex (§3) → D1 relation; OP-05 helper witnesses (§4) → helper obligations;
OP-06 roster audit (81/81 + 158/158 literal presence checks) → map integrity.

Future operations (exact argv; ALL UNGRANTED — requests, never authorization):

- OP-10 compiler-generated enumeration (→ D1a environment extent, G-B1).
  Wrapper inspected: `scripts/check-lean-axioms` runs exactly two measured
  subprocesses after free derivations (git/find/diff): (1) `(cd lean && lake
  build <27 explicit modules>)` — ONE build invocation (27 targets, incl. the
  3 unbuilt modules, so no separate build is needed or requested); (2)
  `lake env lean <tmp>/AxiomGate.lean` — ONE elaboration. Driver: seat shell
  with `PATH=/nix/store/4gr3n4nrp0xxgykyyzdxi3xjj2ikn5x1-lean4-4.25.0/bin:$PATH`.
  Expected observables: `axiom-sources tracked=27 built=27`, `axiom-theorems
  walkOcc/distinct/fold` counts, `axiom-gate: ok`. Cost: 1 build (≈19 s
  class) + 1 elaboration. The lost cold log is NOT re-run (G-B2 withdrawn).
- OP-11..OP-16 STALE kill re-verification, one admitted single-atom mutant per
  owning atom (→ D2/D4): exact method the s1 precedent used, quoted as argv
  template with concrete targets: `(cd <scratch-src> &&
  LEAN_PATH=<scratch-lib>:<base-lib> lean KelGroups/Vote/Fold.lean -o
  <out.olean>)` per mutated module, plus `lake build` only where a full-tree
  check is required. Classification per invocation: elaboration (per-module
  `lean`) vs build (`lake build`) stated per op, never mixed. Costs at
  measured layers: ≈11 s per check-elaboration (TraceTests class), 3–10 s per
  incremental rebuild (Build-2/3 class). Per-atom mutants needed: 14
  inversion + ≈12 vote + ≈10 substrate + final acceptance 2 = ≈38 build-class
  invocations worst case; helper rows cost $0 (done, §4). Presented as an
  unfunded envelope, not a grant; batching savings unsubtracted.
- OP-17 footprint re-binding where artifacts are absent: NO argv exists that
  establishes past footprints — recorded here as an irreducible explicit
  finding (UNESTABLISHED rows), not as deferred execution.

## §6. Irreducible gaps (exact; ungranted)

- G-B1: OP-10 as above (1 build + 1 elaboration). The ONLY execution that
  static work cannot replace.
- Past-provenance gaps (t62 KILLED rows, t54-auditor R-rows, t59/Haskell/
  simulator UNUSABLE rows): no execution closes them — stated as permanent
  explicit findings, not as work to schedule.
- Cold-log loss: permanent, stated, not re-run.

## Reconciliation (no coverage claimed)

- `DISCOVERED`: 239 qualified + 27 modules + axis. `REQUIRED-INPUT`: same 239,
  zero exemptions, relevant pairs only, DEFERRED rows visible.
- `EXECUTED`/`KILLED`/`SURVIVED`/`BLOCKED`: not claimed. Build-2 RED stays
  costing evidence only. Fixture REDs (§2) establish check sensitivity only.
- Every identity: kill pending (unfunded §5), helper-satisfiability exhibited
  (§4), or DEFERRED with reason (§3). Nothing absorbed, covered, or dropped.

*End of R4. History preserved (original + R2 + R3). Packet: this file +
`P1A-qualified-inventory.txt` (`efdeb3…`) + `P1A-qualified-classified.txt`
(`ef93b9…`) + prior retained evidence. Auditor of another family inherits
nothing else.*
