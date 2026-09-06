# S3 Phase 1 report — REVISION R5 (one completed static assessment)

Local only. No push, PR action, comment, gist, publication, deployment or merge.
No `docs/en/design/` writes. No coverage claim. No Phase-2/build/test/
elaboration/auditor grant exists; static assessment only. Spend stays **4
against ceiling 3** (explicit overrun, no refund). Mandate not narrowed: 239
qualified source identities are the floor; the environment extent is
unenumerated (G-B1, the only execution static work cannot replace).

- History preserved: original (`dbc2cb68…`), R2 (`32c06530…`), R3 (`9b5bf6c2…`),
  R4 (`5216adc3…`). R4 content stands except where this revision corrects it
  (t48/t57 per-receipt relation, solvent/alias static completion, OP plan).
- Base/seat/contract: HEAD `3590c0015b84fd58004bf6fb44dd18b107304c48`, #66 OPEN,
  seat PID/PGID `2401092`, contract rev 3. Worktree clean at R5.

## §1. Per-receipt relation — shown per receipt, or demoted with record

Rule applied: a changed checker/file total settles a receipt ONLY through the
hunks governing that receipt's subject. Otherwise the row is UNESTABLISHED-
REUSE with its bounded-retrieval record. Context-change statements (D1–D5 as
retrieved in R4) are kept and cited, but attached per receipt only through
named hunks.

**t48 (files 31–34, 29; candidates `4898e55e`/`a408e09`).** Component diff of
`scripts/check-reactivegas-inversion-coverage` against base: REMOVED
`expectedDeclarations := 163`, `projectRoots` whitelist, `isProjectModule`,
`declaredIn`; ADDED `canonRoot`/`artifactInsideRoot`/`resolveProjectModules`,
`theoremDeclsOf`+`isPrivateDecl`, `claimedCtor`; UNCHANGED
`successOf`/`coveredBy`/`tightnessProved`/`compareCoverage`/`eventCtors`/
`isInversionName` (no ±def hunks), `requiredInversions` (identical 6 names).

| row | per-receipt evidence | marking |
|---|---|---|
| SURFACE (binding rule) | ownership (`projectRoots`→provenance) + declaration-parsing (`declaredIn`→parser) hunks govern exactly this row's mechanism | STALE-DEMONSTRATED |
| GUARDS (live guard/successor) | governing core UNCHANGED (no hunks); ownership layer around it changed, which is not this row's relevant evidence | **demoted to UNESTABLISHED-REUSE** (retrieval: hunk diff clean for the guard core; command unbound in ledger) |
| CANFAIL s2-KILLED | retrieved subjects: live-inversion removal + checker-unwiring controls (s2 audit report); `projectConstants` signature hunks govern the removal control | STALE-DEMONSTRATED |
| AXIOMS s2-KILLED | retrieved subjects: disguised custom-opaque + `admit` controls, six exact `[propext]` reports; `scripts/check-lean-axioms` ABSENT at `a408e09` (verified) — instrument replacement demonstrated | STALE-DEMONSTRATED |
| REGRESSION | `expectedDeclarations 163` removed; current `scripts/` has no `163`/quota (verified) — the recorded instrument output has no counterpart | STALE-DEMONSTRATED (instrument retired) |
| FENCE | NO fence/forbidden hunks in diff; command unbound | **demoted to UNESTABLISHED-REUSE** |
| EVENT-SYNTAX | declaration-parsing rewrite hunks govern this row | STALE-DEMONSTRATED |
| STEP-ITE / STEPEVENT-DELEGATE | subject proof file `Reactivegas/Invariants.lean` changed 28+/20− (the unfolding lines live there) | STALE-DEMONSTRATED |
| INV-HYP-SYNTAX | parser rewrite hunks govern hypothesis spelling checks | STALE-DEMONSTRATED |
| s1 rows (`4898e55e`, whose rig shares NONE of the s2 component vocabulary — 0 marker hits) | whole-rig lineage replacement (467-line diff) governs; row→component mapping inside the s1 rig unrecoverable from prose | STALE-DEMONSTRATED on lineage replacement, with the mapping caveat stated |
| File 31 recut1 (6 killed controls) | same checker lineage as s2 | STALE-DEMONSTRATED |

**t54 (files 26/27/30; lineage `757dac98`; function-level diffs):**
PARTITION/FRANCHISE stay FIXTURE + D5 as retrieved in R4. DISJOINT: 13 changed
lines mention `placeBallot` (the erased-tally site) → STALE-DEMONSTRATED.
NOSTALE: 45/29 changed lines mention `sweepClosures`/`sweepStep` → STALE-
DEMONSTRATED. POLICYFREE: `verdictOf` 49 + `effectedState` 47 changed lines in
the threshold plumbing (the `legacyThreshold` def itself has an empty diff —
stated); → STALE-DEMONSTRATED via the plumbing hunks. NOEXPIRY: D5 + scope kept.

**t57 (file 12): CORRECTED — all 10 rows demoted to UNESTABLISHED-REUSE.**
Retrieval record: gate base `bb3ac41` + toolchain 4.25.0 bound; auditor
build-ledger/probe/gate logs located; per-row mutant↔source pins sought and NOT
found (prose-level descriptions only). The D4 substrate/vote re-cut (1164+/736−
across 7 files) is stated separately as demonstrated context change and is NOT
attached to any individual receipt. Fresh runs need new instruments (§4
OP-39..44); nothing is claimed stale per receipt.

**t62, all other files:** R4 dispositions stand (KILLED→UNESTABLISHED-REUSE with
`3a7b355a` absence; OPEN rows as open obligations; non-Lean subjects UNUSABLE
for D2). **REUSABLE-BOUNDED: 0.**

## §2. Solvent/alias static mapping — finished (execution remainder separated)

*Aliases (7 pairs, bodies verified):* all 7 call-through bodies verified
(`KelGroups.approvals_nodup h entry hentry`, …,
`KelGroups.majority_not_strict_on_even gs positive even`). Signatures differ
(section variables vs explicit `{α : Type}` + qualified types). Static
conclusion: dependence root→counterpart SHOWN (call bodies) — aliases inherit
counterpart atoms; the differing statement text is each alias's own thin
surface (kill-independence test: OP-32..38). Both spellings stay listed.

*Solvent family (proof dependencies read, not names):*
`comune_not_a_member_step` ignores its step hypothesis (`_hstep`, returns `h` —
body read) → owns NO guard atom. `comune_not_a_member_of_reach` is Reach
induction (boot/trans, bodies read) → owned by B-admit (direct-admission
refusal of `comuneId`, `Validate.lean:142`) + boot (`Step.lean:362` refusing
comune aggregates); no economic guard writes membership (T6222 vocabulary
verified). `credit_pledges_step` (`:890`) has 18 match arms incl. per-
constructor fund equations (openPurchase arm read) → owned by the 14 economic
guard/effect atoms (vote-passthrough arms vacuous: `step` returns `none`).
`solvent_preserved` (`:1164`, body read) = `credit_pledges_step` ∧ comune limb
→ 14 economic atoms. `reach_solvent` (`:1177`, body read) = `solvent_init` +
`solvent_preserved` → boot + 14 atoms. `not_insolvent_of_reach` = reach_solvent
+ omega → same. `credit_pledges_of_reach` (private) = Reach induction → boot +
credit_pledges_step atoms. **No DEFERRED solvent rows remain on mapping
grounds**; the execution remainder (kills) is §4 work, explicitly separated.

## §3. Numbered operation plan — named drivers, targets, per-op costs (ALL UNGRANTED)

Methods (exact; layer stated per use, never mixed). M-elab (s1 precedent, read
in `franchise-policy-mutants.sh`): setup `cp -a <base>/lean/.lake/build/lib/lean <run>/lib` + copy target module to `<run>/src/...`; argv `(cd <run>/src && LEAN_PATH=<run>/lib:<base>/lean/.lake/build/lib/lean lean <Module/Path>.lean -o <run>/lib/<Module/Path>.olean)`; expected: exit≠0 naming the owning theorem; cost class elaboration, ≤10 s upper-bounded by measured Build-2 incremental rebuild (per-module `lean` expected faster, unmeasured — stated). M-build: `PATH=<lean4-4.25.0>/bin:$PATH lake build <Module>` (cwd `<base>/lean`); cost class build, 3–19 s measured. `<base>` = `/code/reactivegas-66-s3-phase1`; `<run>` = `/tmp/s3p2/<OP>` (concrete, no placeholders).

| op | requirement (qualified identities) | target (file:line + atom, at base) | driver + expected observable | cost |
|---|---|---|---|---|
| OP-11..24 (14 ops, one per row) | each `step_*_inv` (8 root + 6 `Reactivegas.*`) | `Step.lean` arm guards `:47` openPurchase, `:52` grantPermission, `:56` denyPermission, `:62` deposit, `:67` withdraw, `:74` transferCassa, `:79` donate, `:85` backdonate, `:94` pledge, `:104` acceptPledge, `:111` refusePledge, `:118` correctPledge, `:126` closePurchase, `:133` failPurchase — mutant schema per arm: weaken the arm's guard to admit exactly one more case (donate form demonstrated: `decide (0 < v)`→`decide (0 < v + 1)`) | M-elab on `Reactivegas/Invariants.lean`; RED naming the owning theorem (Build-2 showed `:407` form) | elaboration ≤10 s each |
| OP-25..31 (7) | solvent rows (map §2) | `credit_pledges_step` arms (`:890+`, fund equations) for preserved/reach/insolvent/credit rows; `Validate.lean:142` + `Step.lean:362` (B-admit/boot) for comune rows | M-elab on `Reactivegas/Invariants.lean`; RED naming the row | elaboration ≤10 s each |
| OP-32..38 (7) | 7 root aliases + counterparts | root statements `:877,883,889,899,909,914,923` + counterparts `:312,317,342,379,374,450,459` — mutant schema: break the alias statement/dependence without touching the counterpart body | M-elab on `KelGroups/Invariants.lean`; kill-independence observed both directions | elaboration ≤10 s each |
| OP-39..44 (6) | DISJOINT/NOSTALE/POLICYFREE + t57 vote rows | `placeBallot` erase site / sweep filter / threshold plumbing (`Vote/Fold.lean`, `Vote/Invariants.lean`) + t57 effect/sweep/bypass sites in the same files (atoms per R3 annex; exact lines re-read at execution time from base — stated, not hidden) | M-elab per mutated module; RED naming the row | elaboration ≤10 s each |
| OP-46..55 (10) | remaining vote authored rows (≈31: preservation/sweep/tally/qid/verdict families) | grouped by owning atom (V-sweep/V-tally/V-franchise/V-threshold/V-qid) into one admitted single-atom mutant per atom in `Vote/Fold.lean` / `Vote/Invariants.lean` / `Vote/State.lean` | M-elab per mutated module; each row's property-specific failure observed (one atom may close several rows only so observed — contract rule) | elaboration ≤10 s each |
| OP-56..65 (10) | remaining substrate authored rows (≈24 preservation/admission/hook + root non-alias rows) | grouped by owning atom (B-propose/B-approve/B-admit/B-mutate/B-hook/W-coherence) in `KelGroups/Fold.lean` / `Validate.lean` / `Integration.lean` / `Invariants.lean` | M-elab per mutated module; RED naming each row | elaboration ≤10 s each |
| OP-66..70 (5) | witness rows (≈30 `checkX`/mirrors/frozen) | owning files (`Reactivegas/Invariants.lean`, `Step.lean`, `TraceTests.lean`, `Trace.lean`, `Composition.lean`) — re-elaboration + surrounding-check sensitivity per `*_mutant_caught` pair | `lake env lean <file>`; GREEN-with-`true` + mutant-catch demonstration | elaboration ≤11 s each (TraceTests class) |
| OP-45 | final acceptance | full tree | M-build `lake build` + `lake env lean Reactivegas/TraceTests.lean` + `lake env lean Reactivegas/CorpusGate.lean` (exact argv, existing commands) | 1 build (≤19 s) + 2 elaborations (11 s + 2 s measured) |

Envelope arithmetic (unfunded, batching unsubtracted): 14+7+7(pairs)+6+10+10+5 = 59 elaborations + OP-45 (1 build + 2 elaborations). Every authored family has named ops; helper rows $0 (done §4/R3). Past-provenance permanents excluded (no execution closes them).

## §4. OP-10 identity emission — demonstrated from script + retained output

`scripts/check-lean-axioms` (read): line 236 `for m in proj do IO.println s!"axiom-module {m}"` and line 270 `for (_, n) in wDedup do IO.println s!"axiom-theorem {n}"` — the output CONTAINS the actual compiled identity set (per-module + per-distinct-theorem names), with counts at lines 233/258/283 and the `axiom-gate: ok` marker (291/315). No retained output at this base contains the full set (Build-1/2/3 logs carry only `#print axioms` fragments) — stated. No separate identity driver is required; the gate as shipped emits identities. OP-10 = the gate unmodified: 1 `lake build` over the 27 explicit S modules (targets named in `P1A-S-modules.txt`) + 1 `lake env lean` elaboration (subprocess count inspected, not claimed). Cost: 1 build + 1 elaboration. UNGRANTED.

## §5. Irreducible gaps (exact; ungranted)

- G-B1: OP-10 as above — the only execution static work cannot replace.
- Past-provenance permanents (t62 KILLED, t54-auditor R-rows, t59/Haskell/simulator rows): no execution closes them — permanent explicit findings.
- Cold-log loss: permanent, stated, never re-run.

## Reconciliation (no coverage claimed)

- `DISCOVERED`: 239 qualified + 27 modules + axis. `REQUIRED-INPUT`: same 239, zero exemptions, relevant pairs per annex + §2 (no Cartesian product). Solvent/alias DEFERRED labels REMOVED (mapping finished); remaining deferred: C002 + execution-side kills.
- `EXECUTED`/`KILLED`/`SURVIVED`/`BLOCKED`: not claimed. Build-2 RED and fixture REDs stay evidence of their own kind only.
- Every identity: kill pending (unfunded §3), helper-satisfiability exhibited (R3 §4 corrected §1), or execution-side pending with named op (§3). Nothing absorbed, covered, or dropped.

*End of R5. History preserved (original + R2 + R3 + R4). Packet: this file + `P1A-qualified-inventory.txt` (`efdeb3…`) + `P1A-qualified-classified.txt` (`ef93b9…`) + prior retained evidence. Auditor of another family inherits nothing else.*
