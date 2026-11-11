# Commit Audit

- Submission: 1/2
- Base: 5d4d09ea0b7c2a82e121c77e8903c62b29a18560
- Candidate: 36666dc44ccc4d3a0bb041b95eabf4526f15a77e (GPG good, DSA 20B19354…, parent=base, commit-gate OK)
- Mandate: specs/71-design-record at 5d4d09e (spec 5b5edbf6…, plan ad4342cd…, tasks 936f57f4…, models included bytes=12609 lines=211)
- Scope: FULL `5d4d09e..36666dc`
- Verdict: FINDINGS
- Audit loop: submission `1/2`; next submission `ALLOWED`
- Ceiling raises: `0/2`; ledger `handoffs/CAMPAIGN-LEDGER.md`
- Campaign: CLOSED — ended by SET-POINT
- Builds: `1/3` this ticket; this audit `1`, `cache=cold` at start (lean/.lake absent), nix store possibly warm

Path fence: 1 file `docs/en/design/state-machine.md` +476/−170 mode 100644 blob 3fe458dd. `kelgroups-vote-machine.md` blob 27317c8 identical to base. `lean/**` identical to pin e6c59242. specs/ unchanged vs candidate (planning lives on base). GREEN.diff = 5d4d09e..2eefbed; GREEN-2.diff = 2eefbed..36666dc. Gate v2 sha256=0781cfd50b3cc96965d5a2127ba86eafa63ca995fef6bdfd3a7512f4201d9e1a copied ignored into audit worktree; not edited.

## Invariant matrix

| Invariant | Severity | Verdict | Row state | Proof / evidence |
|---|---|---|---|---|
| R71-01 | BLOCKING | PASS | KILLED | State is conti/casse/collections/votes at State.lean:23-30; no `users`/`responsabili` table rows; GroupView present. M-01-USERS red. Legs 2. |
| R71-02 | BLOCKING | PASS | KILLED | Event 14 / AppEvent 17 derived; retired names namespaced. M-02-15 red. Leg 3. |
| R71-03 | BLOCKING | PASS | KILLED | Route 11/0/3 in Composition.lean:45; unpopulated baseEnacted stated; viewed step signatures; sealed baseHook three arms. M-03-NOAPP/NOBASE/STALESTEP red. Legs 4-5. |
| R71-04 | BLOCKING | PASS | BLOCKED | Per-event table matches Step.lean (pledge `isResponsabile` :94-100; correctPledge referente-only :118-122); tension + V-2/pledge-agency as ruled-not-implemented present. Distinctive honesty-paragraph mutant M-04-NOTENSION survives the gate. |
| R71-05 | BLOCKING | PASS | KILLED | Law vs finite-witness section + 29-name not-a-census + majority_table + root State/Event C7 note present. F-05-SCRUB → WITNESS-RED. Section-only mutant survives on leftover `witnessed by`. 29-of-224 without caveat still greens while `majority_table` token remains (F-05-29KEEP). |
| R71-06 | BLOCKING | PASS | KILLED | PROVED-IN-MODEL; Composition imports Types/Fold/Invariants/Vote.Fold not Step; only library aggregator `import Reactivegas.Composition` (plus TraceTests umbrella mention). Unbound reachability/target/polarity stated. M-06-NOCOMP red; unbound-block mutant survives. Delete-file build not spent (static import graph only). |
| R71-07 | BLOCKING | PASS | KILLED | Renounce accept-and-no-op unfinished vs V-5; notDesignee/notProposer declared-only (Validate.lean:41-42, zero constructions); VOTE_TRACES_V1 vs #74; θ open with exhibits. M-07-NOVOTE red; F-07-THETAONLY survives. |
| R71-08 | BLOCKING | FAIL | KILLED | Five-part Voci non-goal present and M-08-NOVOCI red, but the **fact count is truncated**: record says “twenty Voci/ modules”; `git ls-tree e6c59242 -- Voci/` is **21** paths; `Voci/Quantita.hs` blob 5464eb6 and `Voci/Quantità.hs` blob 0c3db8b both exist and differ (46-line diff). RELIANCE.md “Quantità variant absent” is false. Spec named 21. Property class: a count over a silently shortened discovered extent. |
| R71-09 | BLOCKING | PASS | KILLED | Dates 2026-08-26/27 and 2026-09-05; V-1..V-7, Q-001, NOTE-016, A-Q001, NOTE-031; “Later rulings supersede earlier”. M-09-NODATES red. Leg 10. |
| R71-10 | BLOCKING | PASS | BLOCKED | Pending table has #66 S1 / #68 / #69 / V-5 lifecycle (#81) plus reconciliation hook. M-10-NOTABLE, M-10-NOV5, M-10-NOHOOK all GREEN — shipped gate has no pending-table predicate. |
| R71-11 | BLOCKING | FAIL | KILLED | 80 `lean:` markers in state-machine.md resolve at e6c59242 (0 unresolved); kelgroups-vote-machine.md independently 30/30 (gate does not scan it). M-11-BOGUS unknown-in-docs → CITE-RED. **Missing/malformed controls do not RED:** F-11-EMPTYPREFIX strips `lean:` prefixes, mcount=0, GREEN; F-11-BANG `lean:!!not-a-symbol` dropped by discovery regex, GREEN; gate 11(a) never feeds the mutated copy to the resolver (VACUOUS). Fail-closed missing-Lean (11b) holds. Property class: a discovery quantifier that greens on the empty set, and a negative control that never applies the resolver to the mutated subject. |
| R71-12 | BLOCKING | PASS | KILLED | `canCloseGroup` Predicates.lean:85, usages=1 (orphan def); three conjuncts; verdict missing-guarantee; no invented theorem. M-12-NOCLOSE and M-12-INVENT red. Leg 6. Post-build v2 count still 1 with `.lake` present. |

## Failure modes altered

none altered -- checked: (a) `git diff --stat 5d4d09e..36666dc` = 1 docs file, `lean/**` blob-identical to pin e6c59242; (b) v2 vs v1 changes only the two source-discovery greps (`--include='*.lean' --exclude-dir=.lake` on canCloseGroup and citation resolve); documentary legs 2-5, 7-10 unchanged; v2 exclude holds on synthetic `.lake` and on the real post-leg-12 tree (v2=1, v1-shaped=2); (c) no consumer-facing contract file outside `docs/en/design/` moved.

## Residuals

None. BLOCKING rows are not terminated as RESIDUAL.

## Candidate invariants

- INV-71-NONEMPTY-MARKERS — discovered `lean:` marker count must be > 0 or the citation leg REDs. Proposed severity BLOCKING (R71-11 missing-citation). Unguarded: F-11-EMPTYPREFIX GREEN mcount=0.
- INV-71-RESOLVER-FED-CONTROL — unknown-marker negative control must run the resolver against the mutated docs copy. Proposed severity BLOCKING. Unguarded: gate 11(a) greps Lean for the injected name and never resolves the copy.
- INV-71-MARKER-NAMESPACE — `lean:` resolution must bind the namespaced symbol, not the trailing base name. Proposed severity ADVISORY. Unguarded: `lean:Proposal` matches 2 (`KelGroups.Event` first; prose describes `Reactivegas.Types.Proposal` departure/changeRoles).

Unratified; not used as audit rows.

## Onward discoveries — outside this ticket

- Gate v2 still prints `GATE-v1-GREEN`. Cosmetic. RECORDED, NOT-OPENED. Owner: ticket owner / gate maintainer. Follow-up: none filed.
- functions-model `freshness_pins` (blob/line mismatch RED) is not implemented in the gate; markers are name-only. RECORDED, NOT-OPENED. Owner: ticket owner.
- DOC_VM citations are not in leg 11; independently 30/30 at e6c59242 and byte-identical to base. RECORDED, NOT-OPENED. Owner: ticket owner.

## Blocking findings

1. [R71-08] `docs/en/design/state-machine.md:438` — Voci fact states “twenty Voci/ modules”; discovered extent at pin e6c59242 is 21 paths under `Voci/`, including distinct blobs `Voci/Quantita.hs` (5464eb6) and `Voci/Quantità.hs` (0c3db8b). RELIANCE.md claimed Quantità absent. **Property class:** a count computed over a silently shortened discovered extent is a lower bound wearing the denominator's name. Evidence: `git ls-tree e6c59242 -- Voci/` (21); campaign does not need a mutant for the count (content falsehood). Honest limit: categories listed in parentheses were not re-derived as a second census.

2. [R71-11] `gate.sh` v2 leg 11 — missing and malformed citations do not RED; 11(a) is vacuous. F-11-EMPTYPREFIX GREEN mcount=0; F-11-BANG GREEN; 11(a) never feeds the mutated copy to 11(c). Unknown-in-docs does RED (M-11-BOGUS). Fail-closed missing-Lean (11b) holds. Cold-tree citation discovery on legs 0-11 held (80 resolve, `.lake` absent). **Property class:** a discovery quantifier that greens on the empty set, and a negative control that never applies the checker to the mutated subject. Evidence: `evidence/campaign/followup.log` sha256=80f767289a6aac7b7290cbe17af1d0cd768f92e3835d7be57c682568569e454f; `evidence/campaign/run-mutants.sh` sha256=ac2065039775870e9cb573e5888fd89ca6a20ed0608a61390f50177dc10373a4. Honest limit: 80 live markers on this candidate do resolve; the hole is the checker, which this docs-only seat cannot edit.

## Verification receipts

| Command | Exit | Duration | Evidence |
|---|---:|---:|---|
| `evidence/gate-legs-0-11.sh` (v2 legs 0-11, cold `.lake`) | 0 | 474 ms | `evidence/gate-legs-0-11.log` sha256=4836921c76855134f8a583b7e0dcfbfcb9c95dadd81328aeab9eebd7cc633f7a bytes=1185 lines=30 |
| `./gate.sh` v2 legs 0-12 (`just ci`) | 0 | 134377 ms | `evidence/gate-full.log` sha256=f1ae16616d62483b2d99abf685794a3fab08c9faa25a655faee8dbfa93f588ea bytes=643279 lines=15229 command_sha256=90cdeaf82d17720bd8bfcf9c43179af1f84d1459aeddcc4784d4f72dd4b3d88a |
| `scripts/commit-gate 36666dc` | 0 | — | stdout `OK` |
| `campaign/run-mutants.sh` + `followup.sh` | 0 | — | results.tsv 20e183b1…; followup.log 80f76728… |
| `campaign/lake-exclude-probe.sh` | 0 | — | lake-exclude-probe.log 1e4027ec… |
| `campaign/resolve-markers.sh` SM+VM | 0 | — | sm-resolve.tsv TOTAL 80 UNRESOLVED 0; vm-resolve.tsv TOTAL 30 UNRESOLVED 0 |

## Advisories

- Frozen instruments (read-only seed): `evidence/gate-legs-0-11.sh` sha256=b4a3710d…; `evidence/campaign/run-mutants.sh` sha256=ac206503…; `evidence/campaign/followup.sh` sha256=d933d6ea…; `evidence/campaign/lake-exclude-probe.sh` sha256=39aa4a91…; `evidence/campaign/resolve-markers.sh` sha256=45166c07….
- R71-04 and R71-10 candidate content holds; campaign rows BLOCKED because the shipped gate cannot kill those distinctive classes. Not findings against the prose.
- Marker kind `{law,witness,…}` from data-model is prose headings, not structured marker fields.
