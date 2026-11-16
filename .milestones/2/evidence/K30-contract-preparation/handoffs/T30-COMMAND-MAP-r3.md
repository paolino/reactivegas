# T30-COMMAND-MAP-r3 — frozen requirement-to-command/control map (kelgroups #30)

Companion to `T30-CONTRACT-r3.md` §7 (same frozen content, row-addressable).
Preparation-only (spend 0; commands NAMED, never executed). Base `933e385d`;
Lean `3590c001` (zero-diff EMPTY vs `4a6cd87` on Vote + Integration/State/
Validate). SUPERSEDES r2 map (retained); G1–G5 applied — one principle:
every claim live-bound or explicitly limited.

Predicates: COMPILER-kill (exit≠0 + diagnostic quotes ctor/site + zero
parse-error lines); TEST-kill (exit≠0 + `Failures:` names ≥1 registered
REQ-ID); GREEN-ENUM (exit 0 + lists every allowed ctor). Setup/infra/crash/
timeout/parse = INCONCLUSIVE abort, never kill. Hidden invocations
forbidden. Charge-0 recon free, never evidence.

Exact commands: `nix develop .#ci --quiet -c just build`;
`nix develop .#ci --quiet -c cabal test all -O0 --test-show-details=direct`;
`nix develop .#ci --quiet -c just ci`; probes `nix develop .#ci --quiet -c
cabal test invariants --test-option=--match --test-option=/S30-<Group>/
<REQ-ID>/` with frozen Group slugs below (fully determined strings).

## REQ groups (frozen describe slugs — G3)

`S30-Open`: REQ-OPEN-COLL, REQ-OPEN-PERM, REQ-OPEN-REFUSE, REQ-OPEN-DUP.
`S30-Cast`: REQ-CAST-ASSENT, REQ-CAST-SWITCH, REQ-CAST-POSTSWITCH,
REQ-CAST-RECAST, REQ-CAST-UNKNOWN, REQ-CAST-NONRESP. `S30-Sweep`:
REQ-SWEEP-TALLY, REQ-SWEEP-DISSENT, REQ-SWEEP-FRANCHISE, REQ-RETAIN,
REQ-NOEXPIRY. `S30-Verdict`: REQ-VERDICT-COLL, REQ-VERDICT-PERM.
`S30-Franchise`: REQ-FRANCHISE-CURRENT. `S30-Negative`: REQ-NEG-DELIVER.
`S30-Route`: REQ-ROUTE-ENUM. `S30-Lifecycle`: REQ-HOOK-EXT,
REQ-RECORD-SHAPE. `S30-Client`: REQ-CLIENT-ROUNDTRIP. `S30-Admit`:
REQ-ADMIT-PATH, REQ-NONDECIDE-PERM. Cross-check: every ID registered +
executed else RED. B19 (one ID removed → RED) is the ONLY guard
falsification; C1/C2 absence-only.

## Drift (G1 live mechanism)

LIVE discovery over the real trees (paths pinned at freeze): Lean file-set
`ls` == frozen FILE LIST (Vote/{Types,State,Event,Validate,Fold}.lean +
{Integration,State,Validate}.lean); in-leg `git rev-parse HEAD` ==
`3590c001` binds the pin; item emission = type-declaration +
inductive-arm greps (exact forms §7) with per-file NON-EMPTY guards;
Haskell emission = type-declaration grep over `lib/KelGroups/Vote/*.hs` +
anchored wiring symbols; RECONCILIATION join (emitted⇒mapped;
mapping⇒resolves-live-both-sides). COMPLETENESS (file-set + non-empty) ≠
COVERAGE (join) — distinct legs. file:line = PROVENANCE only. Temporal hole
→ explicit-rebind process rule (re-freeze + re-demonstrate). Exhaustiveness
= in-language evidence only. REQUIRED probes: P-DRIFT-GREEN,
P-DRIFT-ADD (synthetic ctor → RED), P-DRIFT-SRCOMIT (dropped line → RED),
P-DRIFT-MAPOMIT (dropped mapping row → RED), P-DRIFT-FILEADD (added file →
RED) — on real output copies, never fixtures.

## Rows (obligation → owner → auditor; G2/G5)

- RED: B1 + B2 (absence ONLY) | A-RED1/A-RED2 frozen-BASE reruns (dual-use
  resolved; never the candidate calls).
- R30-1: B4 + probes | A-TEST; reviews.
- R30-2: B4; B6 M2; B7 M3; B8 M4a (signature SWITCH RED + POSTSWITCH RED
  co-effect + RECAST GREEN; criterion SWITCH REDs); B9 M4b (RECAST RED +
  SWITCH/POSTSWITCH GREEN) | A-TEST; A-K reruns.
- R30-3: B4 (append + replay); B10 M5; B11 M6 (REQ-NEG-DELIVER); B12
  M7merged (cause-forced-tally → REQ-SWEEP-FRANCHISE RED; site
  closureCause; carried causes excluded); B14 M9 | A-TEST + A-Ks; M6 rerun
  = named boundary rerun.
- R30-4: B4; B13 M8 | A-TEST.
- R30-5: B3 cold (vocabulary + exhaustive 3-arm) | A-COLD; tripwire review.
- R30-6: B4; B18 M13 snapshot → REQ-FRANCHISE-CURRENT RED | A-TEST; A-K13.
- R30-7/14: B4 integrated boundary + roundtrip | A-TEST; A-K6.
- R30-8 + public surface (G4): B3 (2-arm enactment); B15 M10a
  admission-ctor-added → COMPILER-kill = CLOSED-vocabulary totality (NOT
  impossibility-through-surface); three-part surface claim: (i) typed
  vocabulary = frozen review fact; (ii) translation totality = B3 + D2
  coverage (control: drift omissions); (iii) non-vacuity = M10b GREEN-ENUM
  named in B4; residual universal-impossibility UNENFORCEABLE-by-test →
  change-detection (D2 + tripwire → mandate review) | A-COLD; A-K10a when
  disputed; M10b re-checked in A-TEST.
- R30-9: current-base freeze (B3/B4) | A-REBIND iff #68 landed (author
  integrates + fresh final-SHA audit; auditor never repairs).
- R30-10 surface: B4 (hook exact pre/post; 4-cause DATA; append-only;
  atomic discard); B16 M11 hook-ignored → REQ-HOOK-EXT RED | A-TEST; A-K11.
- L-1–L-7: recorded with owners (#81; L-7 gated #76); NO command | record
  review only.
- R30-10U/PROD, R30-11: no command (preserved boundary; no wire/mock) | —.
- R30-12: B20 leg-6 (`spago build` + `spago test`, TEST-boundary, limit
  stated); B17 M12 → REQ-CLIENT-ROUNDTRIP RED | A-CI; A-K12.
- R30-13: B20 `lake build` green only | A-CI.
- Guard: B19 omission rerun (ONLY falsification) | A-OMIT rerun.
- Cold/final: B3 1B + B20 final CI 1B + tracked-clean + Trivial-only +
  founding guard | A-COLD/A-CI.
- SLIM: 3B (legs 1,2,2b,3,4,6,7 analog + drift GREEN probe) | —.

## Fit (frozen — G2/G3/Also-noted)

Owner builds: B1,B2 RED (2) + B3 cold + B4 test + B5–B18 fourteen runs +
B19 omission + B20 CI = GREEN 18 + SLIM 3 = **23**. Above-20: M4b←D4,
M13←D5, B19←D3 (each ordered). M10b in B4 (named); drift in probe cap;
M14 merged away (G5). Probes ≤24: 5 REQUIRED named (P-DRIFT-GREEN/ADD/
SRCOMIT/MAPOMIT/FILEADD) + discretionary kill-confirm ≤14 (ambiguous logs
only) + dispute ≤3 + transient ≤2, REQUIRED-first. **PROPOSED owner 23/24.**
Auditor: A-RED1/2 2B + A-COLD/A-TEST/A-CI 3B + A-K×14 14B + A-OMIT 1B +
A-REBIND conditional 1B + A-RESERVE 1B = 22B; probes 2 required (drift
rerun + spot-check) + finding-narrowing ≤14 + reconfirm ≤8 = 24.
**PROPOSED auditor 22/24** — bottom-up (G2); coverage never trimmed.
Both PROPOSALS pending fit-proof + authorization; gap returns exact cost,
never trimmed scope. Discovery bounds the SET, never coverage.
