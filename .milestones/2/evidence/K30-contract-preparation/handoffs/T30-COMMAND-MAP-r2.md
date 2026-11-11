# T30-COMMAND-MAP-r2 — frozen requirement-to-command/control map (kelgroups #30)

Companion to `T30-CONTRACT-r2.md` §7 (same frozen content, row-addressable).
Preparation owner `t30-contract`, preparation-only (spend 0; commands NAMED,
never executed). Base kelgroups `main` @ `933e385d`; Lean @ `3590c001`
(zero-diff EMPTY vs `4a6cd87` on Vote + Integration/State/Validate).
SUPERSEDES `T30-COMMAND-MAP-r1.md` (retained); D1–D7 applied per parent
assessment `T30-R1-ASSESSMENT.md`.

Predicates (D6): COMPILER-kill = exit≠0 + diagnostic quotes ctor/site +
zero parse-error lines. TEST-kill = exit≠0 + `Failures:` names ≥1
registered §7-REQ example of the row. GREEN-ENUM (M10b) = exit 0 +
enumeration lists every allowed ctor. Setup/infra/crash/timeout/parse =
INCONCLUSIVE abort, never a kill. Hidden invocations forbidden: every cited
result maps to a B-row or PROBE-row. Charge-0 recon (reads, greps incl. D7
tripwire, `git status/diff/rev-parse/log`, `gh issue view`, `--version`):
free, never evidence.

Exact commands (§7-CMDS): `nix develop .#ci --quiet -c just build`;
`nix develop .#ci --quiet -c cabal test all -O0 --test-show-details=direct`;
`nix develop .#ci --quiet -c just ci`; probes
`nix develop .#ci --quiet -c cabal test invariants --test-option=--match
--test-option=/S30-<Group>/<REQ-ID>/` with REQ-ID from §7-REQ.

## Required examples (§7-REQ)

REQ-OPEN-COLL, REQ-OPEN-PERM, REQ-OPEN-REFUSE, REQ-OPEN-DUP,
REQ-CAST-ASSENT, REQ-CAST-SWITCH, REQ-CAST-POSTSWITCH, REQ-CAST-RECAST,
REQ-CAST-UNKNOWN, REQ-CAST-NONRESP, REQ-SWEEP-TALLY, REQ-SWEEP-DISSENT,
REQ-SWEEP-FRANCHISE, REQ-RETAIN, REQ-NOEXPIRY, REQ-VERDICT-COLL,
REQ-VERDICT-PERM, REQ-FRANCHISE-CURRENT, REQ-NEG-DELIVER, REQ-ROUTE-ENUM,
REQ-HOOK-EXT, REQ-RECORD-SHAPE, REQ-CLIENT-ROUNDTRIP, REQ-ADMIT-PATH,
REQ-NONDECIDE-PERM. Gate cross-check: every ID registered + executed, else
RED. B20 (one ID removed → RED) is the ONLY truncation-guard falsification;
C1/C2 RED is absence-only.

## Rows (obligation → owner command → auditor command)

- MAP-RED: B1 build-absence RED + B2 test-absence RED (BUILD ×2, absence
  ONLY) | A2/A3 rerun + attribution review.
- MAP-R30-1: B4 shared + probes | A3 re-run; reviews.
- MAP-R30-2: B4 + probes; B6 M2, B7 M3, B8 M4a (erase-drop →
  REQ-CAST-SWITCH RED; REQ-CAST-RECAST-passing = challenge history, never
  kill), B9 M4b (unguarded-insert → REQ-CAST-RECAST RED; POSTSWITCH vs
  RECAST distinguished) | A3; ≤5 kill re-runs incl. M4b when disputed.
- MAP-R30-3: B4 shared (Store append + replay); B10 M5, B11 M6
  (REQ-NEG-DELIVER), B12 M7, B14 M9 (REQ-RETAIN) | A3 + A10 boundary rerun.
- MAP-R30-4: B4; B13 M8 (REQ-VERDICT-PERM) | A3.
- MAP-R30-5: B3 cold build (4-ctor vocabulary + 3-arm exhaustive compiles;
  zero producing sites) | A2; tripwire reviewed, never a kill.
- MAP-R30-6: B4; B18 M13 franchise-snapshot-in-payload →
  REQ-FRANCHISE-CURRENT RED | A3.
- MAP-R30-7/14: B4 at integrated boundary + persistence roundtrip (M6
  quotes REQ-NEG-DELIVER) | A3 + A10.
- MAP-R30-8: B3 (2-arm enactment); B15 M10a admission-ctor-added →
  COMPILER-kill (narrow interface-existence ONLY); M10b GREEN-ENUM in B4
  (named sub-claim: every ALLOWED ctor encodable + enacted; interface
  proposition) | A2; M10a re-run when disputed; M10b re-checked in A3.
- MAP-R30-9: freeze on current base (B3/B4); conditional rebind (A11 iff
  `paolino/reactivegas#68` landed) | A11 (else unspent with reason).
- MAP-R30-10 (mechanism surface ONLY, D1): B4 (hook runs with exact
  pre/post views; 4-cause record shape as DATA; append-only retention;
  atomic discard); B16 M11 hook-ignored → REQ-HOOK-EXT restoration RED;
  B19 M14 tally/franchiseChange collapse → cause-distinction RED (carried
  causes excluded) | A3; reviews.
- MAP-L-1–L-7: recorded with owners (#81; L-7 gated on #76). NO command —
  explicitly not established here | reviewed as record only.
- MAP-R30-10U/PROD: no command (preserved boundary; tripwire recon) | —.
- MAP-R30-11: no wire, no mock | —.
- MAP-R30-12: B21 leg-6 (`spago build` + `spago test`, TEST-boundary
  roundtrip, limit stated); B17 M12 dropped-propose-path → REQ-CLIENT-
  ROUNDTRIP RED at client boundary | A4.
- MAP-R30-13: B21 `lake build` green only | A4.
- MAP-DRIFT (D2): frozen `lean-vote-ctors.inventory` +
  `hs-vote-matches.inventory` reconciliation; P-DRIFT-1 GREEN + P-DRIFT-2
  omission-RED (PROBE ×2); Haskell exhaustiveness = in-language evidence
  only | recon rerun (auditor PROBE ×1 + spot-check ×1).
- MAP-GUARD (D3): B20 omission-challenge rerun (BUILD ×1; ONLY guard
  falsification) | evidence review + A3 cross-check.
- MAP-COLD-FINAL: B3 cold 1B + B21 final `just ci` 1B + tracked-clean +
  `Trivial` presence-only + founding guard | A2/A4.
- MAP-SLIM: 3B identical-envelope (legs 1,2,2b,3,4,6,7 analog) | —.

## Fit (frozen)

Owner: B1–B2 RED (2) + B3 cold + B4 test + B5–B19 fifteen runs + M10b in B4
(named) + B20 omission + B21 CI = GREEN 19 + SLIM 3 = **24 builds**;
probes ≤24 (P-DRIFT-1/2 + per-REQ `--match` + narrowing). **PROPOSED owner
24/24** (supersedes 20/24; every added run itemized above).
Auditor: A1 recon 0B; A2 1B; A3 1B; A4 1B; A5–A9 ≤5 kill re-runs 5B
(frozen criteria: dispute, M4b/M10a/M11 risk order, rotation;
undisputed = EVIDENCE-REVIEW labeled); A10 2B; A11 1B (R30-9 conditional +
omission-review); A12 reserve 1B; drift rerun from probe cap. **PROPOSED
auditor 12/24.** Both PROPOSALS pending fit-proof at freeze +
authorization; gap returns exact cost, never trimmed scope. Discovery
bounds the SET (new ctor ⇒ new arm ⇒ new mutant), never row coverage.
