# T30-COMMAND-MAP-r5 — frozen requirement-to-command/control map (kelgroups #30)

Companion to `T30-CONTRACT-r5.md` §7 (same frozen content, row-addressable).
Preparation-only (spend 0; commands NAMED, never executed). Base `933e385d`;
Lean `3590c001` (zero-diff EMPTY vs `4a6cd87`). SUPERSEDES r4 map (retained);
NOTE-006 applied — per-identity truth, layer-reaching controls, actual 7-file
extent, full-audit accountability, bounded surface, witness-following kills.

Predicates: COMPILER-kill (exit≠0 + diagnostic quotes ctor/site + zero
parse-error lines); TEST-kill (exit≠0 + `Failures:` names ≥1 registered
REQ-ID); GREEN-ENUM (exit 0 + lists every allowed ctor). Setup/infra/crash/
timeout/parse = INCONCLUSIVE abort, never kill. Hidden invocations
forbidden (leg-unit accounting). Charge-0 recon free, never evidence. BAN:
regex output never cited as semantic inventory, anywhere.

Exact commands: `nix develop .#ci --quiet -c just build`;
`nix develop .#ci --quiet -c cabal test all -O0 --test-show-details=direct`;
`nix develop .#ci --quiet -c just ci` (INCL kelgroups-own `just lean`
sub-step — tabulated); probes `nix develop .#ci --quiet -c cabal test
invariants --test-option=--match --test-option=/S30-<Group>/<REQ-ID>/`
(Groups frozen below — fully determined). Reactivegas-side (read-only — NO
lake build): `git rev-parse HEAD`, per-file `git show HEAD:<path> |
sha256sum`, `git status --porcelain`, `git ls-files`, `git archive`
(each rides its leg's/probe's counter).

## REQ groups + IDs (26, frozen)

`S30-Open`: REQ-OPEN-COLL, REQ-OPEN-PERM, REQ-OPEN-REFUSE, REQ-OPEN-DUP.
`S30-Cast`: REQ-CAST-ASSENT, REQ-CAST-SWITCH, REQ-CAST-POSTSWITCH,
REQ-CAST-RECAST, REQ-CAST-UNKNOWN, REQ-CAST-NONRESP. `S30-Sweep`:
REQ-SWEEP-TALLY, REQ-SWEEP-DISSENT, REQ-SWEEP-FRANCHISE, REQ-RETAIN,
REQ-NOEXPIRY, REQ-SWEEP-IDEM (new: double-sweep stable, no duplicates —
Lean sweepClosures_idempotent + sweepDuplicating transcribed). `S30-Verdict`:
REQ-VERDICT-COLL, REQ-VERDICT-PERM. `S30-Franchise`:
REQ-FRANCHISE-CURRENT. `S30-Negative`: REQ-NEG-DELIVER. `S30-Route`:
REQ-ROUTE-ENUM. `S30-Lifecycle`: REQ-HOOK-EXT, REQ-RECORD-SHAPE.
`S30-Client`: REQ-CLIENT-ROUNDTRIP. `S30-Admit`: REQ-ADMIT-PATH,
REQ-NONDECIDE-PERM. Every ID registered + executed else RED. B20 (one ID
removed → RED) is the ONLY guard falsification; C1/C2 absence-only.

## Extent + per-identity rule (actual 7-file Vote dir, all read in full)

Event, Fold, Invariants, State, Tests, Types, Validate. Types/Event/
Validate: inductives → MUST emit ctor rows. State/Fold: structures +
equation groups → MUST emit. Invariants: theorems/Props/helpers/one named
mutant → expected-empty (proof-only; rows in identity table with exclusion
reasons; NO Haskell requirement invented). Tests: fixtures/builders/
witnesses/guards/examples → expected-empty (witness-only; rows tabled;
review-corroboration column, never kills). Other file empty → RED.
Empty-global → RED always. Full rows: `T30-IDENTITY-MAP-r5.md` (rule
subject: ctor/arm identities + equation groups — stated).

## Drift machinery (oracles named; script: `T30-DRIFT-LEG-r5.sh`)

L1 binding (sha256sum + git): immutable-view hashes + HEAD pins (both
repos) + file-set + mapping self-check + labeled clean sample (residual
race stated). L2-as-execution DROPPED (upstream owns pinned-commit
validity). L3 join: frozen mapping ⨝ live Haskell emission + leg-4 log
(review prompts never evidence). L4 .hi tripwire (GHC oracle): post-exit-0
emission per frozen-module row (Vote.* + `KelGroups.Event` for
BaseMutation/BaseChange — selector fix, verified site) + freshness-marker
rule (stale refused) + hash-pin + diff→review; firing demo rides B22.
L5 arm totality (GHC -Werror, live; demonstrated via M10a's break in B15's
log — the break only, never `.hi`). Function presence = compilation +
REQ-execution. Baseline = review + signed record (enforced:REVIEW).
Temporal hole → explicit-rebind rule. REQUIRED probes: P-DRIFT-GREEN,
P-DRIFT-ADDBYTE, P-DRIFT-DELBYTE, P-DRIFT-ADDFILE, P-DRIFT-JOINMAP
(mapping-copy − row vs LIVE B3 emission). Overlays = archive export + ONE
edit, export-diff bound — trigger discrimination on source-shaped bytes,
NOT review correctness. Output-copy controls DELETED. Item attribution =
enforced:NONE automatic + MANDATORY re-review (RED on ANY mismatch until
signed).

## Rows (obligation → owner → auditor)

- RED: B1 + B2 (absence ONLY; Vote-absence re-verified at r5) |
  A-RED1/A-RED2 frozen-BASE reruns.
- R30-1: B4 + probes | A-TEST; reviews.
- R30-2: B4; B6 M2; B7 M3; B8 M4a (SWITCH REDs); B9 M4b (RECAST REDs,
  inverts source-verified guard) + classification of extras (POSTSWITCH
  REQUIRED; observed = freeze characterization) | A-TEST; A-K unconditional.
- R30-3: B4 (append + replay); B10 M5; B11 M6; B12 M7merged; B14 M9;
  B19 M15 sweep-without-removal → REQ-SWEEP-IDEM RED (filter-drop site vs
  M9's append-drop; duplication vs retention; Lean's own mutant shape) |
  A-TEST + A-Ks; M6 = named boundary rerun.
- R30-4: B4; B13 M8 | A-TEST.
- R30-5: B3 cold | A-COLD; tripwire review (never kill).
- R30-6: B4; B18 M13 → REQ-FRANCHISE-CURRENT RED (post-view = sensitivity
  fixture, NOT produced transition — limit stated, Tests.lean caveat
  carried) | A-TEST; A-K13.
- R30-7/14: B4 boundary + roundtrip | A-TEST; A-K6.
- R30-8 + bounded surface: B3; B15 M10a admission-ctor-added to
  BaseMutation (`KelGroups/Event.lean` site — COMPILER/CLOSED-totality);
  M10b GREEN-ENUM named in B4 (totality-witness `case`); translation
  control = D2 join + witness case (zero new builds); review-only
  remainder named | A-COLD; A-K10a UNCONDITIONAL (stale conditional
  deleted); M10b re-checked in A-TEST.
- R30-9: current-base freeze | A-REBIND iff #68 landed (author integrates
  + fresh final-SHA audit; auditor never repairs).
- R30-10 surface: B4; B16 M11 → REQ-HOOK-EXT RED | A-TEST; A-K11.
- Produced-cause distinction: B12 M7merged (forced-.tally →
  REQ-SWEEP-FRANCHISE RED; carried excluded) | A-K7.
- L-1–L-7: recorded with owners; NO command | record-only review.
- R30-10U/PROD, R30-11: no command | —.
- R30-12: B21 leg-6 (TEST-boundary, limit stated); B17 M12 → ROUNDTRIP RED
  | A-CI; A-K12.
- R30-13: B21 kelgroups-`lake build` green only (+ 9 `#print axioms`
  names persist Lean-side — referenced mechanism) | A-CI.
- Drift: 5 REQUIRED probes | binding GREEN + 5 directional reruns
  (auditor PROBE ×6, named, no inheritance).
- Guard: B20 omission rerun (ONLY falsification) | A-OMIT rerun (+ B19-log
  read alongside — labeled read).
- .hi tripwire: B3 emission + hash-pin; B22 firing demo (GREEN overlay +
  real drift ⇒ diff fires) | A-COLD emission mirror; A-HIDEMO firing rerun.
- Cold/final: B3 1B + B21 final CI 1B + tracked-clean + Trivial-only +
  founding guard | A-COLD/A-CI.
- SLIM: S1 slim-build + S2 slim-test + S3 slim-ci (itemized) | —.

## Fit (frozen)

Owner builds: B1,B2 (2) + B3 cold + B4 test + B5–B19 fifteen runs
(M1,M2,M3,M4a,M4b,M5,M6,M7,M8,M9,M10a,M11,M12,M13,M15; M14 retired-merged)
+ B20 omission + B21 CI + B22 HIDEMO = GREEN 20 + SLIM S1–S3 (3) = **25**.
Above-20 (each ordered): M4b←D4, M13←D5, B20←D3, B22←NOTE-006-2c,
M15←NOTE-006-1 (money-bearing duplication hole; Lean's own mutant shape).
M10b in B4; drift overlays ride drift probes. Probes ≤24: 5 REQUIRED
(P-DRIFT-GREEN/ADDBYTE/DELBYTE/ADDFILE/JOINMAP) + kill-confirm ≤15
(ambiguous logs only) + dispute ≤2 (beyond → BLOCKED) + transient ≤2,
REQUIRED-first. **PROPOSED owner 25/24.** Auditor: A-RED1/2 2B +
A-COLD/A-TEST/A-CI 3B + A-K×15 15B (UNCONDITIONAL) + A-OMIT 1B + A-REBIND
conditional 1B (unlanded: unspent WITH reason) + A-RESERVE 1B + A-HIDEMO 1B
= 24B; probes 6 required drift + narrowing ≤14 + reconfirm ≤4 = 24.
**PROPOSED auditor 24/24.** Both PROPOSALS pending fit-proof (§12 i–x) +
authorization; gap returns exact cost, never trimmed scope. Discovery
bounds the SET, never coverage. Freeze-validation (i–x) plannable exactly
— no prerequisite, no measurement request.
