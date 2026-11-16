# T30-COMMAND-MAP-r4 — frozen requirement-to-command/control map (kelgroups #30)

Companion to `T30-CONTRACT-r4.md` §7 (same frozen content, row-addressable).
Preparation-only (spend 0; commands NAMED, never executed). Base `933e385d`;
Lean `3590c001` (zero-diff EMPTY vs `4a6cd87`). SUPERSEDES r3 map (retained);
N5-1–N5-6 applied — live mechanisms with named oracles, or labeled limits.

Predicates: COMPILER-kill (exit≠0 + diagnostic quotes ctor/site + zero
parse-error lines); TEST-kill (exit≠0 + `Failures:` names ≥1 registered
REQ-ID); GREEN-ENUM (exit 0 + lists every allowed ctor). Setup/infra/crash/
timeout/parse = INCONCLUSIVE abort, never kill. Hidden invocations
forbidden. Charge-0 recon free, never evidence. BAN restated: regex output
is never cited as semantic inventory, anywhere.

Exact commands: `nix develop .#ci --quiet -c just build`;
`nix develop .#ci --quiet -c cabal test all -O0 --test-show-details=direct`;
`nix develop .#ci --quiet -c just ci`; probes `nix develop .#ci --quiet -c
cabal test invariants --test-option=--match --test-option=/S30-<Group>/
<REQ-ID>/` (Groups frozen below — fully determined).

## REQ groups (frozen)

`S30-Open`: REQ-OPEN-COLL, REQ-OPEN-PERM, REQ-OPEN-REFUSE, REQ-OPEN-DUP.
`S30-Cast`: REQ-CAST-ASSENT, REQ-CAST-SWITCH, REQ-CAST-POSTSWITCH,
REQ-CAST-RECAST, REQ-CAST-UNKNOWN, REQ-CAST-NONRESP. `S30-Sweep`:
REQ-SWEEP-TALLY, REQ-SWEEP-DISSENT, REQ-SWEEP-FRANCHISE, REQ-RETAIN,
REQ-NOEXPIRY. `S30-Verdict`: REQ-VERDICT-COLL, REQ-VERDICT-PERM.
`S30-Franchise`: REQ-FRANCHISE-CURRENT. `S30-Negative`: REQ-NEG-DELIVER.
`S30-Route`: REQ-ROUTE-ENUM. `S30-Lifecycle`: REQ-HOOK-EXT,
REQ-RECORD-SHAPE. `S30-Client`: REQ-CLIENT-ROUNDTRIP. `S30-Admit`:
REQ-ADMIT-PATH, REQ-NONDECIDE-PERM. Every ID registered + executed else
RED. B19 (one ID removed → RED) is the ONLY guard falsification; C1/C2
absence-only.

## Extent + per-file rule (N5-3 — live 7-file Vote dir, observed read-only)

Event, Fold, Invariants, State, Tests, Types, Validate. Types/Event/
Validate: inductives → MUST emit ctor rows. State/Fold: structures +
equation groups → MUST emit. Invariants: theorems/proofs ONLY →
expected-empty (reason stated). Tests: witness executables →
expected-empty (reason stated). Other file emitting nothing → RED.
Empty-global → RED always. 5-file evidence list = mirror obligations; the
2 proof/witness modules stay in the reviewed extent — scope unchanged.

## Drift machinery (N5-1/N5-2 — oracles named)

L1 input binding (oracle: sha256sum + git): per-file hashes + HEAD pin +
empty-porcelain both checkouts, single-leg atomicity. L2 elaboration
validity (oracle: Lean elaborator): `lake build` exit 0. L3 coverage join
(oracle: overlay demos): frozen mapping ⨝ live emissions. L4 Haskell
metadata (oracle: GHC): `ghc --show-iface` inventory over B3 `.hi`
products, hash-pinned, diff-triggered; directional demo rides B15's log
(secondary). L5 arm totality (oracle: GHC -Werror, live; demonstrated via
M10a's break in B15's log). Haskell function presence = compilation +
REQ-execution (mapping rows carry REQ-IDs). Lean patterns = REVIEW
PROMPTS only. Baseline = rigorous review + signed record
(enforced:REVIEW; oracle: record + epic source-verification). Regression =
L1–L5 refusal machinery. Temporal hole → explicit-rebind process rule.
REQUIRED probes: P-DRIFT-GREEN, P-DRIFT-ADD, P-DRIFT-SRCOMIT,
P-DRIFT-MAPOMIT, P-DRIFT-FILEADD (`git archive` export + ONE edit,
export-diff bound, hash+join end-to-end — trigger discrimination on
source-shaped bytes, NOT review correctness). Output-copy controls
DELETED.

## Rows (obligation → owner → auditor)

- RED: B1 + B2 (absence ONLY) | A-RED1/A-RED2 frozen-BASE reruns.
- R30-1: B4 + probes | A-TEST; reviews.
- R30-2: B4; B6 M2; B7 M3; B8 M4a (criterion SWITCH REDs); B9 M4b
  (criterion RECAST REDs) + §8 classification of extras (POSTSWITCH
  REQUIRED; observed signatures = freeze characterization, never
  pre-stated acceptance) | A-TEST; A-K reruns (ALL 14 UNCONDITIONAL).
- R30-3: B4 (append + replay); B10 M5; B11 M6; B12 M7merged; B14 M9 |
  A-TEST + A-Ks; M6 = named boundary rerun.
- R30-4: B4; B13 M8 | A-TEST.
- R30-5: B3 cold | A-COLD; tripwire review (never kill).
- R30-6: B4; B18 M13 → REQ-FRANCHISE-CURRENT RED | A-TEST; A-K13.
- R30-7/14: B4 boundary + roundtrip | A-TEST; A-K6.
- R30-8 + bounded surface (N5-5): B3; B15 M10a (COMPILER/CLOSED-totality;
  in-log .hi-diff + exhaustiveness-fire secondaries); M10b GREEN-ENUM
  named in B4 (per-ctor enactment THROUGH totality-witness `case` over
  frozen allowed set); translation-totality control = D2 join (new ctor ⇒
  P-DRIFT-ADD RED) + witness `case`; review-only remainder named (allowed
  set matches intent); universals deleted | A-COLD; A-K10a UNCONDITIONAL
  (stale conditional deleted); M10b re-checked in A-TEST.
- R30-9: current-base freeze | A-REBIND iff #68 landed (author integrates
  + fresh final-SHA audit; auditor never repairs).
- R30-10 surface: B4; B16 M11 → REQ-HOOK-EXT RED | A-TEST; A-K11.
- Produced-cause distinction: B12 M7merged (forced-.tally →
  REQ-SWEEP-FRANCHISE RED; carried excluded) | A-K7.
- L-1–L-7: recorded with owners; NO command | record-only review.
- R30-10U/PROD, R30-11: no command | —.
- R30-12: B20 leg-6 (TEST-boundary, limit stated); B17 M12 → ROUNDTRIP RED
  | A-CI; A-K12.
- R30-13: B20 `lake build` green only | A-CI.
- Drift: 5 REQUIRED probes | binding GREEN + 5 directional reruns
  (auditor PROBE ×6, named, no inheritance).
- Guard: B19 omission rerun (ONLY falsification) | A-OMIT rerun (+ B19-log
  read alongside — labeled read).
- Cold/final: B3 1B + B20 final CI 1B + tracked-clean + Trivial-only +
  founding guard | A-COLD/A-CI.
- SLIM: S1 slim-build + S2 slim-test + S3 slim-ci (itemized; legs 1/2/2b/7
  ride charge-0/probe; drift GREEN probe in cap) | —.

## Fit (frozen)

Owner builds: B1,B2 (2) + B3 cold + B4 test + B5–B18 fourteen runs + B19
omission + B20 CI = GREEN 18 + SLIM S1–S3 (3) = **23**. Above-20: M4b←D4,
M13←D5, B19←D3. M10b in B4; drift/.hi-overlays ride counted runs/probes
(zero added invocations — N5 adds none). Probes ≤24: 5 REQUIRED
(P-DRIFT-GREEN/ADD/SRCOMIT/MAPOMIT/FILEADD) + kill-confirm ≤14 (ambiguous
logs only) + dispute ≤3 + transient ≤2, REQUIRED-first. **PROPOSED owner
23/24.** Auditor: A-RED1/2 2B + A-COLD/A-TEST/A-CI 3B + A-K×14 14B
(UNCONDITIONAL) + A-OMIT 1B + A-REBIND conditional 1B (unlanded: unspent
WITH reason) + A-RESERVE 1B = 22B; probes 6 required drift + narrowing
≤14 + reconfirm ≤4 = 24. **PROPOSED auditor 22/24.** Both PROPOSALS
pending fit-proof (§12 i–vii at freeze) + authorization; gap returns exact
cost, never trimmed scope. Discovery bounds the SET, never coverage. No
prerequisite, no measurement request: §12 freeze-validation is plannable
exactly as specified.
