# T30-COMMAND-MAP-r7 — frozen requirement-to-command/control map (kelgroups #30)

Companion to `T30-CONTRACT-r7.md` §7 (same frozen content, row-addressable).
Preparation-only (spend 0 product builds; 2 plumbing invocations on the
closed pf1 campaign, own counter). Base `933e385d`; Lean `3590c001`
(zero-diff EMPTY vs `4a6cd87`). SUPERSEDES r6 map (retained); NOTE-008
applied — frozen-oid reads, source-hash channel, exact-success records,
POSIX constructs, receipt provenance, mode refusal, uniqueness.

Predicates: COMPILER-kill (exit≠0 + diagnostic quotes ctor/site + zero
parse-error lines); TEST-kill (exit≠0 + `Failures:` names ≥1 registered
REQ-ID); GREEN-ENUM (exit 0 + lists every allowed ctor). Setup/infra/crash/
timeout/parse = INCONCLUSIVE abort, never kill. Hidden invocations
forbidden (leg-unit accounting). Charge-0 recon free, never evidence. BAN:
regex output never cited as semantic inventory. Portability: POSIX sh +
POSIX BRE; no leading-dash formats (F9/F10 lesson).

Exact commands: `nix develop .#ci --quiet -c just build`;
`nix develop .#ci --quiet -c cabal test all -O0 --test-show-details=direct`;
`nix develop .#ci --quiet -c just ci` (INCL kelgroups-own `just lean`
sub-step — tabulated); probes `nix develop .#ci --quiet -c cabal test
invariants --test-option=--match --test-option=/S30-<Group>/<REQ-ID>/`
(Groups frozen — fully determined). Reactivegas-side (read-only — NO lake
build): `rev-parse`, `git show $FROZEN_OID:<path> | sha256sum`,
`status --porcelain`, `ls-files`, `git archive` (each rides its
leg/probe counter). Hash assumption stated in contract (SHA-256
second-preimage; breakage breaks L1/L4 only).

## REQ groups + IDs (26, frozen)

`S30-Open`: REQ-OPEN-COLL, REQ-OPEN-PERM, REQ-OPEN-REFUSE, REQ-OPEN-DUP.
`S30-Cast`: REQ-CAST-ASSENT, REQ-CAST-SWITCH, REQ-CAST-POSTSWITCH,
REQ-CAST-RECAST, REQ-CAST-UNKNOWN, REQ-CAST-NONRESP. `S30-Sweep`:
REQ-SWEEP-TALLY, REQ-SWEEP-DISSENT, REQ-SWEEP-FRANCHISE, REQ-RETAIN,
REQ-NOEXPIRY, REQ-SWEEP-IDEM. `S30-Verdict`: REQ-VERDICT-COLL,
REQ-VERDICT-PERM. `S30-Franchise`: REQ-FRANCHISE-CURRENT. `S30-Negative`:
REQ-NEG-DELIVER. `S30-Route`: REQ-ROUTE-ENUM. `S30-Lifecycle`:
REQ-HOOK-EXT, REQ-RECORD-SHAPE. `S30-Client`: REQ-CLIENT-ROUNDTRIP.
`S30-Admit`: REQ-ADMIT-PATH, REQ-NONDECIDE-PERM. Every ID registered +
executed with EXACT-success records (`PASS: <id> OK` full-line
fixed-string; FAILED/SKIPPED/bare names never match) else RED. B20 (one ID
removed → RED) ONLY falsification; C1/C2 absence-only.

## Extent + per-identity rule (actual 7-file Vote dir, all read in full)

Event, Fold, Invariants, State, Tests, Types, Validate (+ KelGroups
Event/State/Validate/Integration/Types consumed-context). Types/Event/
Validate: inductives MUST emit. State/Fold: structures + equation groups
MUST emit. Invariants/Tests: expected-empty (proof-/witness-only; every
identity tabled + 40-name appendix). Other-file-empty → RED.
Empty-global → RED always. Full rows: identity-map + appendix (rule
subject: ctor/arm identities + equation groups, stated). Projection
statement (appendix §C): 12 paths bound; Fold/Invariants/Tests
(KelGroups) projected out with zero-Vote-identifier verification.

## Drift machinery (script r7 — UNDEMONSTRATED; demonstration = §12-R7)

L1 binding (frozen-oid immutable views both repos + HEAD-position checks +
file-sets both scopes + mapping self-check + labeled clean samples;
residual race stated). L2-as-execution DROPPED. L3 join (frozen mapping ⨝
live emission + leg-4 exact-success log; NONEMPTY + EXACT-COUNT +
row-UNIQUENESS anti-vacuity gates; POSIX BRE row-count with comments-only
proof; unmapped/dangling → RED). L4 .hi tripwire (GHC oracle;
receipt-preconditioned emission per frozen-module row incl.
`KelGroups.Event`; freshness-marker rule; exactly-one-or-RED;
unconditional re-emit; hash-pin; diff→review; firing demo rides B22a/b) +
source-hash tripwire per mapped module (immutable-view reads; catches
unexported additions — INDEPENDENT channel). L5 arm totality (GHC -Werror
live; M10a's break in B15's log — the break only). Function presence =
compilation + REQ-execution. Lean patterns = REVIEW PROMPTS only. Baseline
= review + signed record (enforced:REVIEW). Temporal hole →
explicit-rebind rule. REQUIRED probes (7): P-DRIFT-GREEN/ADDBYTE/DELBYTE/
ADDFILE/JOINMAP/SAMESIZE/SRCADD (archive export + ONE edit, export-diff
bound — trigger discrimination on source-shaped bytes, NOT review
correctness). Output-copy controls DELETED. Item attribution =
enforced:NONE automatic + MANDATORY re-review (RED on ANY mismatch until
signed). Unknown MODE refused (exit 3).

## Rows (obligation → owner → auditor)

- RED: B1 + B2 (absence ONLY; Vote-absence re-verified) | A-RED1/A-RED2
  frozen-BASE reruns.
- R30-1: B4 + probes | A-TEST; reviews.
- R30-2: B4; B6 M2; B7 M3; B8 M4a (SWITCH REDs); B9 M4b (RECAST REDs,
  inverts source-verified guard) + classification (POSTSWITCH REQUIRED;
  observed = freeze characterization) | A-TEST; A-K unconditional.
- R30-3: B4 (append + replay); B10 M5; B11 M6; B12 M7merged; B14 M9; B19
  M15 sweep-without-removal → REQ-SWEEP-IDEM RED (distinct site/duty/
  witness; Lean's own mutant shape) | A-TEST + A-Ks; M6 = named boundary
  rerun.
- R30-4: B4; B13 M8 | A-TEST.
- R30-5: B3 cold | A-COLD; tripwire review (never kill).
- R30-6: B4; B18 M13 → REQ-FRANCHISE-CURRENT RED (post-view = sensitivity
  fixture, limit stated) | A-TEST; A-K13.
- R30-7/14: B4 boundary + roundtrip | A-TEST; A-K6.
- R30-8 + bounded surface: B3; B15 M10a admission-ctor-added to
  BaseMutation (COMPILER/CLOSED-totality; in-log exhaustiveness-fire
  secondary — NO .hi secondary); M10b GREEN-ENUM named in B4
  (totality-witness `case`); translation control = D2 join + witness case
  (zero new builds); review-only remainder named | A-COLD; A-K10a
  UNCONDITIONAL; M10b re-checked in A-TEST.
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
  persist Lean-side — referenced) | A-CI.
- Drift: 7 REQUIRED probes (GREEN/ADDBYTE/DELBYTE/ADDFILE/JOINMAP/SAMESIZE/
  SRCADD) | binding GREEN + 7 directional reruns (auditor PROBE ×8:
  binding + directionals, named, no inheritance).
- Guard: B20 omission rerun (ONLY falsification) | A-OMIT rerun (+ B19-log
  read alongside — labeled read).
- .hi tripwire: B3 emission (+ receipt) + hash-pin; B22a baseline GREEN +
  B22b overlay drift ⇒ diff fires (TWO counted invocations) | A-COLD
  emission mirror; A-HIDEMOa baseline + A-HIDEMOb overlay.
- Source-hash channel: mapped-module immutable reads in D-1 (both modes) |
  auditor re-derives (probe-class, named).
- Cold/final: B3 1B + B21 final CI 1B + tracked-clean + Trivial-only +
  founding guard | A-COLD/A-CI.
- SLIM: S1 slim-build + S2 slim-test + S3 slim-ci (itemized) | —.

## Fit (frozen)

Owner builds: B1,B2 (2) + B3 cold + B4 test + B5–B19 fifteen runs
(M1,M2,M3,M4a,M4b,M5,M6,M7,M8,M9,M10a,M11,M12,M13,M15; M14 retired-merged)
+ B20 omission + B21 CI + B22a baseline + B22b overlay = GREEN 22 + SLIM
S1–S3 (3) = **26**. Above-20 (each ordered): M4b←D4, M13←D5, B20←D3,
B22a+B22b←NOTE-006-2c + NOTE-007(d), M15←NOTE-006-1. M10b in B4; drift
overlays ride drift probes. Probes ≤24: 7 REQUIRED (GREEN/ADDBYTE/DELBYTE/
ADDFILE/JOINMAP/SAMESIZE/SRCADD) + kill-confirm ≤13 (ambiguous logs only,
priority-ordered: disputed/ambiguous first) + dispute ≤2 (beyond →
BLOCKED) + transient ≤2, REQUIRED-first. **PROPOSED owner 26/24**
(rebalanced within caps; builds identical to r6). Auditor: A-RED1/2 2B +
A-COLD/A-TEST/A-CI 3B + A-K×15 15B (UNCONDITIONAL) + A-OMIT 1B + A-REBIND
conditional 1B (unlanded: unspent WITH reason) + A-RESERVE 1B + A-HIDEMOa/b
2B = 25. **PROPOSED auditor 25/24**
(25B; probes 8 required drift + narrowing ≤12 (priority-ordered) +
reconfirm ≤4 = 24). Both PROPOSALS pending fit-proof (§12 i–xi + receipt/
POSIX/portability items) + authorization; gap returns exact cost, never
trimmed scope. Discovery bounds the SET, never coverage. Preflight
campaign: 0 product builds (closed pf1 counter: 2 plumbing invocations);
R7 campaign awaits its own grant with the SINGLE final request in contract
§12-R7.
