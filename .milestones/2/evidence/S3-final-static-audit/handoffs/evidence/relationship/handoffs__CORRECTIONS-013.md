# CORRECTIONS-013 — five defects repaired against stated obligations (no execution)

Static only: archive/source reads quoted with file references. No builds,
queries, elaborations, probes, mutations, Phase-2, or code/theorem changes.
Prior versions preserved; where this file conflicts with them on the five
points below, THIS file governs (other content stands as written).

## A. Files 42/43 — collapsed states separated, seven fields each

Previous text recorded both as OPEN/NONE. Verified false for file 42:

| row (file 42, corrected TSV, all KILLED) | subject | mutation | checker | fixture | footprint+evidence | toolchain | command |
|---|---|---|---|---|---|---|---|
| INV-TC-01 | toolchain-pin enforcement (`scripts/check-lean-toolchain` via `just lean-toolchain-contract`) | M01-historical-4.27.0-pin | contract gate | `instruments/m01-pin-mismatch.sh` (`6ac1bbc1…`, present in archive) | `evidence/m01-pin-mismatch.log` (`7606b51e…`, exit 1, pinned 4.27.0 vs running 4.25.0 — all read in-archive) | pins recorded per receipt | gate argv per `evidence/gate-v1.log` | → STALE-DEMONSTRATED only if base pin/files differ (pin at base STILL `v4.25.0`; subject files vs auditor base: OPEN as to demonstrated move — see below) |
| INV-TC-02 | exact-bytes pin declaration | M02-exact-bytes+trailing-space+missing-v+empty | contract gate | `instruments/m02-exact-pin.sh` (`70146ea5…`) | `evidence/m02-exact-pin.log` (`a0028319…`); candidate pin bytes `leanprover/lean4:v4.25.0` | recorded | gate log | → same marking as -01 |
| INV-TC-03 | comparator soundness | M03-always-agree-comparator | contract gate | `instruments/m03-always-agree.sh` (`71e71d53…`) | `evidence/m03-always-agree.log` (`a9ea57c1…`, exit 1, mutated pin 4.25.1 live patch+1) | recorded | gate log | → same |
| INV-TC-04 | CI order/fence | M04-justfile-drop-call+yaml-swap | contract gate | `instruments/m04-ci-order.sh` (`5e950b2d…`) | `evidence/m04-ci-order-v2.log` (`f4a73526…`) + `gate-v1.log` (`ccfcb62a…`, marker ×2 lines 4/7, lean build at 15014) | recorded | gate log | → same |

Honest status for all four: full seven-field receipts RECOVERED (nothing
UNKNOWN except the auditor's base commit, absent from the ledger — stated);
toolchain subject (not Lean theorems) → UNUSABLE for D2 kills AND retained as
toolchain-context evidence (not dismissed). Demonstrated-move for these four:
OPEN — the base pin matches the auditor's running pin and subject-file diffs
were not established; reuse unestablished. Previous "OPEN/NONE" text for file
42 is withdrawn. File 43 (owner ledger, INV-TC-01..04 OPEN/NONE): confirmed as
read — open obligations, UNESTABLISHED-REUSE.

## B. t62 row-ledgers files 15–21/23 — blanket dismissal replaced by readings

Each file's table transcribed with states as recorded (seven fields: subject /
mutation-or-control / checker / fixture-or-log / footprint+evidence /
toolchain / command; UNKNOWN where the ledger gives prose only):

- File 15 (a011-s1-r1): G62-C-THEOREMS/ECONOMY/EXHAUSTIVE/TRUST-CI KILLED
  (inherited; declaration blob `ab9b4aad…`, full-ticket receipt `d369103c…`);
  I57-01-BOUNDARY FAIL→BLOCKED (duplicate-state finding, `d53064ca…`);
  G62-C-INHERITED57 KILLED (franchise/caller-threshold controls, DISJOINT
  inherited). Footprints: cited hashes, no per-row mutant sources →
  inherited-KILLED rows UNESTABLISHED-REUSE + procedural bar ("do not reopen"
  per ledger text — re-verification barred by the owning ticket's rule, stated
  not overridden); BLOCKED row = open obligation (duplicate-state).
- File 16 (a011-s2-glm): same six rows, all KILLED (carried; "Not reopened"
  except I57-01 with fresh `evidence/probe-main2.log` duplicate/bypass
  instrument — located in archive per directory listing) → same markings;
  I57-01 UNESTABLISHED-REUSE (probe log present, per-row pins prose-level).
- File 17 (a013-grok): six rows KILLED carried (`evidence/a013-gate.log`
  `716afe5d…`, `Step.lean` blob `06b2d12e…` recorded) → UNESTABLISHED-REUSE +
  procedural bar.
- File 18 (s1-codex): THEOREMS/ECONOMY/EXHAUSTIVE KILLED (evidence hashes
  `968fc50f…`, `76c32a87…`, `28d6a8fb…` + frozen instrument `6dee1ad1…`);
  TRACE BLOCKED (F-TRACE: no integrated emitter); INHERITED57 BLOCKED
  (F-I57-ONE-DECISION, F-I57-INTEGRATED-LEGS — ledger text: "DISJOINT,
  FRANCHISE, POLICYFREE checks do not reach the integrated transition and are
  insensitive to path mutations" — recorded as a SCOPE finding limiting those
  rows' ownership, re-keyed accordingly); TRUST-CI BLOCKED (F-I57-TOOLCHAIN:
  pin 4.27.0 vs executing 4.25.0). KILLED → UNESTABLISHED-REUSE + bar; BLOCKED
  → open obligations with named blockers.
- File 19 (s2-codex): THEOREMS/ECONOMY/EXHAUSTIVE KILLED carried-terminal ("do
  not reopen"); TRACE/INHERITED57/TRUST-CI BLOCKED (active scope: F-TRACE
  four mutants; F-I57 legs + I57-10 toolchain boundary) → same markings.
- File 20 (owner-a011-a012): six rows KILLED inherited ("not reopened") →
  UNESTABLISHED-REUSE + bar.
- File 21 (campaign-a013): six rows with A013 obligations (carry blobs, re-run
  CI) → obligations recorded; KILLED-carried → UNESTABLISHED-REUSE + bar.
- File 23 (ceiling-s62-c-a011): THEOREMS/ECONOMY/EXHAUSTIVE/TRUST-CI KILLED
  inherited (blobs `ab9b4aad…`, receipts `968fc50f…`/`76c32a87…`/`90c28b37…`/
  `b6117b60…`) → UNESTABLISHED-REUSE + "Do not reopen or rebuild" bar.

## C. Literal atom ledger + finite per-row relation (no family clôture)

Atoms (source refs at base; G/E from `Step.lean:44-145` read in full):
G-openPurchase{isResponsabile,¬exists(c)}; G-grantPermission{pull-ok,isResponsabile};
G-denyPermission{pull-ok,isResponsabile}; G-deposit{isResponsabile,isMember(u),signer≠u,0≤v};
G-withdraw{+bal≥v,¬stalled}; G-transferCassa{resp(signer),resp(f),signer≠f,v>0};
G-donate{isResponsabile,0<v}; G-backdonate{isResponsabile,0<w,comuneBal≥n·w,auth(s,w)}
+ AUTH-backdonate; G-pledge{pull-ok,isResponsabile,isMember(u),¬acc-u,¬pend-u,0<v,bal≥v,¬stalled};
G-acceptPledge{pull-ok,split-ok,isResponsabile,referente=signer,¬stalled};
G-refusePledge{pull-ok,split-ok,isResponsabile,referente=signer};
G-correctPledge{+0≤v',bal(u)+(v−v')≥0}; G-closePurchase{+permitted,pending.empty,¬stalled};
G-failPurchase{+pending.empty}. Effects E-<ctor> per arm equation (read).
Vote passthroughs (openQuestion/cast/renounce as AppEvent): NONE — `none` arms.
V-open{isResponsabile}; V-cast{isResponsabile,lookup-some}; V-renounce{same};
V-franchise{canonical-view membership}; V-threshold{legacyThreshold/zeroThreshold
(`Vote/Types.lean:44,48`)}; V-sweep{sweepStep,sweepClosures}; V-tally{placeBallot,
assents/dissents}; V-qid{question-id tracking}; V-fold{foldVote/foldFrom};
V-event-hpres{PreservesQuestionSemantics premise}. B-prop-{admin,bootstrap,normal};
B-approve-{admin,pending-found,not-dup}; B-admit-{admin,reserved,dup} (ordered,
`Validate.lean:142-150` verified); B-mut-remove{admin,member};
B-mut-roles{admin,member}; R-canAdd{`roleDef.canAdd gs.appFold`};
R-canRemove{`roleDef.canRemove gs.appFold`}; B-baseapprove; B-event-app{isMember};
B-hook-{memberAdmitted,memberRemoved,rolesChanged} (`Integration.lean:139-145`
verified); B-enact{threshold/met}; W-fold; W-cohere{4 WellFormed fields};
E-govern{windUpAdmin}; E-fund{balance/escrow movement}; AUTH-backdonate.

Per-row relation: retained `handoffs/OPMAP-requirement-operation.txt`
(`7ba6a446…`, 174 lines `OP|requirement|atom`) — machine-audited: all 158
authored qualified identities appear ≥1 (0 uncovered), 0 helper rows present.
Each row's atoms are the third column; rows sharing an op share that op's
admitted mutant, with property-specific failure observed per row (contract
rule). OPEN relevance (explicit, never filled): solvent per-atom
fund-sensitivity beyond the listed E-fund scope; t57-instrument→theorem pins
beyond recorded subjects; R-canAdd/R-canRemove theorem linkage (no statement
names them — recorded as the one atom-scope gap).

## D. Complete operation map (every row → op; classes counted, not fitted)

| op(s) | kind (M-elab** closure) | rows covered | cost |
|---|---|---|---|
| OP-11..24 (14) | Step-closure: rebuild Step + Predicates + check Invariants | 14 inversions + same-arm economic rows + collective observation of conservation/step_authorized in each run | 14×3 = 42 targeted |
| OP-25..31 (7) | Step-closure | 7 solvent rows | 7×3 = 21 |
| OP-32..38 | static classification (C-ALIAS) | 14 alias identities | 0 |
| OP-39/40/41 (3) | Fold-closure: rebuild Fold + check Vote/Invariants | DISJOINT / NOSTALE-core / POLICYFREE rows | 3×2 = 6 |
| OP-42..48 (7) | recovered-instrument re-runs (1 elaboration each; drivers in-archive) | t57 ledger rows OBSERVED (sensitivity re-verification, never kills) | 7×1 = 7 |
| OP-49..57 (9) | Fold-closure | all remaining vote rows per opmap | 9×2 = 18 |
| OP-58..60 (3) | Validate-closure: rebuild Validate + Integration + check | B-admit rows (admin/reserved/dup atoms) | 3×3 = 9 |
| OP-61..63 (3) | Integration-closure: rebuild Integration + check | hook/enact rows | 3×2 = 6 |
| OP-64..67 (4) + OP-67G (1) | Fold-closure / Step-closure (67G) | preservation/coherence + KelGroups-side counterparts + governance (Step-closure) | 4×2 + 3 = 11 |
| OP-68..72 (5) | file elaboration, no mutation | all witness rows by owning file | 5×1 = 5 |
| OP-73 | full-tree build + 2 file elaborations | final acceptance (incl. solvent_init boot check) | 1 build + 2 targeted |
| 81 helpers + 14 aliases (static) | witnesses/classification on file | 0 | 0 |

Envelope (unfunded; batching unsubtracted; not a grant or budget): targeted
42+21+6+7+18+9+6+11+5+2 = 127 + 1 build. Every authored row appears in the
retained opmap; every op above names its closure class; t57 re-runs and kills
are different ops and never the same invocation.

## E. Isolation downgraded; restoration kept separate (F-07 correction)

- WITHDRAWN as "ISOLATION": the Build-2 record establishes ONLY
  single-cause attribution (one-file one-line diff + RED naming exactly the
  owning theorem with the mutated atom quoted). It does NOT establish
  filesystem/source-root separation: the scratch variant was built INSIDE the
  candidate worktree (same `.lake` root), so artifact-separation evidence does
  not exist.
- OPEN (explicit): source-root separation for mutant builds (detached scratch
  worktree + fresh-root build) — unexecuted, ungranted; required before any
  isolation claim beyond single-cause attribution.
- RESTORATION (kept, separate): Build-3 GREEN single-file restore, 3 s wall,
  retained log — re-greening only, cited for nothing else.
- COLD 19 s: observed-once, log lost — bounds nothing, cited nowhere as a
  ceiling (G-B2 stays withdrawn).

*End of CORRECTIONS-013. F-08 compliance: this file was written directly; all
journal writes from the NOTE-013 ack onward use append-to-EOF shell command
with tail readback. No edit-tool write to STATUS.md remains in effect.*
