# Commit Audit

- Submission: 2/2, replacement seat audit-s2r; prior invalid seat archived without verdict.
- Base / mandate: d350f97b5a0b9d844376401451d6b5b3408c8c2f, specs/71-design-record (unchanged in candidate).
- Candidate: 67877b1311c45fbc39d7750151a3b4307dae3593; PIN 4a6cd87fcbc3e4a536bbc9f240f5efe5704022af.
- Scope: FULL d350f97..67877b1, explicitly ordered by NOTE-008; rejected submission 1: 36666dc, report b5d3199f.
- Verdict: FINDINGS — three blocking findings; full frozen gate passed.
- Audit loop: submission 2/2; next submission FORBIDDEN; ceiling raises 0/2.
- Campaign: CLOSED, ended SET-POINT; 12 representative kills; content findings and one additional citation survivor remain.
- Builds: ticket 2/3; this audit 1/2 allowed, cache=cold at start; no further build needed.
- Seat: codex gpt-6-astra / high, %537 in reactivegas:7; argv and separation from muse %516/%518 verified; no author contact.
- Delivery: LOCAL ONLY; candidate, tracked files and frozen gate unmodified.

Provenance: single 100644 docs/en/design/state-machine.md change (+479/-170), direct parent is mandate base, Good signature, commit gate passes. lean/ is blob-identical to PIN; kelgroups-vote-machine.md is byte-identical to base. Ignored gate copy retains SHA256 b9fef1c76e07bc606d67b58512d1122ba6ace96a643f963f5dcd77dbc0a71624. No outside changed file needs a scope justification.

## Invariant matrix

| Invariant | Severity | Verdict | Row state | Evidence / conclusion |
|---|---|---|---|---|
| R71-01 | BLOCKING | FAIL | KILLED | A01-USERS; F-01: four fields correct; zero-never-stored sentence refuted. |
| R71-02 | BLOCKING | PASS | KILLED | A02-15; Discovered Event 14 / AppEvent 17; retired and live substrate names distinguished. |
| R71-03 | BLOCKING | PASS | KILLED | A03-ROUTE; Route 11/0/3, viewed signatures, all three sealed-hook arms match PIN. |
| R71-04 | BLOCKING | FAIL | KILLED | A04-TENSION; F-02: AUTH proof is overstated; per-event lookup prerequisite omitted. Pledge/correction roles otherwise correct. |
| R71-05 | BLOCKING | FAIL | KILLED | A05-WITNESS; F-01: L4 cites a cash-box-debit theorem as an account-credit law; L5 wrongly rejects zero deposits. Law/witness category distinction present. |
| R71-06 | BLOCKING | PASS | KILLED | A06-LINKS; Production Step import closure excludes Composition; classifier and all three missing runtime links disclosed. |
| R71-07 | BLOCKING | PASS | KILLED | A07-THETA; Vote routing and unfinished lifecycle/placeholder status, threshold exhibits, #75/#76 planned status disclosed. |
| R71-08 | BLOCKING | PASS | KILLED | A08-COUNT; 21 recursively discovered Voci files; both Quantita variants have distinct blobs; five-part non-goal present. |
| R71-09 | BLOCKING | PASS | KILLED | A09-DATES; Dated Q-001/V-series/new agency rulings and composition chain agree with supplied authority. |
| R71-10 | BLOCKING | PASS | KILLED | A10-V5; S1 landed manifest and one affected frozen refusal row; #68/#69/#81 honestly ruled-pending; retention #66-S5 caveat preserved. |
| R71-11 | BLOCKING | FAIL | KILLED | A11-UNKNOWN; F-03: missing non-required citation survives. 109 present markers resolve; 41-anchor minimum does not ensure every claim has a citation. |
| R71-12 | BLOCKING | PASS | KILLED | A12-CLOSE; One canCloseGroup source occurrence; three conjuncts; missing-guarantee classification, no invented theorem. |

Every row has its own fresh log/hash in CAMPAIGN-LEDGER.md (SHA256 24251ff160ffdd0fdd0ed43283129512f31645d7be55748e963b8351a2c920c7). KILLED is the campaign floor, not acceptance. ROW-REVIEW.md (SHA256 fc7517b19c37fa379f7fb0223e7ef5e2fd1a10d763a296ceda08151cd87e9550) records all fourteen authority rows, pending sources and required spot checks.

## Blocking findings

1. **F-01 — R71-01 / R71-05: current economic descriptions contradict the model.** `docs/en/design/state-machine.md:235–243` says closure credits the referente account and cites close_spends_referente. That theorem at Invariants.lean:679 proves a decrease in the referente cassa. A successful reachable journey deposits 30, opens collection 7, pledges/accepts 30, grants permission and closes: conti unchanged, cassa 30→0, escrow 30→0. The document-credit predicate evaluates false; the actual cash-debit relation evaluates true. At doc:251 a non-positive deposit is said rejected, but −1/0/+1 produce refused/accepted/accepted. At doc:64 zero is said never stored, but the accepted zero deposit from empty state stores `(u,0)` and `(a,0)`. **Property class:** prose describing an economic effect or boundary must agree with the actual transition and cited statement, including zero. Evidence: witness-verified.log SHA256 f4bbc9caae1a77b9ea01e6f24f9c4a94f28e1b9080a0461bb585088e36650857. Limit: finite witnesses refute these claims; no new universal guarantee or money-model repair is asserted.

2. **F-02 — R71-04: AUTH proof strength is overstated and the lookup refusal is omitted.** `docs/en/design/state-machine.md:186–191` says step_authorized prevents silently dropping a guard. Predicates.lean:74 defines authorizedStep solely as responsabile author for all fourteen events; states and other event arguments are ignored. The actual predicate is true for an admin-authored deposit to an absent member with an arbitrary unchanged result, whereas the real step refuses it. Thus this predicate does not certify every table guard. Separately, the grant/deny table row at doc:119 lists only signer role; Step.lean:53/57 first requires successful pullCollection. The finite control refuses a grant for an absent id and succeeds after opening that id. **Property class:** a documented authorization contract must preserve the scope of its proof and the actual rejection prerequisites. Same witness SHA256 f4bbc9caae1a77b9ea01e6f24f9c4a94f28e1b9080a0461bb585088e36650857. Limit: no real transition violated its guard; this is a false documentation guarantee, not a Lean soundness finding.

3. **F-03 — R71-11: missing-citation rejection still covers only the required minimum.** F11-MISSING-L4 replaces the sole `lean:close_spends_referente` marker with plain text, retaining its complete declaration-like claim. v4 returns exit 0 / GATE-v4-GREEN, with 108 markers instead of 109. A11-UNKNOWN reaches CITE-RED and the unmodified copy greens, establishing that the same real checker is executed. The claim is outside REQUIRED_ANCHORS, so its removal is not checked. **Property class:** quantifying over surviving markers plus a hand-selected minimum does not enforce a citation for every declaration-like claim. Evidence: doc-campaign/F11-MISSING-L4.log SHA256 7add6bcb3d1944fdaabf67cbd62aa2def4d74168622b99d9566011d8ec617ec3; paired mutation document and all controls are hashed in ARTIFACTS.sha256. Limit: this is a static control run with CI deliberately skipped, not a second full-build claim. The true frozen candidate passed full CI independently. Malformed/unknown/empty controls from submission 1 are repaired; this missing-claim class remains.

## Failure modes altered

none altered -- checked: the full candidate diff changes one Markdown file; lean/ is byte-identical to PIN and every consumer/runtime contract outside that document is unchanged. No resource acquisition, background operation, synchronization primitive or degradation path changed. The documentation errors concern descriptions of existing behavior. Source-discovery exclusion was independently checked cold and with the real .lake present; poisoned .lake source remains excluded while a second real source reference is counted.

## Verification receipts

| Command | Exit | Duration | Evidence SHA256 |
|---|---:|---:|---|
| `env SKIP_CI=1 REPLAYS=1 ./gate.sh` before any build | 0 | 15470 ms | static-gate.log: 3adc56ea2d6e090070ef4c3aa19599c9b607f6a1c72a18c45f88d7b0921a8ced |
| `./gate.sh` v4, including `nix develop --quiet -c just ci` | 0 | 167012 ms | full-gate.log: 7ad972f63d1b0e4ae31ee29ceae6a3ca75236e760e6c5d0b52f71c9c52280622 |
| Python doc-campaign.py (12 bad copies, missing-L4 probe, original positive) | 0 | 25277 ms | doc-campaign-run.log: 96e543008b00bef9349a9e6d8e3985753eab92937e0b96feef0ebe32eea4b833 |
| `nix develop --quiet -c bash -c "cd lean && lake env lean <runtime>/instruments/DocWitness.lean"` | 0 | 5450 ms | witness-verified.log: f4bbc9caae1a77b9ea01e6f24f9c4a94f28e1b9080a0461bb585088e36650857 |
| Python pinned-inventory.py | 0 | 221 ms | pinned-inventory-verified.log: ec5fc422dcb372dc5620f54e47f69323724e79c06b10811e4968811222a19467 |
| `bash <runtime>/instruments/lake-exclusion.sh` | 0 | 41 ms | lake-exclusion.log: d2f66e408f87f1d98702bc53725019d2ffd7c8079b21621281d7335b2236a56d |
| `commit-gate HEAD`, provenance and frozen-gate checks | 0 | not separately timed | provenance-final.log: 09dfeab119c8c4ad40f8c6ee97a68ec27344222b6d07a0ab91430e70339589f6 |

Full gate executes Haskell build/format/hlint, toolchain contract, dependency-direction control, inversion control, trace agreement, Lean build and corpus gate. Observed 14/14 trace bindings, intended withheld-backdonate negative control, no hlint hints, 27-job Lean completion, final v4 GREEN. Existing warning output remains in the complete log. Shared Nix store may be warm; local .lake was absent at START and after static legs. Free space before/after build: 220021096448 / 219912806400 bytes.

## Residuals

None accepted. The extra R71-11 survivor is a blocking finding. Neither representative kills nor aggregate green establish full semantic/citation coverage.

## Candidate invariants

None added; all findings bind declared mandate rows. No new severity or scope is proposed.

## Onward discoveries — outside this ticket

None opened. Current source and supplied authority were used only for this document audit. No remote CI/deployment or external simulator corpus was verified; no delete-file build was spent.

## Advisories

- The #81/V-5 pending row has four data cells under five headers; its re-pin text occupies the source-ruling column. Its dated ruling and pending status are supplied elsewhere on the page.
- Exact Voci file census requires recursive `git ls-tree -rz PIN -- Voci/`: 21 files. The non-recursive spelling in the brief lists 18 entries including the UI tree.
- Seven required-table anchors plus the S1 Trace manifest were re-derived directly at PIN, including Vote files. Present markers: SM 80, VM 30, union 109, over 805 declarations. V-7 retention caveat and #66 S5 pointer are present; #75/#76 are named planned, not delivered.
- Prior invalid seat evidence was read only as method/provenance input; no result or build spend was inherited. Setup-only instrument errors are retained with their failed logs and excluded; the reported instruments have exit-zero final runs, and all non-build probes reused existing imports.
- Retained original instruments, documents, logs, authority snapshots, review and ledger are bound by ARTIFACTS.sha256 (SHA256 7a0d6ae4b1cd0ba07bb14174b0168415f2eb73ebbc7b7c1d7d4dadefead21b9d). No runtime-root build tree was created. The detached audit worktree with build products is left for ticket-owner retirement and named in STATUS.

One verdict only. Submission cap remains exhausted. Acceptance and any next action belong to the ticket owner.
