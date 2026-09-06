# Commit Audit

- Submission: S71-B 1/2, fresh FULL audit; prior S71-A seats are archived inputs only.
- Base / mandate: 90dae994da67d889831726dd1f17aaae9ad84be1, specs/71-design-record.
- Candidate: 8e4cbb8b95ac5a2063ea39cf2d2ac6a4c1d15163.
- Source PIN: 4a6cd87fcbc3e4a536bbc9f240f5efe5704022af.
- Scope: whole unaccepted #71 record, all R71-01..12; repair provenance is base..candidate.
- Verdict: **AUDIT-FINDINGS — three BLOCKING findings.** Full frozen gate passed.
- Audit loop: submission 1/2; one repair and fresh submission-2 audit remain allowed by the brief.
- Ceiling raises: 0/2. Campaign: CLOSED, SET-POINT, twelve fresh representative kills.
- This seat: 1/2 builds, 20/20 targeted invocations; no budget overrun.
- Seat: codex gpt-6-astra/high, pane %546 in reactivegas:7, distinct from %516/%542;
  exact pinned argv verified before START. No author contact or candidate repair.
- Delivery: LOCAL ONLY. Acceptance and any onward routing belong to ticket owner %516.

The candidate is a signed, direct child of the mandate base. Its two 100644 changed
files are state-machine.md and eight marker-only additions in kelgroups-vote-machine.md;
removing the added parenthesized markers restores the companion's base bytes.
No tracked path outside the allowed set changed. The candidate and PIN share Lean
tree d3e88048aabd6b834977ce333d9f4e6b47d19154. The ignored audit-local gate copy exists
only to satisfy v6's directory-relative recursion; it is byte-identical to the frozen
gate and was never edited. Gate SHA256:
877d9b9c596c216f688bf32c5f31ba29ad1b56753bb9ae083cf52007cdaf9085.
Inbox NOTE-001 formally confirms that binding; no duplicate verification was run.

## Invariant matrix

All severities were declared BLOCKING. KILLED is the finite campaign floor, not
acceptance or proof that every mutation is caught.

| Invariant | Severity | Verdict | State | Fresh evidence / conclusion |
|---|---|---|---|---|
| R71-01 | BLOCKING | PASS | KILLED | B01-USERS. Four fields and single membership source; stored-zero repair matches live transition. |
| R71-02 | BLOCKING | PASS | KILLED | B02-EVENTS. Compiler indexes: Event 14, AppEvent 17; retired/surviving namespaces distinguished. |
| R71-03 | BLOCKING | PASS | KILLED | B03-STEP. Viewed signatures; route 11/0/3; all three sealed-hook arms match PIN. |
| R71-04 | BLOCKING | PASS | KILLED | B04-TENSION. All fourteen economic event guards read; AUTH role-only scope and grant/deny lookup-first repair hold. Citation gaps are R71-11. |
| R71-05 | BLOCKING | PASS | KILLED | B05-WITNESS. Law/witness and 29-not-a-census caveats; repaired cassa debit and deposit boundary agree with source. Economic ruling omission is R71-09. |
| R71-06 | BLOCKING | PASS | KILLED | B06-LINK. Compiled Step import closure excludes Composition; three unbound links and PROVED-IN-MODEL status disclosed. |
| R71-07 | BLOCKING | PASS | KILLED | B07-THETA. Vote routing, lifecycle limits, dormant errors, fifteen standalone signed events, zero #74 vote events and open threshold exhibits verified at their stated pins. |
| R71-08 | BLOCKING | PASS | KILLED | B08-VOCI. Recursive Git inventory: 21 blobs, distinct Quantita/Quantità; five-part non-goal and open question present. |
| R71-09 | BLOCKING | FAIL | KILLED | B09-DATES. Dated chain largely present; mandated committed-versus-available correction omitted, F-B02. |
| R71-10 | BLOCKING | PASS | KILLED | B10-PENDING. S1 landed/one affected frozen refusal; #68/#69/#81 pending; five-cell V-5 repair and merge-triggered hook. PR80 freshly confirmed unmerged. |
| R71-11 | BLOCKING | FAIL | KILLED | B11-UNCITED. 117 existing markers resolve in fresh compiler indexes, but required semantic blocks and declaration identity are not enforced; F-B01. Companion R-22 count false at PIN; F-B03. |
| R71-12 | BLOCKING | PASS | KILLED | B12-CLOSURE. Orphan definition, three conjuncts, missing-guarantee classification and no invented theorem. |

Full row review: `ROW-REVIEW.md`, SHA256
e8f166f74c44e10b85382e2f9c1a93a3769d181b81c303c9f2e7aef32351332a.
Fresh campaign ledger: `CAMPAIGN-LEDGER.md`, SHA256
7ba6612109bbfdc4d7b5041aa2e439820ee4e468b45a77b0d618836b82d51485.

## Blocking findings

1. **F-B01 — R71-11: claim association loses declaration identity and excludes
   required semantic blocks.** Frozen v6 claimscan compares trailing basenames,
   independently of the resolver's full identity. `F11-WRONG-DECL` appends a claim
   naming `KelGroups.Proposal` with marker `lean:Reactivegas/Types.lean:Proposal`.
   The latter denotes root Proposal, a distinct two-constructor type, while
   KelGroups.Proposal has three constructors. The real checker exits 0 and prints
   GATE-v6-GREEN. `F11-SOLE-MARKER` changes only the grant/deny row's sole
   `lean:pullCollection` marker to plain pullCollection, preserving its complete
   lookup-first claim. The checker again exits 0, even though AUTH rows are explicit
   mandatory claim blocks under R71-11 and NOTE-010. This is not a demand that a
   parser infer arbitrary prose: the supported domain explicitly includes these
   rows. The current candidate already has ten of twelve authority-table rows,
   the first three pending rows, and the Voci fact without co-located markers.
   **Property class:** a claim must bind its own resolved declaration identity,
   and required semantic blocks must remain in the inventory independently of
   backtick/marker presence. The added-uncited `bal` negative control does reject
   at CLAIM-RED; the unmodified positive succeeds, so these are reached-checker
   survivors, not setup failures. Locations: gate.sh:499-575; state-machine.md:115-129,
   445-452, 518-522. Evidence: F11-WRONG-DECL.log SHA256
   `403d9c73e098955b78862702cfb6a41d9fcbfeeae0d22c7453e7dde58940daee`;
   F11-SOLE-MARKER.log `d3e5f743715bddb2948bd2f8bec0ae65cbdfac8f13c463883fa189966158eed1`.
   The latter log equals CTRL-ORIGINAL's bytes; their different document hashes
   are retained in results.json. Frozen instrument doc-controls.py SHA256
   `4197973c6187dbb8ec0327be4e00252795db8fcc7ff2afb78c25d35747c11194`.
   Limit: static documentary runs with CI/witness execution skipped; not Lean
   mutation kills. Present-citation resolution alone is not completeness.

2. **F-B02 — R71-09: the later committed-versus-available ruling is missing.**
   The mandate explicitly includes this correction in the dated authority record.
   Q-001, “Correction 2” and the following stall rationale (lines 520-557), explain
   that closing discharges already-committed escrow and its paired cassa obligation;
   withdrawal consumes uncommitted backing. They distinguish this from the earlier
   cash-moving/cash-neutral explanation and explain protection of the referente
   and fairness to members outside the sealed purchase. The final document's L4,
   L5 and 2026-08-26 authority paragraph do not carry that distinction; the authority
   paragraph retains refund/cash wording and the general fairness principle only.
   Its corrected cassa debit does not supply the missing ruling. **Property class:**
   a required authority chronology must preserve the later correction's substantive
   distinction, not only its date and some nearby conclusions. Locations:
   state-machine.md:237-261 and 475-485; spec.md R71-09. Evidence: retained full
   candidate text plus Q-001 snapshot SHA256
   `98837654cdf99505d1df093432a8c80d24c67727618f2b0d2864a8a20ded193a`.
   Limit: this is a mandate/content omission, not a new economic defect, and not
   a claim that v6's explicitly bounded token predicates prove natural-language truth.

3. **F-B03 — R71-11 sentence truth: the companion's R-22 row states a stale
   validation-error count.** kelgroups-vote-machine.md:35 cites
   KelGroups.ValidationError and says “The ten errors”. At this PIN,
   Validate.lean:7-18 declares eleven, including reservedKey; the latter is a live
   result of validateDirectAdmission at line 146. The fresh T2 compiler-produced
   Validate.ilean contains eleven constructor definitions, not merely eleven
   source-text hits. The row provides no qualification that its count excludes
   the later reserved-key extension or describes only a historical subset.
   **Property class:** current-pin declaration claims must preserve the actual
   extent or explicitly bound a historical subset. Evidence:
   validation-error-compiled-definitions.json SHA256
   `beb4f35358f4a29b280975ddbe14e27dbbdf6d67135dd5ed29146bf0e9e93917`;
   original compiled index retained in evidence/compiled-indexes/KelGroups.Validate.ilean.
   Limit: the model's extra error is not a defect. No companion prose was changed
   in this audit; any repair beyond the markers-only fence needs commissioning
   owner disposition.

## Failure modes altered

none altered -- checked: exactly two Markdown files changed; Lean is blob-identical
to PIN; no consumer contract, runtime source, resource acquisition, background
operation, synchronization primitive or degradation path moved. The ignored gate
copy preserves the authoritative bytes. Documentary errors affect descriptions of
existing behavior. Cold discovery and the real post-build .lake exclusion were
checked; the synthetic poison is excluded while a second real source is counted.

## Verification receipts

Paths below are under `evidence/`. Full stdout/stderr and command receipts are retained.

| Command | Exit | Duration | Evidence SHA256 |
|---|---:|---:|---|
| `env SKIP_CI=1 REPLAYS=0 bash ./gate.sh`, cold | 0 | 12490 ms | T1-cold-static.log `d3e5f743715bddb2948bd2f8bec0ae65cbdfac8f13c463883fa189966158eed1` |
| `bash ./gate.sh`, full v6 including just ci, replays, live witnesses | 0 | 173017 ms | T2-full-gate.log `1f0bb777b0a7232f5b90d1538f7947dc62298e0c017916f9faac79a3ef78ca6d` |
| `python doc-controls.py`, twelve own mutants + two survivors + positive | 0 | 51716 ms | doc-controls-final.log `cb24ac633770a4b54429d8aafd319b9849773213517fb5240df070ca57b857eb` |
| `bash lake-exclusion.sh`, synthetic and real post-build source discovery | 0 | 49 ms | lake-exclusion.log `2eb2b65a767c5842a992705a03d30bd9516a41e45716d344e745b40b3de60d0e` |
| Supplemental compiled reader, attempt 1 | 1 | 5988 ms | citation-witness.log `62476873de10e899b7d33b2d85bcc890a1ff6a4b75728cf08e273946d68e5182` |
| Supplemental compiled reader, attempt 2 | 1 | 6201 ms | citation-witness-final.log `31c4bfd88b09ce291d5eef7412c0b7c7b659dabf6376aba40a5319bb24469a39` |
| Provenance / tracked cleanliness / signature / gate identity | 0 | 34 ms | provenance-final.log `5d8264f417abbc361ae47baf57713a35cbe87b6ffd13272a755ee57c301b3d20` |

T2 executes Haskell build/format/hlint, toolchain/dependency checks, 14/14 inversion
and trace bindings, the intended withheld-backdonate negative control, 27-job Lean
completion, corpus gate, C1-C14/SYNTH and R1-R26. Existing compiler warnings remain
in the log; hlint reports No hints. Local .lake was absent both before and after
T1; the shared Nix store may be warm. Free space before/after T2 was
219228745728 / 219133845504 bytes; shared-host variation is not attributed entirely
to this build.

The supplemental reader errors are auditor-only: unsupported option and section/
private-name handling in attempt 1, private-name surface syntax in attempt 2.
They consumed their two targeted invocations and are not GREEN receipts or
candidate failures. Subsequent T5 reading of existing compiler indexes associates
all 117 markers with their compiled definitions, including the three private ones:
compiled-citation-index.json SHA256
`2d779e8b39008e0d3eee5341bd4fa175fd6e9084e27d6e20e25299038d9c3da5`.
No further compiler/gate invocation was run. The frozen T2 live-witness leg already
passed; its 30-unit journey, deposit triplet, stored rows and role-scope outputs
are also visible in the retained supplemental logs without relabelling those runs.

## Residuals, candidate invariants and onward discoveries

No accepted residuals. No new invariants proposed. No unrelated discovery opened.
All findings bind the supplied FULL mandate and go only to ticket owner %516.
No runtime, deployment, remote CI, or external simulator acceptance is claimed.
#68 remains pending: PR80 OPEN, mergedAt null, head d68a783; receipt SHA256
`747d101577ea1988f5c428da8d5b1e682230a57575b5a78df4b751c5bda7be63`.
The record carries no 11/14 assessment claim. #75/#76/#81 remain explicitly planned.

## Advisories and stopping receipt

The #75/#76 “see pending table” cross-reference has no dedicated rows there,
although nearby prose clearly says planned. The reliance declaration still names
v5 in one invariant; the actual receipt, verified bytes and NOTE-001 bind v6.
Neither was used to weaken the authoritative gate or scope.

MUTATION-CAMPAIGN CLOSED stopped=SET-POINT rows=12 killed=12 residual=0 blocked=0 open=0;
additional_citation_survivors=2; content_findings=3; builds=1/2 targeted=20/20.
No repaired candidate is produced by this seat. One verdict, then COMPLETE.

Build outputs `dist-newstyle` and `lean/.lake` in this audit worktree were retired
at COMPLETE; 78773939 logical bytes removed (du -sb, not a physical-disk saving claim).
Compiler indexes, source fixtures, instruments and logs are retained.
AUDIT-WORKTREE-RETIRABLE: `/code/reactivegas-issue-71-audit-b1`.
