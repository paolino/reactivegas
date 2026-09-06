# S4-B independent audit report

Terminal verdict: **AUDIT-CONTRACT-BLOCKED**

Updated after commissioning NOTE-001, POINTER-1788645204-2481808.
The amendment was read in full and acknowledged without restart. It resolves
the operative-instrument and mutation-permission gaps. All eleven entries in
the expanded admitted manifest verify OK; the operative v2 full hash matches.
The remaining block is dispatch/campaign binding.

Candidate: `189e1ed306f8f8e8bcdd11eeab4fc5657a518fc8`
Base named by commission: `3590c0015b84fd58004bf6fb44dd18b107304c48`
Runtime: `/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s4b-codex`
Worktree: `/code/reactivegas-66-s4b-audit`
Pointer: `POINTER-1788644987-2475968`
Requested scope: the complete original S4 requirements, including both axes of
classification, required-consumer authority, correspondence/proofs, and commit
provenance. The five emphasis areas and nineteen correspondence rows are not
the full denominator. Severity defaults to BLOCKING where undeclared.

This is a preflight disposition, not a semantic finding against the candidate.
Candidate soundness remains unjudged. No owner assertion closes an audit row.

## Preflight result and stopping reason

All eleven current admitted files pass their supplied MANIFEST.sha256. The
brief, commissioning amendment, submission, owner brief, grant, original S4
contract, v1/v2 requirements, three normative amendments and command
reconciliation were read. NOTE-001 now binds v2 as operative authority, with
its amendments and the original S4 contract. v1 is history; only requirements
explicitly incorporated by v2 carry forward.

The required commit-auditor skill says, before inspecting the candidate,
"require concrete, distinct `ticket_owner_pane` and `owner_pane` values plus
the named ticket window in the brief". It also says "Refuse the audit unless"
the packet binds the campaign ledger path and remaining build budget, alongside
the other listed authority fields.

The referenced requirements document names parent %503, but the packet does
not bind the implementation-owner pane, ticket window,
current audit submission number, campaign ledger path/carried
auditor spend, or complete ticket-wide ceiling-raise count. The actual audit
pane resolves to its own named audit window; that does not establish the
required relationship to the two owner seats. No other seat was contacted.
Live discovery of an apparent owner elsewhere would not itself supply missing
commissioning authority.

The original S4 contract now establishes the restricted auditor family set:
codex or grok, excluding muse and claude; the selected live Codex process fits.
The gate-binding question in initial Q-001 is resolved by NOTE-001's operative
v2 designation: just lean / just ci plus the checker at the frozen candidate
are the named executable mechanism. No additional gate.sh checkpoint is sought.

NOTE-001 expressly permits temporary mutations in this detached audit tree,
with retained raw mutants and restoration evidence. This supersedes the prior
unqualified no-edit sentence for this tree. Owner/reference candidate and
admitted instruments remain unwritable. No mutation has occurred.

Question and wake condition: `questions/Q-001-dispatch-campaign-bindings.md`.
No START was appended because the required pre-START checks are not satisfied.
ACK and PREFLIGHT evidence are retained instead, with timestamps later than
brief mtime. No audit command plan is represented as finalized: the carried
campaign state and dispatch binding must be established before reconciliation.

## Live identity — inspection

At 2026-09-05T21:51:38Z:

- Auditor PID = 2474577; PPID = 2474474; PGID = 2474577.
- Full live argv:
  `/nix/store/nqhk0522q8ncygwwx054iq76ckcjll82-codex-0.153.2/libexec/codex -m gpt-6-astra -c model_reasoning_effort=high --dangerously-bypass-approvals-and-sandbox`.
- Process start: 2026-09-05 22:49:16 +0100.
- Process cwd = supplied audit worktree.
- tmux = session reactivegas, window @165 / rg-s4b-audit, pane %561;
  pane PID 2474474.
- HEAD matches the full candidate SHA; `git symbolic-ref -q HEAD` exits 1
  (detached); `git status --porcelain=v1` is empty.
- `find . -type f -name '*.olean' -print | wc -l` = 0.
- Brief mtime = 2026-09-05 22:48:44.305517979 +0100.

These are current identity facts only. They do not certify historical probe
restoration, candidate diff semantics, or independence from unidentified seats.

## Established input checks

| Row | Verdict | Command | Observation | Method |
|---|---|---|---|---|
| Admitted packet manifest | CLOSED | `cd admitted && sha256sum -c MANIFEST.sha256` | Eleven files OK against all supplied full hashes after NOTE-001 | inspection |
| v1 requirements identity | CLOSED | `sha256sum /tmp/reactivegas/ms2/e-lean-compliance/handoffs/S4B-ACCEPTANCE-INSTRUMENT-v1.md` | 44c48239d9b62ef7ec896d7fe4964159c4b23f117865c8f9dd4bd3ebeae29501, matches owner-brief prefix | unchanged-input with explicit byte identity |
| v2 requirements identity | CLOSED | `sha256sum /tmp/reactivegas/ms2/e-lean-compliance/handoffs/S4B-ACCEPTANCE-INSTRUMENT-v2.md` | 2214ff8a0d25f47afded7b7215e9873b5a237d97caea55eb72b1d8f884c5ca4f, matches submission prefix | unchanged-input with explicit byte identity |
| C26 archived log identity | CLOSED | `sha256sum /tmp/reactivegas/ms2/e-lean-compliance/commit-owner-s4b-muse/handoffs/evidence/s4b-c26.log`; `wc -l` on the same path | 699792e4efa56b354bb3c3173751e538bc1739adbe91893199ec93b688978841; 17,987 lines | unchanged-input with explicit byte identity |
| Current worktree identity/hygiene | CLOSED | `git rev-parse HEAD`; `git symbolic-ref -q HEAD`; `git status --porcelain=v1`; olean count above | Exact candidate, detached, clean, zero oleans | inspection |
| Historical restoration after every control | OPEN | No historical verification command executed | Current cleanliness does not establish prior restoration events | inspection |

The archived C26 copy is the evidence artifact used. Its hash agrees with the
brief; no divergence was observed for that file. The other 185 archived files
were not verified, and no full-archive identity conclusion is made.
Hashing retained output is not new execution, nor does it independently
establish that the historical command ran cold at the claimed committed tree.

## Nineteen correspondence rows

For each row below the required separate control was not run. The observation
is a preflight block, not a falsification result; no new-execution row exists.

| Row | Named control | Verdict | Establishing command / observation | Method |
|---|---|---|---|---|
| P01 comune | C5R | OPEN | None executed; Q-001 blocks dependent execution | inspection |
| P02 conservation | C6 | OPEN | None executed; Q-001 blocks dependent execution | inspection |
| P03 solvent | C7 | OPEN | None executed; Q-001 blocks dependent execution | inspection |
| P04 insolvent | C8 | OPEN | None executed; Q-001 blocks dependent execution | inspection |
| P05 uniquePledges | C9 | OPEN | None executed; Q-001 blocks dependent execution | inspection |
| P06 allUniquePledges | C10 | OPEN | None executed; Q-001 blocks dependent execution | inspection |
| P07 permissionToClose | C11R | OPEN | None executed; Q-001 blocks dependent execution | inspection |
| P08 escrowHeld | C12 | OPEN | None executed; Q-001 blocks dependent execution | inspection |
| P09 governanceEnacts | C13 | OPEN | None executed; Q-001 blocks dependent execution | inspection |
| P10 doubleEntry | C14 | OPEN | None executed; Q-001 blocks dependent execution | inspection |
| P12 canCloseGroup | C15 | OPEN | None executed; Q-001 blocks dependent execution | inspection |
| K1 PendingWellFormed | C16 | OPEN | None executed; Q-001 blocks dependent execution | inspection |
| K2 MembersCoherent | C17 | OPEN | None executed; Q-001 blocks dependent execution | inspection |
| K3 PendingCoherent | C18 | OPEN | None executed; Q-001 blocks dependent execution | inspection |
| K4 WellFormed | C19 | OPEN | None executed; Q-001 blocks dependent execution | inspection |
| K5 Enacts | C20 | OPEN | None executed; Q-001 blocks dependent execution | inspection |
| V1 QuestionClean | C21 | OPEN | None executed; Q-001 blocks dependent execution | inspection |
| V2 SweepReady | C22 | OPEN | None executed; Q-001 blocks dependent execution | inspection |
| V3 VoteWellFormed | C23 | OPEN | None executed; Q-001 blocks dependent execution | inspection |

P01/P07's claimed relatum-defect sensitivity and the corresponding limitation
on expression-body sensitivity remain unaudited. No aggregate result or grant
is credited to any correspondence row.

## Mandatory path and assurance controls

| Row | Verdict | Required command; observation | Method |
|---|---|---|---|
| C1 clean baseline | OPEN | `nix develop --quiet -c just lean` not run | inspection |
| C2 counterpart absent | OPEN | Separate mandatory `just lean` control not run | inspection |
| C3 theorem absent | OPEN | Separate mandatory `just lean` control not run | inspection |
| C4 ineffective checker while present | OPEN | Separate mandatory `just lean` control not run; receipt enforcement untested | inspection |
| C24 final-tree proof axioms | OPEN | Final-tree `lake env lean` axiom driver not run | inspection |
| C25 final-tree totality/panic observations | OPEN | Final-tree driver and stream checks not run | inspection |
| C26 cold final CI | PARTLY | Archived hash/line count verified above; `nix develop --quiet -c just ci` not run | unchanged-input with explicit byte identity |

C4's requested interpretation is clear in the admitted grant: execution
enforcement only. C2/C3/C5-C23 must establish semantic sensitivity independently.
Whether candidate behavior implements that division, and whether any historical
exit-127 shortcut contributed evidence, remain OPEN. No exit-127 result is
credited here.

## Requirements ledger

Each row remains open for actual candidate audit; reading requirements only
establishes what must be checked.

| Row | Obligation | Verdict | Command / observation | Method |
|---|---|---|---|---|
| R1 | finite scope and original definitions preserved | OPEN | No candidate verification executed; Q-001 preflight block | inspection |
| R2 | no new runtime monitor or coordinator behavior | OPEN | No candidate verification executed; Q-001 preflight block | inspection |
| R3 | new-module placement | OPEN | No candidate verification executed; Q-001 preflight block | inspection |
| R4 | P01/P07 relate existing expressions | OPEN | No candidate verification executed; Q-001 preflight block | inspection |
| R5 | generic equality assumptions and original theorem preservation | OPEN | No candidate verification executed; Q-001 preflight block | inspection |
| R6 | callable threshold policy | OPEN | No candidate verification executed; Q-001 preflight block | inspection |
| R7 | compiled inventory and per-identity reconciliation | OPEN | No candidate verification executed; Q-001 preflight block | inspection |
| R8 | mandatory counterpart-absent detection | OPEN | No candidate verification executed; Q-001 preflight block | inspection |
| R9 | mandatory theorem-absent detection | OPEN | No candidate verification executed; Q-001 preflight block | inspection |
| R10 | effective-disable detection | OPEN | No candidate verification executed; Q-001 preflight block | inspection |
| R11 | well-typed per-identity falsification | OPEN | No candidate verification executed; Q-001 preflight block | inspection |
| R12 | distinct failures without masking | OPEN | No candidate verification executed; Q-001 preflight block | inspection |
| R13 | final-tree axioms and totality | OPEN | No candidate verification executed; Q-001 preflight block | inspection |
| R14 | arbitrary lookup semantics including duplicate and absent keys | OPEN | No candidate verification executed; Q-001 preflight block | inspection |
| R15 | no unruled well-formedness premises | OPEN | No candidate verification executed; Q-001 preflight block | inspection |
| R16 | exact path fence | OPEN | No candidate verification executed; Q-001 preflight block | inspection |
| R17 | additive own-recipe justfile edits | OPEN | No candidate verification executed; Q-001 preflight block | inspection |
| R18 | complete spend ledger and classification | OPEN | No candidate verification executed; Q-001 preflight block | inspection |

## Budget and coverage receipt

Additional original-S4 obligations remain OPEN: the full live Prop denominator
and both classification axes per identity; definitional identity versus proved
equivalence; logical Decidable evidence versus evaluated executable decision;
bounded Reach classification without unsupported undecidability; authority for
required/unimplemented/not-required consumers; exact paths and #68/#69
dependencies; no duplicate mirror where a consumer already exists; and owned
completion-map entries for unresolved required consumers/correspondences.
Method: inspection of the admitted original S4 contract. No compiled
classification command was executed. The selected nineteen rows cannot close
these obligations by implication.

Auditor actual spend: **0/8 substantive; 0/60 targeted** this context.
No build, failed build, warm repeat, single-file elaboration, or driver probe
was run. This is a measured local spend, not an assertion about an unbound
carried campaign ledger.

The admitted grant explicitly leaves the auditor's original 8/60 total across
both submissions unchanged and raises the separate owner substantive ceiling
6 -> 8. The owner's claimed S1, C1, C1r, C1g, C2, C3, C4, C26 and 42 targeted
calls have not been independently reconciled. No failed owner call is refunded
or relabelled by this report. No budget overrun is alleged.

The mandatory execution floor visibly includes five separate substantive rows:
C1, C2, C3, C4, C26. The nineteen named falsification rows require nineteen
distinct targeted observations, plus positive/typing, compiled inventory,
statement, final axiom and totality work as determined by the actual instrument.
This is a lower-bound inventory, not a reconciled runnable command plan or
permission to spend. No distinct row was compressed into another.

Coverage: 0/19 correspondence controls reproduced; 0/5 mandatory-path commands
run; 0 final-tree proof drivers run; 18/18 requirements remain OPEN.
Stopping reason: authority preflight, before START. No semantic coverage,
proof cleanliness, mutation adequacy, base ancestry, or acceptance claim.

## Boundaries and return

Only local runtime artifacts were written. The candidate was not edited,
staged, committed, pushed, merged, or repaired. No PR, issue, comment, gist,
deployment, browser composer, pane input, or communication to another seat
was used. No build tree was created or retired.

The commissioning owner receives this report and Q-001 through local files.
Candidate acceptance remains outside this seat's authority.

## Retained preflight receipts

`evidence/preflight-identity.txt` repeats the identity checks at
2026-09-05T21:57:22Z with the same candidate, clean detached tree and zero oleans.
`evidence/admitted-manifest-check.txt` retains all eleven OK observations and
operative v2 hash; `evidence/c26-archive-identity.txt` retains C26 hash/line count.
An attempted receipt capture ran Git from admitted/ and failed before its
corrected capture; `evidence/preflight-identity-wrong-cwd.txt` is retained and
excluded as identity evidence. These were free read-only Git/file inspections,
not builds, elaborations, or driver probes; no charged failure was hidden.
