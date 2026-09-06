# S4-B final candidate — FULL STATIC audit

**Terminal verdict: AUDIT-FINDINGS.** Comment containment and preservation of
non-comment program bytes are established. The dated ruling establishes the
current arbitrary-state Reach consumer classification. The complete required
source-sensitive/final-evidence boundary is not established: the supplied final
CI is a dirty-tree, pre-commit run without a retained source-input binding to the
final tree. There is also an unfulfilled closure-map recording requirement.
Neither finding asserts a false theorem or a broken mirror implementation.

Candidate: `04eb6c7d9aeb2a3602fca5ece14cbc033221cb43`, tree
`caaa0488f39a6afb2553680a11fd6bfd86d1c90b`.
Prior candidate: `94bb7bb64324a48f7361252556b4d15e45b3923f`.
Accepted base: `3590c0015b84fd58004bf6fb44dd18b107304c48`.
Scope is the entire eight-commit unaccepted range, the original S4 contract and
admitted amendments, the dated ruling, current/historical closure limits,
prior frozen execution evidence, and final owner CI evidence.

This is one local verdict, with no repair round. **Project executions: zero**:
no build, elaboration, project probe, mutant, checker invocation, LSP request,
Nix evaluation, or CI rerun. Mechanical parsing, Git reads and hashing are the
only executed audit instruments. No source edits, author contact, remote action,
acceptance or issue closure occurred.

## Authority, identity and chronology

The brief hash is
`4ae525975afce63253809d2fafc341e38644d64e20700ca421b7f16c06b59db5`.
Amendment 1 hash is
`d6a20dc6cfa5e55fabda53e66d06535c3fd5927131af0372a269851ace9f9297`.
Both were read and verified before my own START. Amendment 1 governs the
distinction between the immutable historical finding and the independently
decidable current classification obligation.

All 20 admitted files, including superseded instruments and the complete prior
report/classification, were read before START. Manifest hash
`faa5c2ce6500dd5e03a998edf0ff1880c162b9b8d8b550f265e7fe8c19cb8c8f`
verified 20/20 with no self-entry. The first check was issued from the runtime
directory, where its bare relative filenames did not resolve; the corrected
`sha256sum -c MANIFEST.sha256` from `admitted/` verified all entries. This was a
check-CWD error, not corrupt evidence. A second independent byte-hash parser
confirmed the same manifest.

Launch identity was independently observed from `/proc/3437463/cmdline`:
Codex 0.153.2, `-m gpt-6-astra -c model_reasoning_effort=high`, PID/PGID 3437463,
parent shell 3435517. Own pane `%576` resolved to
`reactivegas:rg-s4b-final-audit.1`, distinct from commissioner `%503` and
terminal predecessor `%571`. The initial request containing host-wide `ps -eo`
was declined and did not execute. The accepted observation was restricted to
`ps -p 3437463`, `ps --ppid 3437463`, that PID's `/proc/cmdline`, and own-pane
`tmux display-message`. No host-wide process enumeration was performed.

Separately, this session's own persisted **turn_context and collaboration
control settings** report active `gpt-6-astra`, effort `high` at
`2026-09-06T06:39:46.969Z`, session
`01a0756f-c8cd-7581-b8cd-48b798c0b8c6`. Both model fields agree, independently of
argv. The preceding turn at `06:38:21.380Z` also reports those values. These
records are captured in `evidence/session-model.jsonl` and were reread before
report assembly. This is session-state evidence, not a claim of opening the
interactive model menu or measuring an unrecorded switch interval.

My first STATUS line states both launch and active-session identities, the
amendment, manifest, clean detached candidate and zero oleans. Journal timestamps
come from live local `date -u`; the owner-supplied UTC was not needed. The
dedicated window, exceptional submission and static-only scope are express
commissioned exceptions; generic skill mutation/build requirements do not grant
execution here.

## Independently established source facts

`instruments/static-boundary.mjs` enumerated all 269 tracked blobs and modes,
verified every current worktree file against its final Git blob, and compared
the two commits without Git external diff/textconv. Exactly two files differ;
the other 267 blobs and all file modes are identical. The repair is +6/-4.

| Changed file | Exact boundary | Result |
|---|---|---|
| `lean/Reactivegas/Mirrors.lean` | Existing `/-!` module documentation, opening line 4; changed bullet at 29–32; closing delimiter moves from 33 to 34 | One documentation span changes; every byte outside comments is identical |
| `scripts/check-lean-mirrors` | Existing `/--` declaration documentation at 152–154, inside the single-quoted `LEAN_EOF` heredoc | One documentation span changes; shell prefix/suffix and non-comment Lean bytes are identical |

The scanner recognizes nested block comments, line comments and strings. It
rejected an unclosed synthetic comment and distinguished synthetic executable
token/string changes and additions outside comments. These controls exercise
the mechanical parser only; no project mutation was created or executed. The
first invocation hit Node `spawnSync git EPERM` under the sandbox; its approved
retry completed unchanged. This setup failure is retained in the audit history
and consumed no project execution allowance.

The exact current source SHA256 values are:

| Source | SHA256 |
|---|---|
| `lean/Reactivegas/Mirrors.lean` | `7fefa8cd8e034c1e2f5a353d28270d7a68e940c7be5d0f52eabaca0d6193e1fb` |
| `scripts/check-lean-mirrors` | `f17ecfb0c4d89436261ad202a3b83956fa89392e92d085d309880ae2d31745e6` |

The complete base-to-final range adds two mirror modules, the checker, and ten
justfile lines: four files, 948 additions, no deletions. It contains
`189e1ed`, `59309d6`, `0f3ad01`, `4d0a324`, `b667648`, `ba623667`, `94bb7bb`,
`04eb6c7d`, in ancestry order. Old model definitions, old theorem statements and
proofs, Invariants files, imports, Lake configuration, CI YAML and design docs
are unchanged relative to the accepted base. The two mirror modules and entire
checker were read, together with their predicate definitions, relevant existing
P01/P07 chain, Reach/Trace boundary and mandatory script/CI consumers.

For the repair itself, **no non-comment executable token, s4bExceptions member,
proof, statement, import, nonce logic or check wiring changed**. The exception
array remains exactly `KelGroups.Vote.PreservesQuestionSemantics`,
`authorizedStep`, `stalled`, `Reach`. No duplicate P01/P07 mirror, extra
threshold default, equality-on-threshold or new runtime call site appears.

## Generated driver and source-sensitive boundary

The wrapper uses `cat <<'LEAN_EOF'`, so shell expansion does not interpret the
documentation. The delimiter and all outer shell bytes are unchanged. Its sole
later textual replacement is `__TRACKED_MODULES__`; that token occurs once in
both bodies, outside the changed documentation, and the same 29 tracked module
names supply its replacement. There is no delimiter escape, new placeholder,
shell interpolation, or program-body change.

Mechanical extraction and substitution, without invoking the wrapper, yield:

| Driver representation | Old SHA256 | Final SHA256 |
|---|---|---|
| Raw generated source text | `2c9e36260e56d959de7d7acaec0dae5d23e4cf711a2ba81f4dda20192defbd8d` | `083485a67ca7f71cbd69180638ad4c3e7033e50674f6f003a6839ae2b77dd16b` |
| Comment-erased source | `e6d529bb4606bec57c095fbef6df18368336a024e7bab8fd861249d785b62fef` | Same |

The raw driver is therefore **not byte-identical**. This difference is confined
to its intended documentation. It is not evidence of altered checker program
logic or emitted machine instructions.

However, the stronger assertion that these comments have *no source-sensitive
effect at all* is unavailable. `/-!` and `/--` are Lean documentation syntax,
not wholly discarded trivia: local toolchain source shows `elabModuleDoc`
calling `addMainModuleDoc` with text/range, and `addMarkdownDocString` validating
and storing a declaration docstring. The added module-doc line also moves every
subsequent declaration's source line by one. This matters to a real mandatory
consumer: `scripts/check-reactivegas-inversion-coverage:212–257,439–464` obtains
parsed theorem line numbers and compares them with `findDeclarationRanges?` from
the compiled environment. Its wrapper rebuilds sources first. Lake's
`Module.recFetchInput` hashes the complete file contents.

Static inspection supports unchanged logical checking with consistently rebuilt
source/artifacts; it does **not** make old ranges, doc metadata, raw driver
bytes or compiled artifacts interchangeable with final ones. No new manual-link
syntax is introduced, and neither the mirror checker nor the two mirror modules
reads documentation to choose its coverage verdict. I observed no injected
executable behavior. Documentation/range effects are stated as such, not
misreported as proof failure or a demonstrated changed acceptance result.

The unqualified boundary is consequently **BLOCKED**, rather than silently
narrowed to token equality. In particular, final source/compiled consistency
cannot be supplied by relabeling the old execution evidence; the final-CI
binding defect below prevents that evidence bridge too.

## Finding FS-01 — final CI cannot establish the required final-candidate receipt

**Blocking evidence incompatibility.** The admitted original grant
`QUALITY-S4B-TWO-COMMAND-GAP-GRANTED-20260905.md` explicitly requires final CI at
the exact clean committed candidate, with actual exit and raw hashed evidence.
The new ruling requires the final SHA and clean full-CI receipt, and the brief
requires evidence compatibility at this final candidate.

The supplied log, independently hashed in full, is
`b6a16cfe60e6e07204b4191fd0c93dbdb2c8def107715144cc16ebfef946e34d`,
175,819 bytes / 3,098 lines. Its first **two lines** state:
`warning: Git tree '/code/reactivegas-66-s4b' is dirty`.
It contains no candidate SHA, pre/post tracked-input hashes, execution
start/finish envelope or mechanically captured exit field. The owner reports
exit 0; the log itself shows the expected final success markers.

The actual ordering is explicit in the retained author's NOTE-035: run the one
CI operation, **then commit**. The owner's STATUS also reports the run followed
by the commit. The file mtime is `2026-09-06T06:33:56.876352378Z`; commit
`04eb6c7d` has author/committer time `2026-09-06T06:34:03Z`. The log's nonce is
`1788676427576021718`. File/commit timestamps corroborate the written ordering;
they are not a replacement for missing execution-input provenance.

This is **not a charge that the author ignored NOTE-035**: that note ordered
this sequence. It is a discrepancy between the commissioned receipt criterion
and the delivered execution evidence. Post-run cleanliness and a later final
commit do not independently bind all inputs used by the earlier dirty run.
No evidence proves that it built different executable bytes, and no such
claim is made; the required identity/cleanliness claim is unestablished.

The same log demonstrates why an aggregate green is not the missing binding:
it reports 1,285 axiom checks, 185 source/elaborated theorem matches, mirror
19/4/24/2/29, fresh nonce and corpus 5/32/7. Those are real retained log
observations, not an independently witnessed final-SHA CI execution by this
auditor. Its `Up to date` and `Replayed` output also cannot establish a wholly
cold build merely from the phrase “cold-path.”

Evidence: `evidence/owner-final-ci.log`, `final-ci-assessment.json`,
`owner-submission-3.md`, `owner-STATUS.txt`, `owner-NOTE-035.md`, and
`tree-comparison.json`. No extra execution or repair is authorized by this
finding. It concerns provenance and claim strength, not an observed failing
candidate test.

## Finding FS-02 — required ruling/obligation entry absent from the closure map

**Blocking acceptance-record gap.** The admitted dated ruling requires recording
the ruling in both the issue body and closure map. The issue-body readback
satisfies its part. The current local
`/tmp/reactivegas/ms2/e-lean-compliance/handoffs/CLOSURE-MAP.md`, read in full and
snapshotted, contains neither `RG-S4-REACH-20260906` nor the newly required
finite-history correspondence with its #75/#71 ownership. Its “Current owed
list” still gives S5 only retention and `ONWARD-68-INV-01`.

The separate D1 fit report says that the ruling is recorded in the closure map;
the examined map does not substantiate that statement. The ruling, issue body,
amendment and D1 report do retain the obligation, so this is **not** evidence
that nobody owns S5 or that the obligation was waived. It is the narrower
failure to complete the specifically required durable completion-map entry.

The classification decision below does not depend on retroactively repairing
this record. Evidence: `evidence/closure-map.md`, `owner-d1-fit.md`,
`external-snapshot-index.json`, and admitted ruling/issue-body readback.
Commissioner `%503` owns this record and onward routing to desk `%510`.

## Current Reach classification — independently decided under Amendment 1

**The arbitrary-state consumer classification at 04eb6c7d is established:
NOT-REQUIRED for this milestone.** The authority is
**RG-S4-REACH-20260906, issued 2026-09-06**, not the historical “standing boundary,”
failure to synthesize Decidable, general reachability theory or absent callers.
Both replacement comments accurately cite that dated ruling. The module text
expressly rejects both undecidability and the absent-callers inference; the
checker text cites the ruling and rejects undecidability without making an
absent-callers argument. Neither presents the ruling as pre-existing authority.

The original axes remain separate: executable arbitrary Reach decision is still
NOT-ESTABLISHED by retained bounded evidence. The phrase “NOT-EXECUTABLE,
bounded” does not become a mathematical impossibility theorem. No new decision
procedure is observed or inferred.

The actual Reach definition has boot at `State.empty` with comune exclusion,
and trans on successful `stepEvent` under fixed view/auth. Trace accepts an
arbitrary initial state and `emitSteps` preserves state on refusal. Integrated
apply can change the view. The ruling accurately distinguishes these objects.
It does not establish a bridge or authorize one here. Validation of one supplied
history is not existential reachability decision.

**Historical F-001 at 94bb7bb and its terminal AUDIT-FINDINGS remain immutable.**
This current classification decision does not correct, overturn, or re-date that
prior result. The separate finite-history correspondence remains **OPEN under
S5**, retaining **#75 replay and #71 reporting**, with genesis, fixed view/auth
and refusal-preservation premises to be assessed. #66 and the milestone remain
incomplete.

## Full-contract coverage and retained evidence

The prior manifest
`eb05530995bde80878203222a4430b1afce1d05002cb4bd60d59d60e75f40cc0`
was independently checked **665/665, no self-entry**. A separate parser checked
every digest again, matched all 82 command records to their receipts and both
raw streams (**164 stream checks**), and retained all 44 distinct atom rows,
their one-edit counts, original proof locations, errors and original source
hashes. This is evidence-integrity review, not execution replay or new kills.

The prior actual substantive count is 12 and targeted count 73: the 82-row sheet
contains 11/71, plus M1-S, failed M1-T and M1R-T. Campaign actual remains 18/132
against 18/139. Prior author 18/52 remains spent; the exceptional third
submission adds the one authorized substantive operation to 19/19 and zero
targeted operations. No unused historical allowance is granted to this audit.

Prior S10 executed **2026-09-06T05:50:26.431Z–05:53:21.856Z**, exit 0,
at **94bb7bb**. Its stdout hash is
`c0d10f287764f222b46f804aba22ffd62ab6e08bb478cbb40b687a6e02ee54fa`;
stderr is
`b52f164044a7f30051a39b242933fca8827238913d17d2123a2d3054b4df00f0`.
The prior inventory/axiom and mutation executions retain their individual
timestamps in `prior-command-integrity.json`. None becomes an execution at
04eb6c7d. Prior source hashes, line numbers and oleans retain their original
identities even where non-comment statements remain identical.

| Contract rows | Static final-candidate assessment and actual execution provenance |
|---|---|
| Original Phase A / R7, both axes | All 24 prior predicate identities retained; current Reach consumer classification established by new authority. No new compiled census claimed; prior 3,478 declarations / 29 modules / 24 predicates remain 94bb7bb measurements |
| R1–R3, R5, R15–R17 | Complete four-file diff and Git blobs establish fence, old-definition/statement preservation, no new runtime call sites, new modules only and additive justfile wiring |
| R4, v3.1 P01/P07 | Original expression correspondences remain value-parametric/inline. Two membership helpers and existing close/inversion chain are retained; no duplicate theorem or mirror added |
| R6, R14 | Callable threshold and generic DecidableEq only in new K5 counterpart/theorem remain. Finite list/lookup proofs have arbitrary-state statements, duplicate-key first lookup and absent-key zero; no new well-formedness premise |
| R8–R10 / C1–C4 | Mandatory missing-counterpart, missing-theorem and present-but-disabled checker evidence retained at 94bb7bb, including S11 after failed S02. Identical final invocation/nonce logic; no newly executed controls |
| R11–R12 / C5–C23 | All 44 atom records and distinct streams retained; P01 selected-helper and P07-negR selected-inversion evidence remains at 94bb7bb. No first-error credit added; relatum survivors are never relabeled implementation kills |
| R13 / C24–C26 | Prior cold S10/final inventory/axioms/panic controls retain exact scope and hashes. Current full final receipt is blocked by FS-01; no transfer of compiled artifacts or dates |
| R18 | Separate substantive/targeted accounting retained; four prior setup failures remain spent. This audit has zero project execution and one mechanical-parser sandbox setup failure |
| Ruling closure-map requirement | Issue-body readback present; closure map incomplete, FS-02 |
| Load-bearing comment boundary | Comment/non-comment and wrapper-substitution boundary established; unqualified source-sensitive invariance not certified, as explained above |

The final mirror source preserves the 19 correspondence identities and 17 new
Bool counterparts. Required exceptions V4, P11 and stalled retain the prior
definitional/evaluation evidence; Reach's consumer authority is new and dated.
The prior S4 classification remains historical verbatim. The current change in
its P13 consumer-axis disposition is stated here rather than rewriting that file.

## Historical and onward limits — all retained

| Obligation | Disposition |
|---|---|
| Historical F-001 | Immutable finding and prior verdict at 94bb7bb; current classification assessed separately above |
| S5 finite-history correspondence | OPEN, owned, #75/#71 retained; no bridge proved, no absence-of-bridge inference from spelling searches |
| H-01 P07 | OPEN: historical single-variable isolation cannot be established from the old Step diagnostic; later clean overlay does not reconstruct history |
| H-02 census sortUndecided | OPEN: source-verified-not-executed historical failure path; clean zero and independent UnknownInventory are not its execution |
| H-03 ba623667 | OPEN: recovered-from-snapshot after overwrite, never never-overwritten provenance |

Original S5 retention and inversion-exactness obligations, #71 content, S3's
separate campaign, and other inherited closure-map limits remain where owned.
No remote workflow, full milestone completion, or future-source assurance is
established. The six commissioning record corrections remain corrections at
their recorded scope, including the invalid 42-character SHA, seven old commits,
distinct P07 failure/retry logs, spent submissions, prior retry-grant chronology
and the 3,117-line O6 binding. None is rewritten as new execution.

## Stopping receipt

Finite static scope completed: all admitted text, entire unaccepted diff and
affected source/wiring, dated ruling/current classification, all 665 frozen
evidence digests, 82 receipt pairs, 44 atom records, final local CI and closure
records. No additional source or runtime investigation is silently promised.
The blocked stronger claim and two findings are returned without executing past
the zero allowance or opening a repair round.

The candidate remains clean and detached, with zero oleans in this audit
worktree. No build tree was created or retired. `ONWARD-DISCOVERIES.md` carries
the named open obligations to commissioner `%503`; only that commissioning chain
owns further disposition. The final manifest hashes this report and all locally
retained evidence without including itself.
