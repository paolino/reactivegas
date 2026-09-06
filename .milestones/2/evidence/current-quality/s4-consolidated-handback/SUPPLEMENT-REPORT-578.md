# S4 static final evidence supplement

**Terminal verdict: AUDIT-FINDINGS.** The new log contains real evidence of the
source-sensitive mandatory consumers operating, including a source/range check
that covers the repaired `Reactivegas.Mirrors` module. The owner's leaf-module
residual is therefore not an unobserved-consumer blocker. However, the complete
commissioned evidence obligation remains unestablished: the receipt does not
capture a second HEAD/tree identity after execution. The amended closure map
also incorrectly locates superseded current wording in its historical section.
These are the specific remaining blockers, detailed below; neither asserts a
false theorem, bad mirror implementation, or observed stale olean.

This is a **static evidence supplement**, not a newly executed full audit or a
second mutation campaign. Project executions: **zero**. No candidate edits,
author/sibling contact, repair dispatch, remote operation, acceptance, issue
closure, push or merge occurred. The scope and prohibition are in
`evidence/brief.md:20–47,99–101,144–156`; the post-START amendment is in
`evidence/AMENDMENT-1-CURRENT-STATE-AND-ADDITIVITY.md`.

## Identity, authority and frozen evidence

Commissioner: quality epic owner `%503`, #66 / milestone 2. Auditor: own fresh
root `/tmp/reactivegas/ms2/e-lean-compliance/supplement-auditor-s4b`, session
`01a07594-146c-77f0-b1c8-9d6ace46b0ea`. The live persisted turn context reports
`gpt-6-astra`, effort `high`, with this cwd; this was checked before START and
again after continuation (`evidence/live-identity.jsonl`). Own pane readback is
`%578 reactivegas:rg-s4b-supplement.1`, distinct from commissioner `%503`
(`evidence/seat.txt`). This is not a launch-flag inference. START was written
at `2026-09-06T07:18:19Z`; both later pointers were acknowledged at
`07:27:42Z` in the append-only journal.

Candidate `04eb6c7d9aeb2a3602fca5ece14cbc033221cb43`, tree
`caaa0488f39a6afb2553680a11fd6bfd86d1c90b`, accepted base
`3590c0015b84fd58004bf6fb44dd18b107304c48`. Git identity and clean status were
read from the read-only `/code/reactivegas-66-s4b-final-audit`. All **269**
tracked worktree files match their candidate Git blob identities; the eight
commits and four-file base range are retained in `evidence/candidate-blobs.json`,
`candidate-commits.txt`, and `base-range-files.txt`. This verifies today's audit
source, not an unrecorded state of the author's earlier execution.

Original manifest: **11/11** verified before reading its payloads. Its actual
filename is `INPUTS-MANIFEST.sha256`, despite the brief's shorter name.
Amendment manifest: **3/3** verified before reading its payloads. The first
amendment check used the wrong relative directory and failed to find the
manifest; the corrected check from `inputs-amendment-1/` passed. No project
operation was involved. Both sets were independently byte-hashed again by
`evidence/collect-static.mjs`; results are in `evidence/input-integrity.json`.

Required prior report SHA256:
`43db90494fbad83282092d388382651d9f2d56e7aefe30da7b2c66e08443fe9c`.
The supplied prior **74-entry** manifest and check record were read, and all
74 underlying files were independently rehashed against the original frozen
root; both the shell result and separate parser result agree
(`evidence/prior-manifest-check.txt`, `input-integrity.json`). This is integrity
review, not re-execution of its 665-file historical campaign. The report retains
its terminal `AUDIT-FINDINGS` provenance and original full-contract scope
(`evidence/inputs/PRIOR-FULL-AUDIT-REPORT.md:1–22,241–322`).

All citations below are to retained evidence. `source-…txt` files are exact
candidate source copies; `toolchain-…txt` files are exact local Lean 4.25.0
toolchain source copies. Original paths and SHA256 values are in
`evidence/snapshot-index.json`. No external documentation or network claim is
needed to interpret this pinned source.

## FS-01: what the clean-final receipt actually establishes

The preserved clean log hashes to
`fbc50e0d75d365f3d0a60d5641d24c58d65f3270dd70f81b7051072164818564`, matching
the receipt, original manifest and my independent complete-stream hash. It is
175,711 bytes / 3,096 lines. The superseded log remains
`b6a16cfe60e6e07204b4191fd0c93dbdb2c8def107715144cc16ebfef946e34d`,
175,819 bytes / 3,098 lines. Dirty warnings number **0 versus 2**. This is
corroboration of cleanliness, not a substitute for the identity envelope
(`evidence/log-analysis.json`; both logs are retained under `evidence/inputs/`).

The receipt states the exact candidate/tree, base, cwd, command
`nix develop --quiet -c just ci`, start `07:05:25Z`, exit **0**, finish
`07:07:14Z`, and empty before/after porcelain blocks. Its milestone line
numbers match the raw log. The elapsed interval is 109 seconds, and completion
is later than the prior terminal verdict at `07:06:01Z`
(`evidence/inputs/S2-CI-final-clean.receipt.txt:1–23`; prior report authority
retained above). It is admitted here only, never backdated into the prior audit.

**Remaining blocker FS-01-P: post-execution candidate identity is not captured.**
Receipt lines **2–3** contain the only HEAD/tree pair; they precede the before
block and the command. Lines **12–13** contain only an empty porcelain result.
Cleanliness does not identify a commit: a clean checkout of another commit also
has empty porcelain output. The raw log does not add a post-execution HEAD/tree
pair. Current audit-worktree identity and the consolidated owner's assertion
are not that missing observation. Accordingly, the brief's explicit
before-and-after candidate identity requirement is not established by these
frozen inputs. No checkout change is alleged.

The exit field is retained and consistent with all success markers. The frozen
packet does not include the command that assembled the receipt, so I cannot
independently prove its shell capture order or whether it captured the CI
process status rather than a pipeline's last command. I do not relabel a
handwritten-looking field as independently witnessed execution. This is an
additional limit on the envelope, not evidence that the reported exit is false.
The missing discriminator would be an already-retained execution envelope
recording the CI process exit and both pre/post HEAD/tree identities. None of
the 11 original or 3 amended inputs supplies it. **No run is requested or
authorized.** The exact controlling expectation is
`evidence/brief.md:71–73`, not a new cold-build requirement.

### Mandatory consumers and the four independently challenged assessments

**(a) Cache assistance is observed; initial coldness is not decided by 17
replay lines.** All 17 visible progress records say `Replayed`; three visible
summaries report 27, 42 and 42 jobs (`evidence/log-analysis.json`). But these
are not all Lake invocations in CI. Before them, both inversion calls and the
axiom wrapper build every tracked module, redirect successful build output to
temporary files, and remove those files on exit; the trace-agreement wrapper
calls the inversion wrapper once more. Thus there are **four preceding
all-module build invocations** in the source path, whose successful build
streams are not in the frozen outer log
(`source-justfile.txt:57–69`; `source-scripts_check-reactivegas-inversion-coverage.txt:60–114`;
`source-scripts_check-lean-axioms.txt:87–138`;
`source-scripts_check-trace-coverage-agreement.txt:49–60`).

The claim that Lake *only* prints progress for diagnostics is also overbroad.
`toolchain-lake_Lake_Build_Run.lean.txt:112–143` prints a job when it has output
**or**, in non-ANSI progress mode, its action meets the threshold; normal mode
uses threshold `fetch` (`:235–253`). Quiet/ANSI modes alter visibility. Therefore
absence of a Built line is not a universal rule about silent compilation.
More decisively, replay in a later build cannot exclude compilation in an
earlier suppressed build **within this same CI invocation**, including modules
that emit diagnostics. The owner's statement that no diagnostic-bearing module
compiled fresh in the run is not established. Correct bounded conclusion:
cache reuse occurred; neither a wholly cold execution nor an initially warm
Lean build tree is independently established. I grant no new cold-build credit.

**(b) Replayed `#print axioms` output adds no freshly evaluated print evidence;
“no new axiom evidence” for the whole receipt is too broad.** The named
Invariants print lines sit under replay headers at log **2764–2770,
2927–2933, 3086–3092**. Lake re-emits saved diagnostics
(`toolchain-lake_Lake_Build_Common.lean.txt:255–270`). Those particular prints
are cached.

Separately, log **38–2642** is a newly generated `AxiomGate.lean` driver, invoked
with `lake env lean`, that calls `collectAxioms` for the discovered compiled
theorem set. It prints **29** loaded project modules, including
`Reactivegas.Mirrors` at **67**, **1,285** distinct theorem identities,
**1,287** walk occurrences and **1,285** axiom results, then `axiom-gate: ok`.
Source: `source-scripts_check-lean-axioms.txt:128–138,218–318`. These are fresh
queries of loaded compiled artifacts, not saved module diagnostics. They do
not establish a fresh `.lake` or transfer old cold-sweep provenance. The six
earlier inversion axiom lines bring the total `axioms …` records to 1,291;
none names a non-permitted axiom (`evidence/log-sanity.json`).

**(c) The no-consumer residual is settled to the source-sensitive boundary,
while compile-versus-reuse timing remains unknown.** No tracked Lean module
imports `Reactivegas.Mirrors`; its source imports Predicates and Step and has
no explicit diagnostic command. That limited import-graph observation is
correct (`evidence/tracked-modules.json`, `log-analysis.json`,
`source-lean_Reactivegas_Mirrors.lean.txt:1–260`). It does not make the module
invisible to mandatory CI: the explicit tracked-module wrappers include it.

The inversion driver builds/imports all discovered modules, walks source files,
parses actual public theorem declarations, and requires an elaborated theorem
at each source line with the matching name. `findDeclarationRanges?` supplies
the compiled selection line. It fails on a missing source-position match or
wrong declaration, and emits its success marker only if failures are empty
(`source-scripts_check-reactivegas-inversion-coverage.txt:63–114,208–258,439–510`).
Log **9–31** records **29** source modules, **185/185** source/elaborated-backed
theorems and success. The repaired module contains public declarations such as
`view_mem_of_isMember` at **73** and `conservation_corr` at **123**, previously
**72** and **122** (`source-lean_Reactivegas_Mirrors.lean.txt` and
`prior-lean_Reactivegas_Mirrors.lean.before.txt`). An unchanged old range at the
old line would fail this source-position check. This is an executed retained
consumer result interpreted through its actual source, not a newly executed
auditor test or mere search hit.

Together with log **67** and the mirror driver, this supplies evidence of
consistent final shifted theorem ranges and module loading, conditional on the
receipt's execution-source identity. It does not require knowing whether Lake
compiled Mirrors in a suppressed build or reused an artifact produced from the
same repaired bytes. It does not directly read back every docstring or private
declaration range; those are not interchangeable with old values either.

**(d) Lake's ordinary replay path checks dependency/input trace equality; the
unqualified artifact-provenance claim needs bounds.** The pinned source reads
the entire Lean file and hashes its text, including documentation
(`toolchain-lake_Lake_Build_Module.lean.txt:42–50`). It combines the source,
dependency/setup and option traces before build/reuse (`:680–730`). In ordinary
mode, an existing saved trace must match the input/dependency hash and outputs
must exist (`toolchain-lake_Lake_Build_Common.lean.txt:228–270`). Old mode can
instead use mtime; it defaults false and is enabled by `--old`, which these
source commands do not pass (`toolchain-lake_Lake_Build_Context.lean.txt:15–36`;
`toolchain-lake_Lake_CLI_Main.lean.txt:58–64,242`).

This supports ordinary source-consistent incremental reuse. A replay label is
not itself a committed-Git-identity measurement, retained olean digest, producing
run timestamp, fresh axiom elaboration, or universal exclusion of old-mode
behavior. The local reuse check trusts saved metadata/output existence; the
log does not supply an independent artifact-to-commit attestation. I observed
no cache corruption or old-mode invocation, and invent no requirement to rerun
the unchanged campaign. The remaining identity blocker is the missing envelope
observation above, not cache use by itself.

### Positive claims and controls

| Claim | Assessment and evidence span |
|---|---|
| Mirror driver imports the repaired module and attributes by home | Established: `source-scripts_check-lean-mirrors.txt:52–64,201–239,368–391`; direct import and `getModuleIdxFor?`/module-name lookup. Home attribution does not by itself hash source. |
| Driver ran in this invocation | Supported by regeneration to a fresh temporary file, deletion of old receipt, direct Lean invocation and nonce write/readback: same source `:33–51,394–419`; `source-justfile.txt:67–69`; clean log `:2935–2965`. Nonce `1788678416820927632` corresponds to `07:06:56.820Z`, within the receipt interval, and differs from the superseded nonce. It binds the driver receipt to the invocation, not the imported olean's producing time. |
| Census unchanged | All mirror enumeration lines excluding the nonce are byte-equal between the two logs: 19 rows, 4 named exceptions, 24 discovered predicates, 2 promoted obligations, 29 tracked modules; 17 orphan-checked Bool definitions. Exact four exception identities and `Reach.below` exclusion are present, not inferred solely from the count (`log-analysis.json`; clean log `:2935–2965`; checker `:152–156,349–359`). The comparison log was the **pre-commit comment-repair run**, not an independent run of the pre-repair 94bb7bb source. Its old identity-binding gap is not repaired retroactively. |
| Log line 3 toolchain control | A real negative comparison control: parsed valid patch-incremented pin must be rejected by the same comparator against the live running version; parser failure is inconclusive, acceptance fails the wrapper (`source-scripts_check-lean-toolchain.txt:73–116`). Log line 3 records the intended mismatch result. |
| Log line 8 dependency control | A **positive scanner control**, not an executed negative forbidden-import control: the same regex finds 19 existing Reactivegas imports; zero hits would fail. The main nonempty forbidden-import branch can fail, but this log does not show a planted forbidden import triggering it (`source-nix_lean-dependency-direction.sh.txt:6–33`). This is source-import matching, not a compiled dependency census. The owner's grouping of all three as negative controls that “did fail” is inaccurate. |
| Log line 36 inversion control | Derived `backdonate` coverage is withheld; the same `compareCoverage` returns an error naming missing backdonate, observed at log `:32–37`. An unexpected success or wrong missing-name diagnostic records a failure (`source-scripts_check-reactivegas-inversion-coverage.txt:349–417,477–510`). The outer control succeeds because rejection is expected; this is not a nonzero project compilation or a semantic guard mutant. |

These controls are not checks that cannot fail. Their exact observed domains
above are narrower than proof of all cache, theorem or dependency properties.
No new mutant or execution credit is assigned.

## FS-02 and Amendment 1

The original pre-map, v1 map and amendment v2 map are all retained unchanged.
The original diff is +51/-1: it adds the ruling section and S5 bullet, with the
single declared period-to-semicolon line amendment (`evidence/map-pre-v1.patch`).
This fact is recorded, but additive-only acceptance was explicitly withdrawn
after START; it is not my governing criterion.

V2 SHA256 is
`e5216dd5c6aa4ae3b8936aadd578d199ef5365815afd8432260bf64b9c8023a9`.
It matches the current local authoritative map readback. The amended input's v1
copy is byte-identical to the originally admitted after-map. All three maps'
historical tails from `# HISTORICAL RECORD` onward have the identical SHA256
`d6f343cd47c8a79a5f7816a80372d6e4ea0ffdc0e94664133b60c2dde50fdf77`
(`evidence/map-analysis.json`).

| Amended requirement | Assessment |
|---|---|
| Actual current milestones | V2 `:12–19,180–197` records landed S2R `3590c001` / #88, S2 accepted, S3 terminal findings + SS-0 9/4 + new static commission, S4 terminal FS findings followed by later CI and this supplement, and three OPEN S5 obligations. These match the newly supplied commissioning authority/NOTE-073; remote branch/PR state and S3 executions are not independently re-audited here. The cache wording has the limits stated under (a). |
| Dated ruling and precise distinctions | V2 `:128–167`: date, NOT-REQUIRED arbitrary-state decision, required finite-history validation, no undecidability/absent-caller inference; genesis, fixed view/auth and refusal distinctions all present. Qualified wording `:156–161` removes the universal exclusion. Candidate `source-lean_Reactivegas_Predicates.lean.txt:90–101` and `source-lean_Reactivegas_Trace.lean.txt:241–290` support the distinctions. No bridge proof inferred. |
| Ownership and retained obligations | V2 `:163–176,190–197` retains OPEN finite-history correspondence in S5 with #75/#71, retention outside V-5, inversion exactness, and H-01/H-02/H-03 on their original terms. Historical F-001 at 94bb7bb remains OPEN, separately from current classification. |
| Required issue-body recording | Independently read the hash-verified retained issue-body readback: `evidence/prior-ISSUE-66-BODY-READBACK.md:33–55` records the dated ruling and owned finite-history obligation. This verifies the frozen readback, not today's remote issue body. |
| Owner's earlier false map claim | The prior D1 report `evidence/prior-owner-d1-fit.md:76–83` is still present with original SHA256 `6824f937…`. V2 `:130–136` and the consolidated disposition `:13–35` explicitly admit that claim was false. Original omission remains observable in the pre-map snapshot. No silent repair of the D1 original occurred. |

**Remaining blocker FS-02-P: current preservation references are false.**
V2 line **15** and lines **180–182** say that the old S2 submission-3 and
successor-campaign wording is preserved **in the historical section below**.
That section, **203–459**, is exactly the earlier historical tail. The removed
current row and owed-list text were not moved into it; it contains neither
`submission 3` nor `S2-SUCCESSOR-CAMPAIGN-PROPOSAL`. The old text survives in the
separate v1/pre-map snapshots, so **no evidence loss is alleged**. But the
current map's explicit locator is incorrect, and Amendment 1 criterion 1
requires superseded wording to remain readable there. V2 **135–136** also
retains the unqualified claim that every earlier line is untouched, although
the current-section changes are visible in `evidence/map-v1-v2.patch`.

The discriminator is exact text reconciliation: the v1 removed row and list
versus the entire v2 historical tail, not an isolated header or a spelling
search offered as a behavioral test (`evidence/map-analysis.json`, both diffs,
and the retained complete map versions). This is a documentation-record
finding under the amended contract. No source repair or second submission is
commissioned. The ruling's content, historical-tail preservation and explicit
withdrawal themselves pass; the inaccurate current preservation account does not.

No `docs/en/design/` file differs in either candidate range. All current audit
worktree blobs match the frozen tree; the amendment changes only external map
records (`evidence/base-range-files.txt`, `repair.patch`, `candidate-blobs.json`,
`map-v1-v2.patch`). This is a static scope check, not surveillance of every
possible intervening filesystem write by another actor.

## Documentation-boundary disposition and stopping receipt

The desk's unqualified zero-source-effect claim is **withdrawn, not proved**
(`evidence/inputs/NOTE-072-S4-TERMINAL-DISPOSITION-AND-FINAL-SUPPLEMENT.md:5–13`;
`evidence/inputs/S4-FS01-FS02-CONSOLIDATED-DISPOSITION.md:114–132`). The two-site
repair remains within the existing module doc at Mirrors **4–34**, changed
**29–32**, and the driver's declaration doc at **152–154**. Exact patch and
prior parser evidence agree (`evidence/repair.patch`, `prior-comment-boundary.json`).
The prior full report's conclusion that non-comment program bytes, statements,
proof terms, imports, exception members, runtime and checker logic are unchanged
is not contradicted. It is retained at its original provenance, not re-proved
by this supplement.

Documentation metadata, declaration ranges, generated documentation bytes and
text-induced build hashes may change. The prior generated driver artifacts
remain separately hashed: old `2c9e3626…`, final `083485a6…`
(`evidence/prior-driver.before.txt`, `prior-driver.after.txt`), with equal
comment-erased bytes established by the prior full audit. I claim neither raw
driver byte equality nor zero metadata effects. The local toolchain stores
module/declaration documentation and its ranges
(`toolchain-Lean_Elab_BuiltinCommand.lean.txt:18–29`,
`toolchain-Lean_DocString_Add.lean.txt:183–214`). The source/range consumer
evidence above narrows the earlier unobserved-consistency gap; it does not
authorize substituting old oleans/ranges/hashes for new ones.

All prior mutation executions, cold receipts and historical findings keep
**94bb7bb** and their own timestamps. No semantic failure is waived. No
unchanged semantic check was rerun. F-001/H-01/H-02/H-03 and the prior full
report's other owned limits remain as recorded in
`evidence/inputs/PRIOR-FULL-AUDIT-REPORT.md:241–312` and its onward record.

Finite supplement scope completed: all original/amended input integrity,
required prior report/manifest, four disputed cache assertions, positive
census/nonce/import claims, three controls, amended map criteria, and the
documentation boundary. This report returns **FS-01-P** and **FS-02-P** to
commissioner `%503` locally. The evidence is compatible at the observed consumer
layer, but **the complete requested compatibility/record claim cannot be
certified with the frozen envelope and current map wording**. There is no
automatic retry, repair, new execution request or promised follow-on audit.

The source worktree remains clean with no `.lake` created. Final identities,
input integrity and manifest verification are recorded alongside the terminal
STATUS event. `MANIFEST.sha256` covers this report and retained evidence, with
**no self-entry**. Completion hands control back; it grants no candidate
acceptance, #66 closure or merge authority.
