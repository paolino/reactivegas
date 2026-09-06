# Commit audit — AUDIT-FINDINGS, blocking=3

- Submission **2/2**, candidate **9c8756a252c46bc5745badafcc9126abd3e5d9a7**.
- **FULL original mandate** against accepted S1 base **4a6cd87**, including original rows and integration. Mandate base 507bc79; model base e6c5924.
- **One remaining cold build** was available at dispatch and consumed by the exact full command. Ticket now **3/3 building audits**, this seat **1**; ceiling raises **0/2**. Later experiments reused this seat's warm tree.
- Independent seat %524, reactivegas:5, CLI codex, live PID 822000 argv gpt-6-astra / high. Author and ticket owner: muse. Authorization: operator ruling via NOTE-009. No contact, repair, commit, push, merge or external publication.
- Frozen gate v4 SHA-256: `cb5b8d1530773ca2d0c5dbb7d486caff52a166fd40b7e0db79604bc2f921ee2a`. Missing from the fresh checkout, it was installed as ignored gate.sh from the verified ticket snapshot.
- Campaign **CLOSED by SET-POINT**, **4 KILLED / 2 RESIDUAL / 3 BLOCKED** assessment rows. This completes a findings audit, not acceptance. Next submission under this ticket: **FORBIDDEN**; blocking failures require a re-cut.

The exact full gate passed. The full amended mandate does not: three independently demonstrated blockers remain. No disposition was omitted for lack of budget.

## Post-START mandate and independence record

START was journaled at **09:20:33Z** against brief SHA-256 `cbc7b686a78b59f9b796ef30957b04f7a500ad96f2ad7fb042c75cacbb7a824c`. The brief did not yet contain the four later dispositions. NOTE-001 was delivered at **09:22:51Z**, acknowledged at **09:23:23Z**. NOTE-002 required the explicit post-START record and establishability table; both acknowledgements are in this seat's append-only STATUS.

The amendment changed scope/severity, including ratification of **CI-74-A** as required BLOCKING whole-wrapper binding. It supplied no acceptance evidence. Conclusions here are the seat's own and rest on current-candidate experiments. The CI omission was independently journaled before NOTE-001. All invalid audit-s2 verdicts were disregarded and inherited rows reopened.

## Establishability within the already allocated build

| Disposition | Establishable? | Independent evidence |
|---|---|---|
| D1 — R74-03 CI execution | **Yes** | B07 rejects corrupted corpus with real verify, exit 1; B08 runs the committed CI command `just lean`, exit 0, leaving those same corrupt bytes intact. |
| D2 — declared jq dependency | **Yes** | clean-shell.log reports JQ-ABSENT; real verify reaches the live check, then exits 127 at jq. |
| D3 — whole approved wrapper values | **Yes** | Four context mutants survive compiled check and shipped jq. Independent live-context comparison rejects mutated view/initial, then accepts current values. |
| D4 — public check arity | **Yes** | IO08 exits 0 for `check ONE_PATH`; exact-byte assertions prove economic output in ./check and integrated output in ONE_PATH. Named residual RA74-ARITY. |

No disposition requires another cold build. BLOCKED ledger states below denote established unmet requirements in the frozen candidate, not unperformed experiments.

## Blocking findings

**F74-1 — R74-03: automated CI never runs the corpus verifier.** `.github/workflows/ci.yaml:79` and `:82` invoke the toolchain contract and `just lean`; the lean recipe at `justfile:55` omits corpus verification. Only the separate local ci recipe calls it at `justfile:119`. The same one-byte defect is rejected by real verify (B07, exit 1) and survives the actual CI Lean command (B08, exit 0, mutation preserved). Property class: required checks must execute on the automated acceptance path and observe their defect class. This is a local execution of that committed command with a mounted corpus, not a claim of a remote Actions run. S1 checks remain exercised by just lean.

**F74-2 — D2 / clean-checkout deliverable: jq is an undeclared runtime dependency.** `justfile:98` and `:103` require jq; `nix/project.nix:14` does not supply it. The ordinary full gate used the host environment. Under `nix develop --quiet --ignore-environment --keep HOME --keep USER`, the real recipe reports JQ-ABSENT, successfully reaches `ntraces=5 nevents=32 nsteps=7 live-bound`, then exits **127** at jq. This is a reproducibility failure, not an advisory portability observation. Property class: shipped dev-shell commands resolve required tools from the declared environment. Failing closed does not satisfy the clean-checkout success requirement.

**F74-3 — CI-74-A, ratified by D3: replay-context values lack live binding.** `CorpusExport.lean:93` and `:107` bind only traces and steps. Independent mutants change economic view.members[0].key to ZZZ, integrated initial.members to [], economic auth to permissive, and integrated auth to permissive. Every mutant preserves the approved wrapper keys and passes both compiled check and the corresponding shipped jq program. Property class: all existing replay-context field values must be bound to authoritative live context, alongside event arrays and key sets.

Current view and initial values match independent live calls: this is a missing can-fail property, not a claim that current fields are corrupt. The ordinary byte comparator detects one-sided changes to checked-in files. These experiments isolate value/shape checks; they do **not** claim an isolated altered file passes the full verifier. The gap is independent context binding rather than comparison of emitter output with its own frozen output. The acknowledged call-site independence still does not establish serializer-instance independence.

## Original rows and integration

| Row | Verdict / state | Independent evidence and limits |
|---|---|---|
| G74-CALLS-EXISTING | PASS / KILLED | Compiled checker rejects empty traces, 40→41 same-size event change, truncated extents, last-element changes on both arrays and non-array values. Normal live extents are 5/32/7. Emitter/check use distinct calls to existing definitions. |
| G74-ENVELOPE-CLOSED | PASS / KILLED | Verbatim candidate jq programs reject extra top keys, extra nested keys and deleted top keys on both wrappers. Top additions pass the array checker as expected: separability witnessed. No emitted key-set change. |
| G74-VERIFY-FAILS-CLOSED, original byte/manifest mechanism | PASS / KILLED | Full gate provides clean 0. B01/B02: byte corruption 1 then restored 0. B03/B04: manifest corruption 1 then restored 0. Exact SHA restores. Frozen v4 rejects stub verifier exit-0 in B05. This narrow result does not close F74-1/F74-2. |
| G74-RECORD-HONEST | ADVISORY / RESIDUAL | docs identical under e6c5924, 507bc79, 4a6cd87; counts, vote hole, provisional status accurate. Coverage handoff still contains current-tense UNPROVED claims and old hash. RA74-RECORD: ticket owner, T7403→#71. |
| G74-ADDITIVE-ONLY | PASS / KILLED | B06 rejects an untracked forbidden docs path before build. Six implementation paths plus six unchanged planning paths differ from S1. Existing model bytes exactly equal ordered S1. All 259 tracked-file SHA-256 identities verified. |

The merge bc44998 has parents fed19b3 and 4a6cd87; S1 has sole parent e6c5924. Against S1 the six implementation paths add 188 lines; the other 287 lines are unchanged frozen planning specs. No implementation deletions against S1. The repair against the immediate merge parent has the brief's four paths, +78/−3.

The independently derived JSON delta from submission 1 is exactly `/traces/4/steps/6/result/guard/declaration`: UNPROVED → step_withdraw_inv, **+9 bytes**. Economic output **14494 B**, SHA-256 `73a077fc514038e40f84aca4a995fe68623e3af46ed11c0280d5b963137576aa`; integrated unchanged **7673 B**, SHA-256 `1f173aec9c3afd9cb95265e4be2966b9316e810a969d9fc40f672b17120f3675`. Current emitted bytes contain zero UNPROVED entries.

The full gate also independently ran S1 checks: 14/14 constructor coverage, withheld-backdonate control reducing coverage to 13/14, and matching trace coverage. These audit replay inputs; they are not a widened S1 authorship audit or a new theorem-completeness claim. Mode: bounded exporter implementation audit, not STATEMENTS/PROOFS/INVERSIONS commissioning.

## Failure modes and residuals

- Missing input and malformed JSON propagate exit 1; both economic and integrated parse branches are exercised (IO01–IO03).
- Failure opening either output propagates exit 1 (IO04–IO05). A failed second write leaves the first output; no atomic two-file write guarantee was declared. Verify emits into its temporary directory.
- B09 injects check exit 43 after successful byte/manifest checks and observes recipe-body exit 43. It uses the exact recipe body after the already-completed build step: a focused probe, not another full gate.
- jq absence propagates 127 (F74-2). No threads, synchronization primitives or concurrent exception paths were added.
- **RA74-ARITY (D4)**: `corpusExport check ONE_PATH` falls into the write arm at `CorpusExport.lean:138`, exits 0 and can overwrite ./check and ONE_PATH. Recorded under the amendment's explicit ADVISORY residual option because the shipped caller supplies three arguments and cannot reach this malformed invocation. Owner: ticket owner; local filed ID RA74-ARITY in onward-discoveries.md, carried into the re-cut decision. This neither fixes the defect nor claims universal bad-arity refusal.
- **RA74-RECORD**: distinguish stale coverage discussion from current bytes; owner/follow-up ticket owner, T7403→#71. The same stale source comment is inherited unchanged from S1: OD74-S1-COMMENT is RECORDED, NOT-OPENED for the epic's Lean compliance owner through the ticket owner.

No new candidate invariant is proposed: CI-74-A is now ratified and blocking. Vote coverage and the out-of-band integrated threshold remain declared limits; no fields or semantics were widened.

## Finite campaign and evidence integrity

Byte fault set: **18 applied/executed mutants, 14 killed, 4 context survivors**. Operators cover empty output, same-size changed values, both wrappers' top/nested/deleted keys, truncated extents, tail elements, non-array values and four context fields. **39 byte/check/IO invocations** include clean/separability controls and eight compiled IO probes; the boundary harness adds **9 executions**. Repeated byte controls are not additional unique mutants. This is a finite fault set, not an exhaustive universe.

The byte instrument observed defective empty/changed-value seeds fail before admitting clean controls. Boundary instruments observed corrupted bytes fail before restored controls. Edits assert application. Initial namespace setup attempt 1 failed at /dev/urandom, exit 134 before any domain judgment; source/log retained and excluded. A private device mount fixed the instrument setup. No syntax/import/setup failure counts as a kill or second cold build.

Only the brief-required frozen gate made transient corpus/manifest writes in the worktree; its post-run hashes equal the candidate. Other mutations used runtime fixtures or namespace mounts over a read-only candidate, sharing this seat's existing warm .lake. No instrument staged or edited tracked files. Individual receipt hashes were rechecked. Sources, fixtures, logs and ledgers remain local; reproducible build outputs are retired separately.

| Frozen instrument | SHA-256 |
|---|---|
| instruments/campaign.mjs | `6d262da02032e9571b4ee3dcf3c55f80966c18c392fe482ede273d184ef57e81` |
| instruments/boundaries.mjs | `e22316fffb0185f2b9019bbc2b60e61c47c8758f306a3b7f3b16f2e4943934f4` |
| instruments/live-context.lean | `395042c9bc6527de7421ca216dfaca2ae7a88c2e6713017463a41ee3825ead4e` |
| instruments/provenance.mjs | `ed496ad7d38cc1a77f3b6406be7ee3bb301429ebda221b80b59506ea55721ba0` |

Lean affordance: lake env lean against already-built imports for independent live-context calls. Main byte/IO judgments used the compiled candidate binary. No timing/concurrency/memory claim relies on interpretation.

## Verification receipts

All evidence paths are relative to this root's evidence/. Individual command/exit/duration/hashes are in results.json and boundary-results.json.

| Command / receipt | Exit | Duration | Evidence SHA-256 |
|---|---:|---:|---|
| Exact `nix develop --quiet -c ./gate.sh`, including export, verify, controls, full just ci | 0 | 156224 ms | full-gate.log: `3504b4c56347695f97007c5b2915c5c8c500654a182ef990c859e355dd36ede4` |
| Compiled byte/IO campaign | 0, expectations met | 2375 ms | byte-campaign.log: `96b21029b030401b21fad3cf55e5fd67487607f56b4f71d6b72da47e30a5d6ce` |
| Warm mounted-boundary campaign | 0, expectations met | 32565 ms | boundary-campaign-v2.log: `eebb0b328cd7a3402126cfff795aa0fc97b78b60fc79e1e9ec5c33f5152b2d51` |
| Clean-environment Nix shell, real verify | 127 | 1487 ms | clean-shell.log: `4d16d49a3c98459b73ec4dfbf09ee4657536cbf98bf84a04ab1e0893093270c6` |
| Independent live context via lake env lean | 0 | 2549 ms | live-context.log: `09772de052b565e5e5640aed9608d8e41acfc24332d98b6746bd87b62d618655` |
| Provenance and context red/green comparison | 0 | 136 ms | provenance.log: `558a83047fc59d6641ffa619c81e8810c3073301de78053e56c9de87953f5dc4` |

/code free space: **220834570240 B before**, **220539506688 B after** cold build; /tmp after **31148613632 B**. Later shared-host readings are retained, not attributed solely to this audit. Compiled binary identity is in binary.sha256; retirement and final identity are in retirement.log. The full per-row terminal ledger is evidence/campaign-ledger.md, also appended to the ticket's carried ledger without erasing earlier seats.

## Re-cut proposal and return

Carry the frozen candidate, report, both amendments, three blockers, complete ledger and two residuals into a **new ticket/campaign with a revised mandate**. Acceptance must establish automated verifier execution, a self-contained declared dev-shell environment and can-fail binding for all approved wrapper values. Carry D4's explicit residual disposition and the coverage correction obligation. This proposes a disposition to the commissioning owner; it authorizes no repair of this candidate and resets no current cap.

Returnable detached worktree: **/code/reactivegas-issue-74-audit-s3**, still at 9c8756a. Durable local root: **/tmp/reactivegas/ms2/e-haskell-impl/t74-corpus-exporter/audit-s3/**. No external publication or acceptance was performed.
