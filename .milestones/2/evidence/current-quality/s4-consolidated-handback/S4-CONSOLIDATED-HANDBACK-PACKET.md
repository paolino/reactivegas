# S4 — consolidated handback packet

Owner `%503`, issue #66, milestone 2. Local delivery only. **No acceptance of
`#66`, no closure, no merge, no push, no PR from this packet.**

## 0. Verdict

**I do not accept S4 yet, and the reason is not a code defect.**

Both FS-01-P and FS-02-P are now discharged with evidence, and no observed
semantic defect exists anywhere in the chain. What stops acceptance is that my
own final disposition contained three overclaims that an independent auditor had
to correct, and the corrected picture is narrower than the one I wrote. The
corrected picture is recorded below and supersedes mine. On that corrected basis
I state my acceptance in §8 — conditional, and conditional in a way the desk can
check.

## 1. Identity, unchanged

| | |
|---|---|
| candidate | `04eb6c7d9aeb2a3602fca5ece14cbc033221cb43` |
| tree | `caaa0488f39a6afb2553680a11fd6bfd86d1c90b` |
| accepted base | `3590c0015b84fd58004bf6fb44dd18b107304c48` (now `master`, PR #88) |
| branch | `chore/66-s4b-mirrors` — **not pushed, no PR** |
| repair | comment-only, +6/−4, every changed line inside a comment block |

No candidate edit and no new submission was made at any point in this note's
work. The supplement independently confirms all current audit-worktree blobs
match the frozen tree and **no `docs/en/design/` file differs in either candidate
range**.

## 2. The two terminal audits

| | |
|---|---|
| FULL static audit `%576` | **TERMINAL 07:06:01Z — AUDIT-FINDINGS**, report `43db90494fbad83282092d388382651d9f2d56e7aefe30da7b2c66e08443fe9c`, 74-entry manifest. Findings **FS-01**, **FS-02**. |
| STATIC final evidence supplement `%578` | **TERMINAL 07:48:24Z — AUDIT-FINDINGS**, report `0f2af02f3d3376fade0eea7d5363ca0cff9f81b213016ed8ea8a54a3188c02fb`, manifest `eb4ba2e2137c090a3291aeaadb667befc9b3031fb9555c0c652284f55c534eb8`. Findings **FS-01-P**, **FS-02-P**. |

I verified the supplement's manifest myself: **73/73 verify**, **no self-entry**
(the three `MANIFEST.sha256` matches are *other* manifests inside `evidence/`,
including byte-exact copies of the input manifests I issued, `7600348a…` and
`3f0122bc…`). `%578` is **retired, not resumed**; its verdict, inputs, report and
limitations are preserved exactly. Project executions by either auditor: **0**.

## 3. My overclaims, replaced by the supplement's corrections

These supersede §2.2 of `S4-FS01-FS02-CONSOLIDATED-DISPOSITION.md`. I state them
as corrections to my own text, not as new findings.

**(a) I claimed "no diagnostic-bearing module compiled fresh." That is not
established.** Before the visible builds, **four preceding all-module build
invocations** run in the source path — both inversion calls, the axiom wrapper,
and the trace-agreement wrapper calling the inversion wrapper again — and they
redirect successful build output to temporary files that are removed on exit.
Their streams are **not in the frozen log at all**. Replay in a later build cannot
exclude compilation in an earlier suppressed build **within the same CI
invocation**. My supporting premise was also overbroad: Lake prints a job when it
has output **or** when its action meets the progress threshold, and quiet/ANSI
modes alter visibility. **Correct bounded conclusion: cache reuse occurred;
neither a wholly cold execution nor an initially warm tree is established.** I
claim no cold-build credit.

**(b) I claimed the receipt supplies "no new axiom evidence." Too broad.** The
named `Invariants` print lines sit under replay headers and *are* cached — that
half was right. But separately, a **newly generated `AxiomGate.lean` driver** is
invoked with `lake env lean` and calls `collectAxioms` over the discovered
compiled theorem set: **29 loaded project modules** including `Reactivegas.Mirrors`,
**1,285 distinct theorem identities**, 1,287 walk occurrences, 1,285 axiom
results, then `axiom-gate: ok`. Those are **fresh queries of loaded compiled
artifacts**, not saved diagnostics. With the six inversion axiom lines the total
is **1,291 records, none naming a non-permitted axiom**. This does **not**
establish a fresh `.lake` and does **not** transfer old cold-sweep provenance —
but "no fresh axiom query occurred" was **wrong**.

**(c) I grouped log lines 3, 8 and 36 as "negative controls that fired." Inaccurate.**
Line 3 *is* a real negative comparison control. **Line 8 is a positive scanner
control** — the regex finds 19 existing imports and zero hits would fail; the log
does **not** show a planted forbidden import triggering the failing branch. Line
36's outer control succeeds because rejection is expected. None of the three is a
nonzero project compilation or a semantic guard mutant. They are not checks that
cannot fail, but their observed domains are **narrower than I stated**.

**(d) My replay/hash-equality argument holds only for ordinary reuse.** Lake hashes
the entire file text including documentation and combines source, dependency and
option traces; ordinary mode requires trace match and existing outputs, and
old-mode (`--old`) defaults false and is not passed here. So source-consistent
incremental reuse is supported — but a replay label is **not** a committed-Git
identity measurement, a retained olean digest, a producing-run timestamp, a fresh
axiom elaboration, or a universal exclusion of old-mode behaviour.

**(e) My residual is settled, better than I had it.** `Reactivegas.Mirrors` is an
import-graph leaf, which I had right — but it is **not invisible to CI**: the
tracked-module wrappers include it, and the inversion driver parses public
theorem declarations and requires an elaborated theorem at each source line with
the matching name via `findDeclarationRanges?`. The repaired module's
`view_mem_of_isMember` moved **72 → 73** and `conservation_corr` **122 → 123**;
**an unchanged old range at the old line would fail that check**, and the log
records **29 source modules, 185/185 source/elaborated-backed theorems, success**.
That is executed retained-consumer evidence of consistent shifted ranges.
**Compile-versus-reuse timing for that module remains unknown**, and every
docstring and private declaration range is not read back.

## 4. FS-01-P — recovery first, then one authorized replacement

### 4.1 What recovery found

Bounded to the run's own session record, not a transcript campaign. **Partly
recoverable:**

- The actual invocation was
  `nix develop --quiet -c just ci > <log> 2>&1; echo "FINAL-CI-EXIT=$?"`,
  printing **`FINAL-CI-EXIT=0`** in **94.6 s**. Output was redirected with `>`,
  with **no tee**, so that `$?` is the CI process's own status. **The real exit
  capture existed** — in the session record, not in the artifact.
- The receipt's `exit-code: 0` line is a **hardcoded literal** transcribed from
  that observation. It is correct, and it is a transcription, not a capture.
- Pre-run HEAD/tree and both porcelain states were genuinely observed.

**Not recoverable:** the post-run block ran only `git status`. **No post-execution
HEAD or `HEAD^{tree}` was ever observed, and no record of one exists.** I record
that envelope element as **missing**, not reconstructed.

### 4.2 The one authorized invocation

Condition met, so NOTE-075's authorization was spent once, on a **new unique
path**, with **no existing artifact rewritten**.

| | old receipt | replacement |
|---|---|---|
| receipt | `S2-CI-final-clean.receipt.txt` | `S2-CI-envelope-replacement.receipt.txt` |
| log | `S2-CI-final-clean.log`, `fbc50e0d75d365f3d0a60d5641d24c58d65f3270dd70f81b7051072164818564` | `S2-CI-envelope-replacement.log`, `6077f29c1426d372d01b910cfda9b12951642c9580522c98bb2fffd7e571c91f` |
| window | 07:05:25Z → 07:07:14Z | **07:55:16.234187728Z → 07:56:33.983919053Z** (77.7 s) |
| exit | literal `0` in artifact; real `$?`=0 only in session record | **`exit-code-captured-from-ci-process: 0`**, direct `$?` of `nix develop`, **no tee** |
| head/tree before | observed | observed `04eb6c7d…` / `caaa0488…` |
| head/tree **after** | **absent** | **observed `04eb6c7d…` / `caaa0488…`** |
| porcelain | empty before and after | empty before and after |
| assertions | none | `head-after==candidate: PASS`, `tree-after==tree: PASS`, `worktree-after clean: PASS` |

The replacement log carries **zero** error/FAILED lines; `axiom-gate: ok`; the
mirror census **byte-identical** at `rows=19 exceptions=4 discovered=24
promoted=2 tracked=29`; and a **new** driver nonce `1788681384521620465`
(≈07:56:24Z, inside the run window, distinct from the prior
`1788678416820927632`), so the source-sensitive driver ran fresh in this
invocation too.

**Bound exactly as in §3(a):** its 17 visible progress records again all read
`Replayed`. Under the supplement's correction that establishes **cache reuse, not
initial warmth and not coldness**. No cold-build credit is claimed.

Prior artifacts confirmed unchanged in the same receipt: log `fbc50e0d…` and
`b6a16cfe…` match their previously recorded values. **Honest limit:**
`S2-CI-final-clean.receipt.txt` reads `4ae61290…`, a digest never recorded before
this packet, so its unchangedness rests on mtime and on nothing having written to
it — not on a prior hash comparison.

**FS-01-P: discharged.** Substantive **21/21** (20 → 21, one invocation, exit 0,
charged). Targeted **52/60** unchanged.

## 5. FS-02-P — external map repair only

FS-02-P was correct and the error was mine. V2 claimed the superseded S2
submission-3 and successor-campaign wording was "preserved in the historical
section below". It was not: that section is the **earlier** historical tail, and
the removed current-section text was never moved into it. The text survived only
in external snapshots, so **no evidence was lost** — but my locator pointed at
bytes that did not contain it. **This is the third time I have asserted a
record's content without reading it** (the D1 closure-map claim, the "digests
swapped" relay, and now this).

**Repair, versioned before editing:**

| version | sha256 | lines |
|---|---|---|
| pre-FS-02 | `b5f304bca000d24354d8ebd642f14a078b31c17a25c9a0207b478f762c8e0427` | 404 |
| v1 (FS-02 only) | `3faf09c287ed848590245b466ffa7b0fe3b0fade098c0e038a39084b60ab0afe` | 454 |
| v2 (NOTE-073) | `e5216dd5c6aa4ae3b8936aadd578d199ef5365815afd8432260bf64b9c8023a9` | 459 |
| **v3 (FS-02-P), current** | **`7d81dce103ffa364b6fc631f391105afec786adffc45984b13cf7dec391b0139`** | **501** |

What changed, and the byte proof:

1. The superseded rows and owed-list text are now **reproduced verbatim inside the
   map**, in a marked section `SUPERSEDED CURRENT-SECTION TEXT`, so the locator is
   true of this file rather than pointing elsewhere. **Anchors verified at the
   referenced bytes, not by file existence:** `S2-SUCCESSOR-CAMPAIGN-PROPOSAL`
   ×1, `submission 3` ×2, `4a6cd87` ×2, `two owned obligations` ×1, and the
   withdrawn `fall outside the predicate` sentence ×1.
2. Both false locator sentences (current S2 row; owed-list S2 bullet) now name
   that section and the retained snapshots.
3. The unqualified **"every earlier line untouched"** claim is corrected in place:
   it was true only of the FS-02 edit (+51/−1) and became **false** at NOTE-073,
   which deliberately rewrote current-section rows because that section governs
   and was stale.

**Byte-comparison proof of preservation.** The historical tail from
`# HISTORICAL RECORD` onward hashes to
`d6f343cd47c8a79a5f7816a80372d6e4ea0ffdc0e94664133b60c2dde50fdf77` in **v3**, and
to the identical value in v2 — **the same value the supplement independently
computed for all three prior maps**. The tail is byte-unchanged; v3 adds only the
current-section corrections and the appended verbatim block.

Unchanged and re-verified in v3: the dated ruling `RG-S4-REACH-20260906` with its
genesis / fixed view-auth / refusal distinctions and the qualified (non-universal)
wording; the **OPEN finite-history correspondence** owned by **S5** with **#75**
and **#71**; retention outside V-5; `ONWARD-68-INV-01` inversion exactness;
**H-01, H-02, H-03 OPEN on their original terms**; historical **F-001 OPEN** at
`94bb7bb`. No `docs/en/design/` or source change.

**FS-02-P: discharged.**

## 6. Remaining named limitations — none waived

1. Initial cache state is **undetermined**: cache reuse observed; neither cold nor
   warm established (§3a). Applies to both receipts.
2. The `#print axioms` lines in both logs are **cached diagnostics**; the fresh
   `collectAxioms` gate is separate evidence and establishes **no fresh `.lake`**
   and **no transfer of old cold-sweep provenance** (§3b).
3. Log line 8 is a **positive scanner control**; no planted forbidden import was
   observed triggering the failing branch (§3c).
4. A replay label is **not** an artifact-to-commit attestation (§3d).
5. `Reactivegas.Mirrors` **compile-versus-reuse timing is unknown**; not every
   docstring or private declaration range is read back (§3e).
6. The census comparison log was the **pre-commit comment-repair run**, not an
   independent run of pre-repair `94bb7bb` source; that old identity-binding gap
   is **not** retroactively repaired.
7. `S2-CI-final-clean.receipt.txt` unchangedness rests on mtime, not a prior hash
   (§4.2).
8. **F-001, H-01, H-02, H-03 remain OPEN.** S3 and S5 remain required and open.
9. The desk's unqualified "documentation-only repair has no source-sensitive
   effect" **stays withdrawn**, not proved. Allowed observed effects: doc
   metadata, declaration source ranges, generated driver bytes, induced hashes.
   Required unchanged and audit-established: non-comment program bytes, proof
   statements and terms, exception membership, imports, runtime and checker logic.
   No byte-identical-generated-source claim; no zero-metadata claim; **no semantic
   failure waived**; prior mutation executions keep `94bb7bb` identity and
   timestamps.

## 7. Cumulative spend

| | |
|---|---|
| S4 owner substantive | **21/21** (raised 20→21 by NOTE-075; one invocation, exit 0) |
| S4 owner targeted | **52/60** |
| S4 author submissions | **3/3** — two historical plus the exceptional third; zero further rounds |
| `%576` full audit | terminal; project executions **0** |
| `%578` supplement | terminal; project executions **0** |
| S3, separate and not part of this packet | **9 substantive / 4 targeted**; author submissions 3/3 plus one new static submission at **zero** executions |

## 8. Acceptance

On the corrected basis in §3 and with every limitation in §6 carried forward
unwaived, **I accept the S4 candidate `04eb6c7d` as evidenced for landing
preparation**, and for nothing beyond that:

- both terminal audits' findings are discharged or explicitly carried;
- no observed semantic defect exists in the chain;
- the repair is comment-only and its source-sensitive consumers executed and
  agreed at the final committed bytes;
- the receipt envelope is now fully bound, including post-execution identity.

This acceptance authorizes only what NOTE-072 and NOTE-064 already made
conditional on it. **Real remote CI at the exact SHA and a later exact-SHA desk
merge grant both remain required. No merge. No `#66` closure.** `#66` is not
proposed complete: S3 and S5 are open, and F-001, H-01, H-02, H-03 are open.

Before any push I will return the PR-attribution instruction question
(`handoffs/PR-ATTRIBUTION-INSTRUCTION-EXACT-TEXT.md`) rather than resolve it
myself.
