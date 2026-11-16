# S2 successor campaign — implementation owner brief

**Authorized by desk NOTE-031.** One named successor campaign. **ONE submission
under this grant** — not two funded by one audit. Findings return to the desk for
disposition: **no automatic successor, no automatic ceiling raise, no additional
submission.**

**Seat:** `muse`, **fresh context** — `pi --provider opencode-go --model
muse-spark-1.3-contributor --thinking xhigh`, launched `muse --approve`. Journal
to `STATUS.md` here with your **verified live argv and post-cursor START** in the
first line. Ticket owner `claude-opus-5[1m]` `%503`.

**Repo:** `/code/reactivegas-66-s2r`, branch `chore/66-s2r-ownership`, base
**`4a6cd87fcbc3e4a536bbc9f240f5efe5704022af`** — accepted `master`. Cold, no
`lean/.lake`. **Recheck the accepted remote at START and journal it.**

**The three rejected candidates are not yours to reuse.** `5745a2c`, `561347d`
and `b0c2cdb` are rejected; their evidence is **evidence at its SHA**, never
inherited acceptance. **Do not copy their ownership code.** You are re-cutting
the whole subject.

## Upward reporting

**Never type, paste, send-keys or send a pointer into pane `%510` or any human
seat**, for any purpose including an acknowledgement. Write
`handoffs/SUBMISSION.md` and journal a `STATUS.md` event with path, hash and next
state. That **is** delivery. Anything you spawn inherits this.

## Caps

| | |
|---|---|
| substantive invocations | **9** — actual nested compiling / full-gate invocations, **including failed and warm attempts**. Read-only and version interrogations cost **zero** |
| targeted queries / probes | **30**, separately counted, failed setups included |
| submissions | **ONE** |
| raises | **none automatic** |

**There is no slack.** `../handoffs/S2R-CONTROL-RECONCILIATION.md` allocates all
nine against mandatory rows, and I reported that to the desk rather than reducing
scope. **A failed invocation costs a mandatory row.** If that happens, **report
the concrete command and cost gap — do not improvise and do not exceed.**

## Exact permitted path list — frozen

**These paths and no others.** "Associated scripts" is not a permission.

```
scripts/check-lean-axioms                        (new)
scripts/check-reactivegas-inversion-coverage
justfile
.github/workflows/ci.yaml
lean/Reactivegas/Invariants.lean                 (Row B renames only)
lean/Reactivegas/TraceTests.lean                 (dead re-exports only)
lean/Reactivegas/Predicates.lean                 (Row C doc comment only)
```

**Anything outside this list is a question to me, not an edit** — including
`scripts/check-trace-coverage-agreement`, which is S1's and is expected
untouched.

**Forbidden:** any economic or base **model** change; any **theorem statement**
change; `docs/en/design/**`; `#70`, `#74`, `#68`, `#71` territory.

**Any new Lean driver must be declared to Lake**, and **its overlap with #70 is
reported through me** — never owner-to-owner.

**The acceptance instrument is version-frozen by me** at
`39d6aa4e2c0c0170` (base contract). **No author-controlled silent gate
adaptation:** if you need the executable contract changed, **ask**. Do not adapt
the gate under yourself.

## The subject — the whole of it

`4a6cd87..final`. You redo **every** original row, not only the new five:
the axiom gate over a discovered extent; the census with **no quota**; Row B's
nine renames and three dead re-exports; Row C's doc path; the fence.

## The five required elements

| # | element | what must be established |
|---|---|---|
| 1 | **Resolver / ownership authority and canonicalization** | the actual authority deciding project-vs-dependency, and how paths are canonicalized before comparison. **Not a spelling test** |
| 2 | **Actual project/dependency source-output relation** | the real relation between a tracked source and the artifact it produces — derived, not asserted |
| 3 | **Equivalent-path controls** | classification **invariant** under equivalent loader paths: relative vs absolute, symlinked, aliased. Each classifies **identically** |
| 4 | **Independent source omission** | `S \ B` **and** `B \ S` both fire on genuinely project-owned subjects, layers kept **explicitly distinct** |
| 5 | **Missing-authority behaviour** | **two lawful outcomes, exactly one evidenced.** If you **retain** the empty/unset `LEAN_PATH` guard, execute **that exact control** (instrument prepared and hashed; one elaboration, zero builds). If you **legitimately replace** the authority and **retire** the branch, **record the retirement and execute the replacement control**. **Do not keep a dead branch alive just to keep the old probe runnable** |

**Forbidden as substitutes:** `B := S`, any name or namespace whitelist, and the
`import Lean` closure. **A fourth guess is not a repair.**

Three candidates failed here because each answered *"which modules are ours?"*
with a **guess about the environment**. Establish an **authority**.

## Bounded advisories — not requirements

**`CI-T-SHARED-FILTER`** (both T derivations share `thmInfo` and B membership —
two views of one inventory) and the **shadow-name invariant** are **bounded
advisories**. Not in scope, not hidden requirements, **not to be smuggled in**.
State `CI-T-SHARED-FILTER` honestly in your packet; do **not** describe the gate
as having two independent theorem sources.

## Your nine invocations — allocated, not suggested

See `../handoffs/S2R-CONTROL-RECONCILIATION.md` for the full mapping. In short:
baseline · clean `Std`-importing root · **relative-path** control · **alias**
control · project-owned `B \ S` · missing-authority control · `by sorry` ·
non-standard axiom + using theorem · final full `just ci`.

Everything else — zero-discovery, truncation, removed module, one-sided T,
panic totality, dead re-exports, census reconciliation — is **probe-level**
against the 30.

## Submission

`handoffs/SUBMISSION.md`: candidate SHA; each of the five elements with its
executed evidence; **every original row** re-established with its command and
output; the **ownership authority documented** with its **fail-closed behaviour**
(a directory or module-name assertion is **not** evidence); the exact enumerated
spend against **9 and 30**, failed and warm attempts included; the
`CI-T-SHARED-FILTER` limit stated; and anything you could not close, honestly,
with its owner.

**One submission.** Make it the one that stands. Do not park between routine
steps.
