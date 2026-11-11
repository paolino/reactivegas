# S3 independent full static audit — bounded parent disposition

Owner `%503`. One bounded disposition, returned whole. Static; no execution, no
fourth author repair, no measurement grant taken.

## 1. Verified, not accepted on its word

| claim | my check |
|---|---|
| report `3f7260b6…` | recomputed, **matches** |
| audit manifest `65a6c29e…`, 30 entries | recomputed, **matches**; `sha256sum -c` from the runtime root → **30/30, zero non-OK** |
| its own execution account | **0 builds, 0 elaborations, 0 probes** — the zero-execution fence held |
| author cap | **3/3**, and the verdict explicitly does **not** authorize a fourth repair |

*(My first `-c` run reported 61 failures because I ran it from `handoffs/` while
its paths are runtime-root-relative. Same cwd error I have now made three times
on three different manifests; the manifest was never at fault.)*

Its independent work is substantial and checkable: **all 207 rows** individually
judged (`ROW-REVIEW-207.md`), **all 81 helper identities** (`HELPER-REVIEW-81.md`),
the **323-entry** original manifest mechanically rehashed **323/323**, all **239**
declaration sites matched after parsing namespace boundaries with nested comments
and strings stripped, and the OP-10 output reconciled at 1,213 distinct names.

**It independently confirmed the seven root declarations after `end KelGroups` at
`:872` — sites 877, 883, 889, 899, 909, 914, 923.** That is the exact set whose
existence my broken file-counting instrument missed and the desk corrected.

## 2. Verdict: AUDIT-FINDINGS. Phase 1 remains incomplete.

| finding | independent verdict |
|---|---|
| F-01 receipt transcription/inventory | **PARTLY — blocking** |
| F-02 recoverable provenance | **PARTLY — blocking** |
| F-03 semantic denominator/relation | **PARTLY — blocking** |
| F-04 obsolete `no_expiry` scope | **CLOSED** |
| F-05 helper antecedent witnesses | **CLOSED, static only** |
| F-06 complete executable phase plan | **PARTLY — blocking** |
| F-07 measured costs/isolation | **PARTLY — blocking** |
| F-08 terminal journal tail | **CLOSED** |

**Five partly-blocking, three closed.**

## 3. My own assessment does not survive intact, and I say so plainly

I had recorded **five CLOSED** — F-01, F-02, F-04, F-05, F-08. The independent
audit sustains **three**. **My closures of F-01 and F-02 are not sustained**, and
its reasons are specific, not stylistic:

- **F-01** — the corrected docs, toolchain, recut and exporter entries are real
  improvements, but the *original relationship* still misstates or omits archived
  identities and states, and **seven fields are not supplied per experiment**.
- **F-02** — recovery of the report SHA, the candidate and the six t57 instruments
  *is* supported; what remains incomplete is the **required reassessment**:
  available t62 instrument/command/hash bindings are **collapsed back to prose**,
  and t57 instruments **acquire unsupported cross-property links**.

I labelled those closed on the strength of the *recovery* and the *corrections*.
The auditor closed on the strength of the *obligation*, which is the right test —
a finding closes when what it demanded is delivered, not when the artefact around
it improved. **This is exactly why my labels were bound as "my assessment, not an
exclusion list", and the binding did its job.**

I adopt the independent dispositions. **Phase 1 is incomplete.**

## 4. Successor recommendation — I endorse it, with the cap stated

It recommends a **new bounded static specification/instrument commission** —
explicitly **not** reopening either terminal author — because *the current packet
is not executable enough to authorize its own 18-run request*. Six ordered
finite packages, all static: SS-1 evidence reconstruction (`receipts.jsonl`,
one record per actual experiment including repeated historical states), SS-2
complete domain (239 identities, atoms, ownership, preserving root wrappers,
private compiled names and the 974 retained generated names), SS-3 observation
registry (all 207 old row IDs carried forward by reference, 81 helper instances),
SS-4 frozen measurement instruments **including an initial cold baseline**, SS-5 a
mechanical validator that **rejects** unresolved references, missing identities,
count drift and unsupported claims, SS-6 exactly one frozen submission. Then a
**fresh independent static instrument review** of that one output.

Two details it gets right that I want on the record: the check-elaboration file
must use the **fully-qualified `Reactivegas.checkSweepIdempotent`** — the exact
defect I found in the earlier request — and sharing must use **two separately
admitted single-atom variants under separate roots**, not a sampled equivalence.

**What I am not doing:** not commissioning it. That needs a numeric grant and a
scope the desk owns. Author cap stays **3/3**; historical **5 substantive / 3
targeted** stays spent; the measurement request stays **ungranted**; no fourth
author repair is implied by any finding here.

## 5. Standing

All three submission sets remain immutable and separately verifiable. The S3
zero-execution fence held on both sides — author and auditor — with **zero**
project execution in this entire audit. `#66` open, and S3 does not close with S4.

---

# ADDENDUM — successor scope, numerical request, and a challenge to the ordering

Appended, not a second checkpoint. Also corrects an error of mine.

## 0. My error

My 06:41:27 journal called `%575` "the S3 static auditor working its own paths".
Its `STATUS.md` had already carried **TERMINAL AUDIT-FINDINGS at 06:19:25.966Z**.
**Process presence is not active audit work**, and I read liveness off a pane
instead of the journal that had already been supplied to me. Its context and
outputs are preserved; I am not waiting for another terminal from it.

## 1. The auditor's ordering is NOT mandatory, and I now challenge it

SS-1 → SS-6 places **frozen measurement instruments fourth**, after complete
evidence reconstruction, a complete 239-identity domain, and an observation
registry carrying **all 207 row IDs** — and only then tests a real command.

**That is fixture-before-real-interface, and this milestone has already paid for
it once in #30.** Writing a 207-row registry against an *assumed* command
interface risks discovering, after the whole registry exists, that the first real
invocation does not behave as the registry assumed — and then every row's
"required input" is wrong at once.

The evidence that this risk is live, not theoretical, is already in hand:
- the one retained anchor **R-BUILD2 has an isolation defect** — its `LEAN_PATH`
  names the candidate worktree;
- the check-elaboration instrument was written against an **unqualified**
  `checkSweepIdempotent` that cannot resolve;
- an earlier plan asserted a **false universal** about elaboration halting at the
  first failing obligation, refuted by its own log.

Three interface assumptions, three defects, all found only when someone looked at
a real artefact. **I endorse the auditor's content and reject its sequencing.**

## 2. Smallest real experiment that validates the approach first

**SS-0, before any registry is written.** One chain, one atom, cured isolation:

| # | operation | class | purpose |
|---|---|---|---|
| 1 | cold `lake build` in a **fresh scratch checkout of `3590c001`**, full per-module log retained | substantive | establishes `U-COLD` and the baseline the incremental number is meaningful against — the thing the current request puts *after* its cycles |
| 2 | apply **one frozen single-atom diff**, timed incremental build, retain per-module lines and the exact failing `file:line` + error text | substantive | establishes `U-CHAIN` **and** whether the outcome is a **semantic RED at a named obligation** or a setup failure |
| 3 | `git checkout -- .`, timed re-run to GREEN, `git status --porcelain` empty before and after | substantive | establishes `U-RESTORE` and that restoration is real, not assumed |
| 4 | isolated check elaboration on **`Reactivegas.checkSweepIdempotent`**, fully qualified, in a precisely stated proposition | targeted | establishes `U-CHECK` as a **distinct** unit and kills the `#eval`-timer conflation |

**Charge classes stay separate: 3 substantive + 1 targeted. Nothing is averaged
and no class is treated as equivalent to another.**

**Frozen inputs:** scratch checkout of `3590c0015b84fd58004bf6fb44dd18b107304c48`,
**never** the candidate or repair worktree; porcelain verified empty **before and
after** each cycle; the single-atom diff frozen and hashed **at authorization,
not at run time**; toolchain pinned Lean 4.25.0.

**Choose the C-STEP chain deliberately.** It is the one chain with a retained
anchor, so the run doubles as a **method validation**: if the cured-isolation
result reproduces R-BUILD2's shape — `Step 1.2s`, `Predicates 445ms`, failing
`Reactivegas.Invariants` with an exact-term RED at `Invariants.lean:407` — the
method is sound and the isolation cure changed nothing it should not have. If it
does **not** reproduce, we have learned that for four operations instead of
eighteen.

**Stop condition:** stop and report at the first non-zero outcome. A setup failure
consumes its operation and returns the actual blocker; it never becomes a semantic
result.

## 3. Full remaining requirement, kept distinct

**SS-0 is a bounded prototype and is NOT S3 acceptance.** The original obligations
stand in full and unreduced: identity/atom/receipt reconstruction (SS-1, SS-2),
the observation registry over all 207 row IDs and 81 helper instances (SS-3), the
complete frozen measurement instruments (SS-4), the mechanical validator that
**rejects** unresolved references, count drift and unsupported claims (SS-5), and
one frozen submission with its terminal handoff (SS-6) — followed by a fresh
independent instrument review. **Five findings remain partly-blocking. Phase 1
does not become complete because a prototype succeeds.**

What SS-0 buys is that SS-1 through SS-6 are then written against a **measured**
interface rather than an assumed one.

## 4. The numerical request

**3 substantive + 1 targeted, one chain, one atom, separate classes, no retry
reserve, stop at first non-zero.** Against the S3 book, which stands spent at
**5 substantive / 3 targeted**, this is a **new** bounded allowance and I am
requesting it, not taking it.

**I am commissioning nothing.** No project execution, no fourth author submission,
no new seat and no cap follows from this assessment. If the desk grants SS-0, the
seat, its fences and its receipts still need their own dispatch.
