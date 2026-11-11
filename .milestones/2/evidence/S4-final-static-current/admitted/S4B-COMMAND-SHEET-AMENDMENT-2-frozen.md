# S4-B frozen command sheet — amendment 2 (supersedes the prior sheet's counts and argv)

Prior sheets are **preserved as superseded history**. This is the **single
current** sheet. **A prose reference back to contradictory commands is not a
freeze** — where an older sheet disagrees, **this governs**.

## Budget — amendment, not a reset

| party | historical retained | new phase | total ceiling |
|---|---|---|---|
| owner substantive | **8** | **7** | **15** — records the **14 → 15 exception** |
| owner targeted | **42** | **8** (was 4) | **50 of an unchanged 60** |
| submissions | 1 spent | — | **2/2, no reset** |
| auditor | — | — | **15 / 69 unchanged**, full-candidate scope |

**Raise history preserved: 6 → 8 → 14 → 15.** The **14 → 15** step is recorded
with its reason: **faithful current-candidate execution of an already-required
control** (fresh C4/noop). **No further automatic raise.**

**The unallocated ten targeted are NOT automatically spendable.** 42 + 8 = 50;
the remaining 10 need their own justification.

**Layer rule, retained T6/T7 precedent:** single-module `lean -o` runs of the
stated shape — **no dependency rebuild, no whole-project or test execution** —
are **TARGETED**. **Count both compiles and every positive and negative
elaboration.** Failed and setup calls keep their actual cost.

## C4 — transfer refused

**Mechanism-identity transfer is NOT accepted** in place of the required
present-but-disabled checker control. The historical C4 receipt is **retained as
input, not as the control**.

**One additional substantive**: a **fresh C4/noop through the actual final
candidate mandatory path**, **restored afterwards**, and **before** the final
clean CI.

## Corrections binding on the phase

**1 — Do not predict certainty.** §1 admits the mutant `Types` stops `Step`
before `Mirrors`; §3 nevertheless lists the two promoted helpers as **O5
CERTAIN**. Those conflict. **O5 is the real earlier-module protection.** The
shadow pair may establish selected-helper sensitivity **only on its own executed
receipt**. **Never call the O5 failure a rechecked downstream proof, and never
present two results as one execution. Record actual failures, not predicted
certainty** — apply this to **every** affected O4/O5 table.

**2 — One resolved argv.** The prior handoff still gives `lake env lean` with the
old miniatures while the supplement says DIRECT `lean` with explicit paths and no
fully resolved argv. Bind **exact cwd, commands, input and output paths,
ordering, shadow construction, and the actual existing clean-input provenance**.

**3 — Withdraw or evidence the "measured" claim.** The argv deviation is labelled
**"probe-proven"** and the lake-appends-paths-last explanation **"measured"**.
**Name the retained receipt and give the resolved search-order explanation, or
withdraw those labels.** Do **not** invent a run or spend a probe to defend the
prose. Explicit shadow-first loading stays a valid instrument choice **if
actually established** by the admitted pair and bound inputs. **A specific
unrelated error is not a negative result.**

**4 — Keep both discovery obligations.** **Module completeness can refuse before
predicate discovery**; a **named predicate diagnostic requires actual import and
classification**. **O2 may share a run only where every claimed diagnostic is
actually reached and retained.** Today's source observation that the exporter
adds no predicates is **not a compiled census measurement** — "new-identity work
measured zero" overstates it. **Keep OT4's prefix-limited scope explicit**; it
cannot stand in for the repaired ownership rule or future-module coverage.

**5 — Freeze before execution.** This sheet and the gate inputs are frozen
**before any phase execution**. **Preserve the actual cost of every failed and
setup call.** If another unbound prerequisite or a concrete fit gap appears,
**return that exact gap before spending on it. Do not narrow coverage to fit.**

## Scope carried forward

Isolated selected-chain probes are **not** a fresh compilation of the original
whole dependent module — **the original production-path runs remain required**.
Verify the **actual copied dependency footprint, selected bytes, namespaces,
imports and loading** before binding. **Nothing here admits a result before
execution.**

Fence unchanged. **No terminal auditor context is reused.** C1 remains next
landing. No push, PR, merge, comment, `#66` closure, S3 execution or Phase-2
grant.

---

## CORRECTION 2026-09-06 — this sheet does NOT yet bind commands

**My defect.** Amendment 2 freezes the **requirement** to bind exact cwd, argv,
input/output and search paths. **It contains no actual resolved shadow compile,
negative or positive command.** Calling it "the single current command sheet"
does **not** discharge its own §2. **The mandate telling the author to construct
a sheet cannot stand in for that sheet.**

**Status: the command-binding prerequisite is NOT met.**

I checked before saying so: the owner's consolidated supplement describes the
shape only — *"Shadow compiles (`lean -o`, single-file elaboration+codegen, NO
lake project…)"* — which is a **class of command, not a resolved one**. No
artifact yet carries cwd, full argv, output paths, search paths and clean-input
identities.

**Before the first changed shadow operation:** the owner produces that concrete
artifact, and **I read, cite, hash and bind it**. If one already exists that I
have missed, citing and hashing it is enough to continue. **No extra checkpoint
once this is met**, and **no blind kill, restart or reset** at any point.

## O1 is preserved

`evidence/S2-O1.log` (167,193 bytes) exists and its `just lean` command was
concrete. **Preserve that evidence and its spend.** **No retroactive claim that a
later command binding preceded this start** — O1 ran before this binding, and the
record says so.

## Search order — credited at its own scope, and my earlier wording corrected

`instruments/S2-lean-env-search-order.receipt.txt` reads:

```
/code/reactivegas-66-s4b/lean/.lake/build/lib/lean:/nix/store/…lean4-4.25.0/lib/lean:/tmp/PROBE_MARKER
```

Project and toolchain paths come **before** the marker — **consistent with the
inherited `LEAN_PATH` being appended last**. The earlier wording ("lake appends
its paths LAST") **reversed what was appended** and is withdrawn. **Credit the
observed order at its own scope**: it establishes the order in that receipt, not
a general loading guarantee. **No new probe is requested.**

## Standing

**Source selected-chain predictions may be reported as executed results only when
their real receipts exist.** Everything else in this sheet stands.

---

## BINDING 2026-09-06 — the resolved commands exist and are BOUND

The prerequisite recorded in the previous correction is **met**. Read, cited and
hashed by me:

| artifact | sha256 |
|---|---|
| `instruments/S2-shadow-resolved-commands.md` | `af82f826bd08ee4b22d4c8b1369a6ef7f1fb53107abc1d55fd4161b867484013` |
| `instruments/S2-chain-P01.lean` | `7bc5c01f…` (matches the artifact's own citation) |
| `instruments/S2-chain-P07.lean` | `9dab73e2…` (matches) |
| `instruments/S2-clean-olean-manifest.sha256` | 29 clean oleans |

**It is resolved, not a shape.** Literal argv, e.g.

```
nix develop --quiet -c bash -c 'cd /code/reactivegas-66-s4b/lean && mkdir -p /tmp/s2shadow/KelGroups \
  && LEAN_PATH=/code/reactivegas-66-s4b/lean/.lake/build/lib/lean \
     lean -DautoImplicit=false -o /tmp/s2shadow/KelGroups/Types.olean KelGroups/Types.lean'
```

with per-operation cwd, inputs, outputs, search paths, restore step, charge class
and receipt path.

**My earlier reading that it contained no resolved argv was a false negative of my
own method** — I grepped for `lean -o` and fenced blocks, while the argv is
inline-backticked in bullets. Corrected here.

### Why it answers the masking finding

- **P01 chain imports `KelGroups.Types` only**, and `LEAN_PATH` is **exactly**
  `/tmp/s2shadow`, which holds **only** the mutant `Types.olean`. **No `Step`
  import anywhere — so `comune_cannot_authorize` can neither fire nor mask.** The
  target is reachable **by construction**, rather than by asserting O5 reaches it.
- **P07 chain imports Types/State/Step** with `LEAN_PATH=/tmp/s2shadow:<lib>` **in
  that order** — shadow first, clean deps behind — so the mutant `Step.olean`
  wins while the dependency environment is preserved.
- **Neg and pos run the SAME driver bytes**, which is the
  otherwise-identical positive control required.
- Neg expects exit ≠ 0 **exactly at** `view_mem_of_isMember` and
  `isMember_of_view_mem`, with `P01-orig` proving alongside as contrast.

### Clean inputs are valid despite O1's failure

O1 failed **at the mirror checker**, which the `lean` recipe runs **after**
`cd lean && lake build`. The build therefore completed, and the 29 manifest
oleans are genuine fresh clean inputs. Recorded so no one later mistakes the
failed O1 for absent clean inputs.

### Standing

**Targeted phase is bound and may proceed**: 8 operations, an exact fit against
the +8 authorization (42 + 8 = **50/60**). **No further checkpoint from me.**

**Substantive remains BLOCKED**: 9/15 spent, 6 left against 7 needed. **The +1 is
a request I cannot grant.** Any unexpected red, extra invocation or unbound
prerequisite returns as a **new exact gap before further spending**.

---

## BINDING WITHDRAWN 2026-09-06 — shadow worlds collide. My binding was defective.

**I withdraw the preceding binding.** Three defects, the first serious and
**missed by me**.

### 1. The two shadow worlds are one directory

`/tmp/s2shadow` holds **both** mutants:

```
SH-P01compile → /tmp/s2shadow/KelGroups/Types.olean      (mutant Types)
SH-P07compile → /tmp/s2shadow/Reactivegas/Step.olean     (mutant Step, compiled against CLEAN Types)
SH-P07neg     → LEAN_PATH=/tmp/s2shadow:<clean-lib>
```

So **P07neg resolves the retained P01-mutant `Types` as well**. Its declared
world — *"only mutant Step, all dependencies clean"* — **is false**, and the
command sequence itself establishes the collision.

**I checked P07's shadow-first ordering and did not check what the shadow
directory still contained.** That is my error, not a later discovery.

**Required:** **separate clean owned shadow worlds**, or equivalent **verified**
isolation, **including exclusion of stale files before first use**. **`mkdir -p`
alone does not establish an empty world.** No extra compile is needed to design
this, and **no production-source accommodation is authorized**.

### 2. Bind full hashes by file, not copied prefixes

Computed by me, per file, full:

```
7bc5c01f971ece0df537156a1e405a6bde42c77c481d12df39c7ec14d8d079e0  S2-chain-P01.lean
9dab73e2a543ab2dcd3e1356debb34cf68d3a089635a8a601b93463da11962eb  S2-chain-P07.lean
531eb3e919ae02be00df3a6dbd3e6619648b9fa75125f7c50e901503de676274  S2-witness-close.lean
ab3dd269f3a8c65096bc1030a393d37e2896cd8f9557d90cb7eab33da83b8ce0  S2-census.lean
```

The command sheet's P01/P07 attributions **agree with these files**; the
**consolidated return's mapping disagrees** and must be corrected to match.
**Bind these full digests; do not carry 8-character copies as identity.**

**My previous binding said the driver hashes "match your own citations" — I
checked internal consistency between documents, not identity against the files.**
That is a weaker check than I described.

Also bind: the **syntax-repair commit `b667648`** and the **actual clean-olean
manifest provenance**. **O1 failed at the checker**, so whatever module builds it
completed may be retained **honestly at that scope** — never called a successful
whole O1.

### 3. Section 3's order contradicts itself

It reads *"O1-retry?? — NO: O1 already ran … O1-retry is NOT authorized"* and
then *"otherwise the program stops at 15/15 after O1-retry"*. Replace with **one
executable order and one remaining-cost table** matching the grant below.
Retained O1 module outputs may be used **for their proven scope**; **no
gratuitous duplicate cold build**. **Whole-path O1 retry and final clean CI
remain required.**

## Grant recorded

**Owner ceiling 15 → 16.** 9 spent, **seven remaining** — O1-retry, O2, O3, O4,
O5, noop, O6. Reason: **retry after the counted failed O1**, preserving every
original remaining control and the final CI.

**Raise history: 6 → 8 → 14 → 15 → 16.** Submission **2/2**. Targeted
**42 + 8 = 50** within an unchanged **60**. Fresh-full-auditor **15 / 69**
unchanged. **No automatic further retry or raise. The parent executes no Lean.**

**Targeted operations are HELD** until the shadow-world separation is bound —
the collision affects P07neg directly.
