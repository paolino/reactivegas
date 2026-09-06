# S4 — consolidated disposition of FS-01, FS-02 and the documentation boundary

Owner `%503`. Local delivery only. Candidate unchanged:
`04eb6c7d9aeb2a3602fca5ece14cbc033221cb43`, tree
`caaa0488f39a6afb2553680a11fd6bfd86d1c90b`. No edit, no new submission, no
acceptance, no push, no PR, no merge, no `#66` closure.

Auditor `%576` is **TERMINAL at 2026-09-06T07:06:01Z** with verdict
**AUDIT-FINDINGS**, report `43db90494fbad83282092d388382651d9f2d56e7aefe30da7b2c66e08443fe9c`.
The clean CI completed **07:07:14Z, after that verdict**. It is **not** admitted
to `%576`'s verdict and `%576` is not reopened, restarted or narrowed.

## 1. FS-02 — discharged here, and my false claim corrected

FS-02 was correct. The ruling required recording in the **#66 issue body and the
closure map**; only the issue body was written.

My own `handoffs/S4-D1-FIT-VERIFICATION-AND-MY-WITHDRAWAL.md` line 79-80 states
*"The ruling is recorded in the **#66 issue body and the closure map**, never as a
comment"*. **That sentence was false when written.** It restated the requirement
in the perfect tense as though discharged. I did not verify the map before
asserting its content — the same class of error as relaying a digest without
computing it. The independent auditor caught it; I did not.

**Repair, at source.** `handoffs/CLOSURE-MAP.md`:

| | |
|---|---|
| before | `b5f304bca000d24354d8ebd642f14a078b31c17a25c9a0207b478f762c8e0427`, 404 lines |
| after | `3faf09c287ed848590245b466ffa7b0fe3b0fade098c0e038a39084b60ab0afe`, 454 lines |
| lines removed | **exactly one** — `    #68 audit (NOTE-023). See the row below.` → same text with `;` so the S5 bullet list continues |

Pre-repair copy frozen at
`handoffs/CLOSURE-MAP.pre-fs02-preserved.md`. The map's prior omission is preserved
because every other earlier line is untouched; the correction is stated in the
new section rather than by rewriting history.

The new authoritative section sits immediately before `## Current owed list` and
carries: the dated ruling `RG-S4-REACH-20260906` and its scope; the explicit
statement that it is neither an undecidability claim nor an absent-callers
inference nor pre-existing authority; the three precise distinctions between
finite replay and `Reach` (**genesis** — arbitrary `Trace.initial` vs
`Reach.boot` requiring `State.empty` plus comune exclusion; **fixed view/auth**
vs integrated `apply` histories that change the view; **refusals** —
`TraceResult.refused` retains state and continues vs `Reach.trans` covering only
successful `stepEvent`); the **OPEN finite-history correspondence** as an owned
**S5** obligation retaining **#75** replay and **#71** reporting dependencies,
explicitly not waived by S4 landing; S5's unchanged retention and
`ONWARD-68-INV-01` inversion-exactness obligations; **H-01 / H-02 / H-03 OPEN on
their original terms**; and **F-001 OPEN** as the immutable historical finding at
`94bb7bb`, which the current-boundary classification at `04eb6c7d` does not
retroactively close. The S5 entry in `## Current owed list` gained the
finite-history correspondence as a third bullet.

No `docs/en/design/` file was touched.

## 2. FS-01 — the clean-final receipt, assessed at source

Receipt: `commit-owner-s4b-muse/handoffs/evidence/S2-CI-final-clean.receipt.txt`.
Log: `.../S2-CI-final-clean.log`, digest
`fbc50e0d75d365f3d0a60d5641d24c58d65f3270dd70f81b7051072164818564` —
**recomputed by me, matches**.

### 2.1 What it establishes

| # | Fact | Evidence, verified by me |
|---|---|---|
| 1 | Exact candidate identity, clean | `git rev-parse HEAD HEAD^{tree}` in `/code/reactivegas-66-s4b` returns `04eb6c7d…` / `caaa0488…`; `git status --porcelain --untracked-files=all` returns **0 lines**. Receipt's own before/after porcelain blocks are both empty. |
| 2 | Command, cwd, exit, wall | `nix develop --quiet -c just ci` from `/code/reactivegas-66-s4b`, **exit 0**, `07:05:25Z → 07:07:14Z` (109 s). |
| 3 | The sequencing defect is gone | This log opens `check-lean-toolchain: pinned=4.25.0 running=4.25.0`. `grep -c "is dirty"` → **0**, against **2** in the superseded `S2-CI-comment-only.log`. The old log is preserved unchanged. |
| 4 | The source-sensitive consumer executed **in this run** | `MIRROR-RECEIPT-WROTE nonce=1788678416820927632` — a per-run nonce, so the driver was regenerated and elaborated here, not replayed as a whole. The full `MIRROR-KIND-CENSUS` / 19 `MIRROR-ROW` / 2 `MIRROR-PROMOTED` / 4 `MIRROR-EXCEPTION` enumeration is re-emitted. |
| 5 | That consumer actually loads the repaired module | The generated driver's own text carries `import Reactivegas.Mirrors` (`scripts/check-lean-mirrors:60`) and attributes declarations by module home (`:379`, `home == "Reactivegas.Mirrors"`). This is precisely the declaration-range / module-metadata consumer class the desk enumerated, running against the final committed bytes. |
| 6 | The repair shifted no census value | `MIRROR-CHECK-OK rows=19 exceptions=4 discovered=24 promoted=2 tracked=29` — **byte-identical** to the pre-repair run. `MIRROR-EXCEPTION Reach OK` present; `MIRROR-BELOW-EXCLUDED Reach.below`. `s4bExceptions` lives in the script (`:155`), not in `Mirrors.lean`; `exceptions=4` confirms membership unchanged **at execution time**, not merely by diff. |
| 7 | Controls that can fail did fail on cue, in this same run | line 3 `negative-control: mismatch detected`; line 36 `negative-control: withheld=backdonate source=Reactivegas.step_backdonate_inv detected=yes`; line 8 `lean dependency direction: OK (control imports=19)`. |

### 2.2 What it does NOT establish — stated as limits, not softened

**(a) This is a cache-assisted run. It is not a cold build, and I do not call it
one.** Across three lake builds (27, 42, 42 jobs) **every** `[n/m]` progress line
in the log reads `Replayed` — 17 of them — and there is **no `Built` line
anywhere**. Lake emits a progress line only for a job that produces diagnostics,
so this does **not** prove that nothing compiled; it proves that **no
diagnostic-bearing module compiled fresh**. Either reading leaves it warm.

**(b) Therefore this receipt supplies no new axiom evidence.** The `#print axioms`
info lines at `Reactivegas/Invariants.lean:1641`, `:2351`, `:2352`, `:2353` sit
under a `Replayed Reactivegas.Invariants` line: they are **cached diagnostics
re-emitted**, not recomputed. Under the standing rule that `#print axioms` is
valid only on a genuinely clean build with a fresh `.lake`, the axiom claim
continues to rest on its **own earlier cold receipt at its own identity**,
unchanged and not re-established here.

**(c) The repaired module's own build/replay status is not readable from this
log.** No module imports `Reactivegas.Mirrors` — it is a leaf, consumed by the
mirror-check driver rather than by the Lean import graph — and it emits no
diagnostics, so it leaves **no `Built` and no `Replayed` line**. What row 5 above
establishes is that the driver imported it successfully at the final committed
bytes and produced an unchanged census; it does not establish whether its olean
was recompiled here or replayed. **This is the residual I hand to the
supplement.** The `Replayed Reactivegas.Invariants` line cannot cover it, because
`Invariants` does not import `Mirrors`.

**(d) Provenance of the replayed artifacts.** A `Replayed` line is Lake asserting
input-hash equality between the cached artifact and the current source, so the
cached oleans correspond to the **committed 04eb6c7d bytes** regardless of which
earlier run produced them. That is a hash argument, not an observation of the
producing run, and it is offered as exactly that.

### 2.3 Budget

Owner **20 substantive / 52 targeted — now spent.** No further project operation
is granted or requested by me.

## 3. The documentation boundary, as the desk redrew it

I record the desk's own withdrawal rather than restating it as proved: the
unqualified claim that a documentation-only repair has **no** source-sensitive
effect **is withdrawn** and is **not** treated as established. It was overstrong
in the commissioning condition.

**Allowed observed effects** of this exact two-site repair: module and declaration
doc metadata; declaration source ranges (the edit sits in a header doc comment at
`Mirrors.lean:29-32`, so every declaration below it shifts); raw generated driver
documentation bytes; and source/build hashes induced by that text.

**Required unchanged, and established by the full static audit `43db9049…`:**
non-comment program bytes, proof statements and terms, exception membership,
imports, runtime and checker logic. I claim **no** literal byte-identical
generated source and **no** zero-metadata effect. Old compiled artifacts, ranges
and source hashes are **not** interchangeable with new ones; the prior mutation
executions keep `94bb7bb` identity and their own timestamps. **No semantic failure
is waived by any of this.**

## 4. Disposition

- **FS-02 — CLOSED at my altitude** by the map repair above, with my false claim
  corrected in the record rather than deleted.
- **FS-01 — evidence supplied, with (a)–(d) as stated limits.** Whether that
  evidence discharges the finding is for the independent supplement, not for me.
- `%576`'s terminal verdict stands untouched. **F-001, H-01, H-02, H-03 remain
  OPEN.** S3 and S5 remain required and open. `#66` is not closed and is not
  proposed complete.
