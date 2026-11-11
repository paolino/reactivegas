# S4 D1 — operation fit verified, and a claim of mine withdrawn

Owner `%503`. Static; nothing spent. Two things the ruling required of me before
anything else: withdraw an incorrect claim, and verify the one authorized
operation fits **before** it is spent.

## 1. My "exactly the same thing / nothing connects them" is WITHDRAWN

The ruling is right and I verified every part of it at source.

**Finite replay and `Reach` are not unconditionally the same object.** They differ
on three independent axes:

| axis | `Trace` | `Reach` |
|---|---|---|
| genesis | `Trace.initial : State` (`Trace.lean:266`) is **arbitrary**; `emitTrace` takes it as a parameter | `Reach.boot` fixes `State.empty` **and** carries `(h : comune_not_a_member view)` |
| refusal | `TraceResult.refused (guard : GuardClaim)` is a retained outcome | `Reach.trans` extends only on `stepEvent … = some s'`, i.e. **successful** steps |
| view/auth | integrated `apply` histories can **change the view** | `Reach view auth` is parameterised by a **fixed** view and auth |

So my equivalence claim was wrong on all three, and my "nothing connects them"
was inferred from the absence of spelled `Reach.boot` / `Reach.trans`
applications.

**That inference is the exact fallacy I had condemned two paragraphs earlier in
the same document** — "neither the absence of a current consumer nor the absence
of a `Decidable` instance proves the consumer unnecessary". I made the very move I
had just called out, in the same artifact. Absence of spelled constructor
applications is not proof that no connection exists.

**What survives:** the observable the milestone needs is validation of a supplied
finite history, which is executable; an arbitrary-state decision procedure is not
required. **What does not survive:** any suggestion that this makes the
correspondence trivial or already present.

The finite-history correspondence is now an **owned S5 statement-completeness
obligation**, OPEN, with #75 replay and #71 reporting dependencies retained. It is
**not waived to let S4 land**; #66 and the milestone stay incomplete until it is
discharged. Any eventual bridge must establish its **genesis, view/auth premises
and refusal preservation**, and must not let validation of one supplied history
masquerade as deciding existential reachability. **No bridge is implemented inside
this S4 repair.**

## 2. The one authorized substantive operation FITS

The op is one final full local `nix develop --quiet -c just ci`, cumulative owner
ceiling 19, **no additional targeted operations and no retry reserve**.

`just ci` = `lean-toolchain-contract` → `build` → `format-check` → `hlint` →
`lean` → `lean-corpus-gate` → `lean-corpus-verify`.

**Evidence that one operation suffices:** the *identical* recipe already passed at
this exact candidate. The retained `S2-O6.log` from the earlier final cold CI ends
`Build completed successfully (42 jobs)`, `corpus/economic.json: OK`,
`corpus/integrated.json: OK`, `corpus-check: ntraces=5 nevents=32 nsteps=7
live-bound` — the last step of `ci` — with **zero** error lines. The worktree is
at `94bb7bb…`, porcelain empty, 30 oleans present.

**Why a comment-only change cannot disturb it:** `build`, `format-check` and
`hlint` are Haskell-side and cannot be affected by Lean comments or by comments in
a shell script. `just lean` re-elaborates `Mirrors.lean` and its dependents, which
a comment change cannot alter semantically, and the `#`-anchored substitution
defect that broke an earlier run was fixed at `b667648`.

**Residual risk, stated rather than smoothed:** with no retry reserve, an
*environment* hiccup — nix evaluation, a cold cabal store, a network fetch —
consumes the operation. That is not a reason to refuse it; it is a named
condition. **If the run fails at a step unrelated to the change, that is a setup
failure to report as a concrete blocker, not a candidate defect**, and it returns
the actual blocker as the ruling requires.

**Verdict: the operation fits. I am proceeding, and this is the report the ruling
asked for before spending.**

## 3. What I am not doing

No bridge implementation. No executable token, exception membership, proof,
statement, import, nonce or check-wiring change — the repair touches **only** the
Reach justification comments in `lean/Reactivegas/Mirrors.lean` and
`scripts/check-lean-mirrors`. The ruling is recorded in the **#66 issue body and
the closure map, never as a comment**, with no closing wording. Prior text,
history and the terminal `AUDIT-FINDINGS` verdict are preserved. Historical
submissions 2/2 and owner 18 substantive / 52 targeted remain spent. No
acceptance, push, PR, merge, release or `#66` closure.
