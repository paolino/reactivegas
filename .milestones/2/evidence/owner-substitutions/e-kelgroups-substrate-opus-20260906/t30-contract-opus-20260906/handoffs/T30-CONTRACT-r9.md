# T30-CONTRACT-r9 — versioned `#30` contract adopting TAXONOMY-v2

Ticket owner `t30-contract-opus-20260906` (`%572`), under epic owner `%532`.
Authority: `T30-COMMISSION-MANDATE-v4.md`
sha256 `173e0b5fe4af108a98f842b5b9bb2bc1bf533bb9ab9d40ee21b8834b1bfe90e4`, §3.
This is the **one** versioning that note authorizes. Companion:
`T30-COMMAND-MAP-r9.md`.

## 0. Form of this version, and why it is a delta

r9 **binds r8 by hash and replaces only the named blocks.** Everything in r8
not named in §2 below stands unchanged and is incorporated by reference:

| bound artifact | sha256 | status under r9 |
|---|---|---|
| `T30-CONTRACT-r8.md` | `ea6c2019892da2148ef237128156e0aed9ee2e8c123ca38eb8d13baff201e71e` | **governing except §2 below**; preserved byte-intact |
| `T30-COMMAND-MAP-r8.md` | `ca033b1edcd7def8466a90909ceee941d521a948ea4f27c84ea361dc4500b15d` | same; superseded rows named in the r9 map |
| `T30-DRIFT-LEG-r8.sh` | `f0afa32b4fbb13ac6084b6c3c5abd503f7e21f051fef458265b97fd56a4de3e3` | **not a gate input**; defect witness only |
| `T30-DRIFT-LEG-r9.sh` | `69c529ca22e1a798e1ffb1810902243d9f3c2a9223da50d1eef66b2a39833a25` | **the gate leg** |

The delta form is deliberate. Re-typing 34 KB of contract prose to change one
block is the most likely way to widen product semantics by accident, and the
mandate forbids widening. A hash-bound delta cannot drift: every sentence not
reproduced here is the r8 sentence, provably.

## 1. Scope — unchanged, undiminished

Every `R30` row; `S1`–`S6`; every `REQ` ID and per-identity obligation; client
**adapt-only**; integrated refusal, persistence and replay; closure evidence;
the review obligations. Ruled-but-unlanded dependencies stay **distinctly
recorded as dependencies** and are never folded into delivered scope. `#29` and
Reactivegas `#73` remain OPEN. `#33`/`#34` remain downstream and undispatched.

**No product semantics are widened by this versioning.** Nothing in §2 or §3
adds, removes or relaxes a behavioural obligation.

## 2. Superseded statements (mandate §2, carried verbatim in force)

| superseded text | replaced by |
|---|---|
| r8's single-slice / "no S30-2" statement | the decomposition in §4 below |
| the Muse **ticket-owner** assignment | ticket owner **Opus `claude-opus-5[1m]`/high at `%572`**; Muse is the **commit owner** |
| the **r8 leg** as gate input | the **r9 leg**, sha `69c529ca…` |
| "old fixture status: unexecuted" | pf8r2 executed: `SUITE: PASS (baseline=GREEN setup-failures=0 mispredicts=0)`; synthetic 5/5, closed |
| **TAXONOMY-v1** in contract §8 and the command-map block | **TAXONOMY-v2**, §3 below |

No other r8 content is superseded.

## 3. TAXONOMY-v2 — replaces the §8 block

```text
exit 0  PASS    — all gates green on resolved inputs.
exit 1  RED     — a verdict was rendered: >=1 subject check failed on resolved
                  inputs (pins, file-sets, hashes, clean-samples, mapping gaps,
                  tripwire diffs, join mismatches, uniqueness dups, count
                  mismatch, exec absences, stale/empty products).
exit 3  REFUSAL — no verdict was possible AND none had yet been rendered:
                  unbound config, unknown mode, missing/unreadable frozen
                  inputs, unresolvable artifacts (0 or >1 candidates), no
                  producer evidence, broken tools.

PRECEDENCE. A refusal raised while a verdict has already been rendered
(OVERALL_FAIL=1) prints its refusal reason AND exits 1. A refusal raised with
no verdict yet rendered exits 3, unchanged from v1.
```

### 3.1 Why, in one line

v1 inverted NOTE-009 §5's own rule. That rule says a setup failure is never a
domain kill; its dual is that **a domain kill must never be reported as a setup
failure**, and v1 did exactly that. For an acceptance gate, a consumer
branching on exit 3 as "instrument broken, ignore the verdict" would discard a
real RED — the dangerous direction of the two.

### 3.2 A10 — the corrected promised exit

`A10` (stale `.hi`) is mapped **RED(1)** in the r8 command map.

| | value | basis |
|---|---|---|
| r8 map promise | **RED(1)** | `T30-COMMAND-MAP-r8.md`, RED(1) row list |
| v1 delivers | **3** | `refuse()` exits 3 unconditionally after the `3-fresh` RED skips emission and `4-missing` refuses |
| v2 delivers | **1** | observed, `scratch/pf8r2/cases/A10/exit` |

**Adopting v2 makes the map's own promise true.** It does not change what A10
must observe; it corrects the status under which the observation is reported.

**v1 behaviour here is source-derived, not an executed comparison.** pf8r2 ran
**v2 only**; `TAXONOMY_V2=0` was never executed, so **no v1 execution exists to
cite**. The v1 cell above is read from the r8 leg's `refuse()` body. Stated
plainly so no reader mistakes it for a measurement.

### 3.3 A27 — the newer control

`A27` is a **new r9 control**, not an r8-map row: a correct dump is pre-seeded
and emission is skipped, so the join must refuse an artifact this run did not
emit (`4-provenance`). It is the second of exactly two cases in which a refusal
follows a rendered verdict, and therefore the second row where v1 and v2 differ
at all. Determined mechanically over the invocation-5 streams: **A10 and A27
are the only two of 31.** Every other row is pure-RED or pure-refusal and v1
and v2 agree on it.

### 3.4 Error identities are preserved — binding

v2 changes **only the exit status a consumer branches on after a verdict has
been rendered.** Explicitly, and enforceable by reading the leg:

- **No reason line is removed.** Every `DRIFT-FAIL:` and `DRIFT-REFUSE:` line
  v1 printed, v2 prints, with identical text.
- **No identity is merged.** The refusal keeps its own message and its own
  cause; the rendered RED keeps its own. Both appear in the same stream, and a
  `DRIFT-NOTE:` line names the precedence that was applied.
- **An accumulated RED never becomes a no-verdict label.** That is the whole
  correction.
- **An accumulated RED never becomes a PASS because a later step refuses.**
  A refusal cannot lower `OVERALL_FAIL`; the only transition v2 adds is
  `3 → 1`, never `→ 0`. `FINAL: PASS` is gated on `OVERALL_FAIL -eq 0` and is
  unreachable from any refusal path.

## 4. Decomposition (supersedes r8's single-slice statement)

| slice | content | compiler contact |
|---|---|---|
| **S30-0a / S30-0b** | real `KelGroups.Event` and `KelGroups.Server.JSON` metadata / selection / freshness evidence on accepted `933e385d`, in an isolated cold worktree/export | **first, before any candidate** |
| **S30-1** | extent declarations: `lib/KelGroups/Vote/{Types,State}.hs`, Lean-mirrored identities, `exposed-modules` | second |
| **S30-2…n** | the behavioural rows in the r8 map's order: R30-1 open; R30-2 placement/switch/recast; R30-3 sweep/closure/retention and non-duplication; R30-4 verdictOf; R30-5 refusals produced; R30-6 franchise; R30-7/14 negative delivery; R30-8 route separation; R30-10 mechanism surface; R30-12 client adapt-only | each slice |
| **S30-final** | replay/closure evidence, `Trivial` presence, full `just ci`, tracked-clean both ends, founding guard | full CI |

Client, integration, replay and closure are **slices in this sequence**, never
deferred past the compiler boundary. R30-9 (`#68`-gated rebind), R30-10U/11,
R30-13 and R30-X fences remain in the mandate unchanged.

### 4.1 Freeze prerequisites are temporal (mandate §4)

Requirements, expected controls and scripts are bound **before subject
execution**; candidate-dependent rows are demonstrated **before
GREEN/acceptance**. A test of missing Vote outputs cannot be a prerequisite for
permission to create those outputs. **No obligation is waived by this
ordering** — every row is still owed, only later. Candidate-specific outputs
and emitted pins are **evidence on the actual candidate**, never assumed facts
at the initial freeze.

Transfer rule: reusing **toolchain knowledge** across module sets is
legitimate; transferring **uniqueness, mapping or verdict evidence** across
module sets is not.

### 4.2 Channel independence — no fabricated signature

Source/metadata-channel independence remains **required**. Carried correction:
**an unexported source edit is not guaranteed to leave an entire
`ghc --show-iface` dump unchanged.** The gate therefore either binds a
**concrete can-fail observation** on the actual candidate, or the finding is
**returned honestly as a design limitation**. Fabricating stability, or
normalizing away load-bearing dump data to make the expected signature appear,
is forbidden.

## 5. Budgets — replaced by the mandate's grant

The r8 §9 figures were **proposals**. The granted, binding, cumulative ceilings
from product spend 0 are:

| seat | ceiling |
|---|---|
| owner | **28 substantive whole-project operations / 22 targeted probes** — the 26-unit candidate envelope **plus** S30-0a/0b, which **add to** and never replace candidate B3/B22a |
| author submissions | **max 2 total** (one normal repair), **sharing that same ceiling** — no separate repair pool |
| auditor | **25 substantive / 24 targeted**, cumulative across **max 2** fresh per-submission audits |

Counting rules, binding: every whole-project build/test/CI invocation counts; a
hash-only source restore is not a verification build but is itemized and
charged if run; a "narrow" probe that compiles beyond its declared scope **is**
a whole-project operation and counts as one; no parallel heavy builds; no
automatic raise or reset on branch, model or submission change. If the work
cannot fit, the **exact additional operation and scope is returned before**
spending beyond the bound.

The auditor inherits **no** semantic acceptance from the synthetic PASS.
Conditional `A-REBIND` stays **unspent with its reason recorded** while its
dependency is unlanded. Synthetic 5/5 and all S28 history stay separate and
spent; **no sixth synthetic run.**

## 6. Frozen semantics and fences

Reactivegas frozen at `3590c0015b84fd58004bf6fb44dd18b107304c48`. **No new Lean
edits. No threshold default. No expiry. No dormant refusal producers. No
`#81`/`#76` implementation by anticipation.** If `#68` or another accepted
relevant base lands, an explicit overlap/fit assessment comes first — no
automatic budget, no acceptance inheritance.

Delivery: local commits authorized; push and **draft** PR only after full local
acceptance, a qualifying independent audit, and real remote CI at the exact
head, with **no closing keywords** for `#29`/`#73`; no merge, no release, no
issue comments, no deployment, no `#33`/`#34` dispatch.

## 7. Seats

| role | seat | pin |
|---|---|---|
| ticket owner | `%572` | `claude --dangerously-skip-permissions --model 'claude-opus-5[1m]' --effort high` |
| commit owner | one Muse | `muse --approve` (Pi / opencode-go / `muse-spark-1.3-contributor` / xhigh) |
| auditor | one fresh per admitted submission | Codex `gpt-6-astra`, effort **high**, explicit in live argv |

Never Muse/GLM/Claude auditing. No provider fallback or model substitution by
helper default. Terminal roots from earlier campaigns are **retained inputs,
never resumed audit contexts**.
