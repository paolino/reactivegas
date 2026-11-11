# Ticket owner — `reactivegas#90`, money custody (first economic-core slice)

| | |
|---|---|
| role | ticket owner, one issue-backed PR |
| worker id | `t90-money-custody` |
| runtime root | `/tmp/reactivegas/ms2/e-haskell-impl/t90-money-custody/` |
| parent | epic owner `%504`, `/tmp/reactivegas/ms2/e-haskell-impl/` |
| issue | https://github.com/paolino/reactivegas/issues/90 — **authoritative and complete; read it in full** |
| repo / base | `paolino/reactivegas`, base `origin/master` |
| Lean authority | **`efef604de87b2a1efae51e84d1a9150e585c1db0`** |
| substrate | kelgroups **`933e385d`**, accepted — `GroupView`, `isMemberInView`, `isAdminInView` |
| worktree | create `/code/reactivegas-issue-90`, branch `feat/90-money-custody` |

Load: `orchestrator-contract` → `ticket-orchestrator` → `resolve-ticket` →
`worker-protocol` → `tmux-orchestrator` → `gate-script` → `verification` →
`invariants` → `haskell`, `nix`. **Reload the current files** from
`/home/paolino/.codex/skills`; baseline **`af60ac2`**.

## The mandate is the issue

`#90` carries the four binding refinements in full — predicate boundary,
production wiring in scope, partial step-addressed corpus replay, and the shared
ten-execution budget. **Freeze your gate against it.** I am not restating it
here; a second copy would drift from the first.

Four things I will check, because they are where this slice can go wrong:

1. **`s.votes` absence is supporting inspection, not proof of closure.**
   Acceptance is the behaviour of the four arms **and their frame conditions** —
   untouched state preserved — never all fourteen.
2. **Enumerate every membership/admin query the four Lean arms use** from the
   source at the authority pin. Do not infer a minimal set. `Key` stays
   lossless; no second membership store.
3. **Pick the actual distinguishing witness for each negative control.** A
   mutant the frozen corpus never reaches proves nothing; establish which step
   distinguishes it. All four arms need mutant evidence, and `transferCassa` has
   **zero** corpus steps so it is tested directly.
4. **Minimal deps are a design choice for wasm, not a wasm build.** Nothing here
   closes `#82`, and nothing may say it does.

## Budget — one shared ceiling of ten

Author at most **4**; two initial inspectors, one possible repair, one possible
delta inspector allocated from the same ten. **Allocate before you dispatch.**
Executed nested stages count. Setup failures are logged distinctly and **never
credited as semantic kills**. At most two submissions, one adjudicated repair
batch, no raises.

**If the mandatory command schedule cannot fit, return the exact unmet branch
before executing — never a weaker gate.**

## Seats

- **You:** `codex`, `gpt-6-astra`, effort `high`.
- **Implementer:** alternating ladder, a family that is not yours. Approved for
  the commit-owner seat: `glm` (probationary, one seat, no secrets — satisfied
  here) or `muse`. Verify live argv before admitting `START`.
- **Inspectors:** **`codex gpt-6-astra` high, or authorized `grok-4.6` with at
  most ONE grok seat in this ticket. Never `muse`, never `glm`.** Two blind
  inspectors on submission 1 with **distinct fault scenarios**; **one**
  adjudication into **one** repair batch; **one** delta inspector on submission 2
  if there is one. **No re-adjudication above you** — I will not re-run it.

Model and effort explicit in every launch argv, verified live, with a
post-cursor `START`.

## Authority

Commit locally; push and open a **draft** PR after full local CI green.
**No merge, no deployment, no issue or PR comments.** Upward delivery is your
`STATUS.md` and local files only — **never** a human composer.

Journal at each substantive phase using the **known tag vocabulary only**;
invent no tag. Every stop is `PARKED` with a wake, `BLOCKED` with a concrete
question, a capacity handoff, or a terminal completion. A parked child writes its
resume brief and exits.

Ask me only for a **concrete new boundary or a budget gap**. Everything else is
yours to decide.

You are not alone in the codebase; do not revert edits made by others.
