# NOTE-036 — ONE final CI on the clean committed candidate. The gap is mine.

## The gap, and whose it is

**My NOTE-035 ordered CI before commit.** You followed my sequence exactly. The
result is that `S2-CI-comment-only.log` opens with **two `Git tree … is dirty`
warnings**, so it is **not** a receipt of a clean committed final run — and the
independent auditor is right to ask. **This is a commissioning sequencing failure
of mine, not a fault of yours, and the log is not to be described as a clean run.**

## What is authorized

**Exactly ONE** additional full local CI, on the clean committed candidate:

```
nix develop --quiet -c just ci
```

- **No code edit. No new submission. No targeted build. No retry.**
- Owner cumulative substantive ceiling becomes **20** (19 already spent).
  Targeted stays **52**. **The failed assurance is not a free budget reset.**

I verified before authorizing: HEAD is
**`04eb6c7d9aeb2a3602fca5ece14cbc033221cb43`**, tree
**`caaa0488f39a6afb2553680a11fd6bfd86d1c90b`**, and
`git status --porcelain=v1` is **empty**, including with `--untracked-files=all`.
**The tree is already clean without touching anything — do not erase any file to
force a clean state.**

## What the receipt must bind, in one artifact

- `git rev-parse HEAD` and `HEAD^{tree}`
- **full `git status --porcelain=v1` BEFORE and AFTER** the run
- the exact command and its **cwd**
- start and end timestamps from a live clock (use
  `/code/llm-settings/shared/skills/worker-protocol/scripts/status-event` for the
  journal; capture UTC in the receipt itself)
- the **exit code**
- a **digest of the complete stdout and stderr**, with the full log retained at a
  **new unique path** — `S2-CI-comment-only.log` is **preserved unchanged**

The accepted base stays `3590c0015b84fd58004bf6fb44dd18b107304c48`. **Invent no
historical envelope**: this receipt describes the run it is, at the time it ran.

If the run fails at a step unrelated to the comment change, that is a **setup
failure**: it consumes the operation and you return the actual blocker. No retry.

## After

Return the receipt. I route it as **additional evidence** to the existing fresh
full static auditor under its **unchanged mandate** — it is **not** a required
verdict and **not** permission to narrow or reopen anything. If that auditor is
already terminal when the evidence arrives, its terminal verdict is preserved and
I return the exact residual review need rather than restarting it.

No push, PR, merge, comment or `#66` closure follows from this.
