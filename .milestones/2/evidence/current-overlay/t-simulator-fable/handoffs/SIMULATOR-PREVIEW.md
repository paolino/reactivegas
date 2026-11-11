# Simulator preview — latest built candidate, running now

## URL

http://127.0.0.1:8791/

Loopback-bound on this host (no network exposure). If you are reading this from
another machine, tunnel it:

ssh -N -L 8791:127.0.0.1:8791 epyc

## What it is serving — exactly

| field | value |
|---|---|
| candidate | `48f76d96eb0975ec6c21cc5ba490af196d4882fa` |
| branch | `feat/economics-simulator-fable` (repo HEAD, tree clean) |
| accepted base | `efef604de87b2a1efae51e84d1a9150e585c1db0` (S4 landed) |
| asset | `economics-simulator.html`, **single self-contained file**, 345 636 bytes |

**This is the latest built candidate** — not an older preview. `48f76d96` is the
current branch head.

## Asset-to-build binding

```
candidate blob sha256   c3bf4b3adc76354e2351da7bbf117508be7bf87817dea9c8fc50c9904b801eec
served over HTTP sha256 c3bf4b3adc76354e2351da7bbf117508be7bf87817dea9c8fc50c9904b801eec
                        IDENTICAL
```

The page has **zero external script/style/image references**, so the single file
is the whole application — nothing else is fetched or served.

**No build was run.** The page is the built artifact already committed at the
candidate, and the completed v17 gate verified `build --check` 13/13 — the core
slices are byte-identical to the generated page. I served the git blob directly.

## HTTP check

```
status 200 · 345636 bytes · text/html; charset=utf-8 · 6.5 ms
title: Reactivegas — simulatore economico
```

## What you are looking at

This candidate carries the C1 repair work: substrate string keys preserved
through the UI, receipt-driven chrome, the signer-authority refusal, the
spelling-independent control discovery, and the **expanding-rings geometry** you
ruled on — both rings grow on a pannable canvas, all purchases shown together at
the full 92-unit separation, verified at counts 8, 9, 10 and 103.

## Status of the code, so the preview is not mistaken for acceptance

**C1 is NOT accepted.** `gate-v17` is green end to end, but final local CI is
**blocked on #92** (the S4-B mirror checker derives its tracked-module set live
while its generated driver's imports are fixed; quality owner `%503` owns that
repair). The independent full audit has **not** run against this candidate.

**Treat this as a preview for testing, not a release.**

## Lifetime

Temporary, tied to this session. To restart it:

node /tmp/reactivegas-sim-preview/serve.mjs

---

# Remote access — and why I did not publish to the existing public route

## The existing route is STALE. Do not treat it as latest.

https://preview.dev.plutimus.com/lambdasistemi/reactivegas/sim-fable/

Retrieved by me just now: **HTTP 200, 315 891 bytes**, sha256
`6899ab46a595777ce0ca5fbeaaa4575c81856cca585f8f0de37d8b91f60e19a7`.

The candidate is **345 636 bytes**, sha256 `c3bf4b3a…`. **They differ.** That
route is serving an older artifact and is **not** tracking the worktree — the
worktree file is byte-identical to the candidate, so the watcher that once fed
that route is no longer picking it up.

## Why I did not overwrite it

- Its recorded authority (`publish-batch1.md`, from `NOTE-031`) is
  **"publish the accepted slice only"**.
- **C1 is not accepted** — final CI is blocked on #92 and no independent audit
  has run against this candidate.
- Putting unaccepted bytes at a URL that historically carried the *accepted*
  slice would **misrepresent the artifact** to anyone who finds it.

I also hold no established mechanism for a candidate-specific public route; that
publishing path is not mine and I have not been granted it for an unaccepted
candidate.

## What works right now for a remote operator — no publication needed

Tunnel the loopback preview over your existing ssh access:

ssh -N -L 8791:127.0.0.1:8791 epyc

then open

http://127.0.0.1:8791/

That serves the candidate bytes exactly, verified byte-identical over HTTP, with
nothing exposed to the network and nothing published.

## The obstacle, stated plainly

If a **public** candidate-specific route is wanted, it needs either an
authorisation to publish an **unaccepted** candidate to a clearly
candidate-specific path, or the desk to point me at the publishing mechanism.
**I will not repurpose the accepted-slice route for unaccepted bytes.**

---

# Restart record — the first server died; it is now detached and stable

The initial preview process was terminated (**SIGTERM, exit 143**) when its
background task ended, so the URL was briefly dead. **Relaunched detached**
(`setsid`), and it now survives the session:

```
pid 115919   PPID 1   LISTEN 127.0.0.1:8791
```

Re-verified after the restart:

```
status 200 · 345636 bytes
served sha256  c3bf4b3adc76354e2351da7bbf117508be7bf87817dea9c8fc50c9904b801eec
               identical to the candidate blob
three consecutive checks over ~4 s: 200, 200, 200
```

**http://127.0.0.1:8791/** is live.

If it ever stops, restart with:

setsid nohup node /tmp/reactivegas-sim-preview/serve.mjs > /tmp/preview-server.log 2>&1 &

---

# SUPERSEDED — the real preview is live. Use this URL.

The loopback server above is **stopped and withdrawn**; it was never a product
preview. The operator review surface is:

https://preview.dev.plutimus.com/paolino/reactivegas/pr-94/simulator/

Verified independently by me, not taken from the run:

```
status 200 · 345636 bytes · text/html
served sha256   c3bf4b3adc76354e2351da7bbf117508be7bf87817dea9c8fc50c9904b801eec
tracked at head c3bf4b3adc76354e2351da7bbf117508be7bf87817dea9c8fc50c9904b801eec
candidate blob  c3bf4b3adc76354e2351da7bbf117508be7bf87817dea9c8fc50c9904b801eec
title           Reactivegas — simulatore economico
```

PR head `c037bf4c7fe5cf6f13786f11eee00d02418d0368`, run 34030504905 attempt 2
SUCCESS.

https://github.com/paolino/reactivegas/pull/94
https://github.com/paolino/reactivegas/actions/runs/34030504905

**Still unaccepted.** PR #94 stays draft until the independent simulator
acceptance and the mandatory CI blocker (#92) close.
