# NOTE-020 — before the first shadow operation: give me the resolved commands

**Do not interrupt a running command, and do not spend a repeat build to correct
documentation.** O1 is preserved exactly as it stands.

## My defect, corrected in the sheet

Amendment 2 froze the **requirement** to bind exact cwd, argv and paths — **it
contained no actual resolved command**. Calling it "the single current command
sheet" did not discharge its own §2, and **a mandate telling you to build a sheet
is not that sheet**. Updated sheet in `admitted/`, hash
`299a7c753a130291a45403686e124bb4`, `MANIFEST.sha256` regenerated.

## What is needed, before the first changed shadow operation

A **concrete artifact** carrying, resolved and literal:

- **cwd** for each operation;
- **full argv** — the actual `lean -o …` line, not the class of command;
- **input and output paths**;
- **search paths** as they will actually be set;
- **clean-input identities** — which prebuilt oleans are used, and their hashes;
- **shadow construction** — how the shadow tree is assembled, and in what order.

I checked your consolidated supplement first: it gives *"Shadow compiles (`lean
-o`, single-file elaboration+codegen, NO lake project…)"*. That is a **shape, not
a resolved command**. If a concrete artifact already exists and I have missed it,
**say which file** — citing and hashing it is enough and we continue immediately.

**I read, hash and bind it. Then you run — no further checkpoint from me.**

## O1 stands

`evidence/S2-O1.log`, 167,193 bytes, with a concrete `just lean` command.
**Preserved, and its spend counts.** Nothing here claims a later binding preceded
it — O1 ran first and the record says so plainly.

## Your search-order receipt — credited, and it corrects the earlier wording

`instruments/S2-lean-env-search-order.receipt.txt`:

```
/code/reactivegas-66-s4b/lean/.lake/build/lib/lean:/nix/store/…lean4-4.25.0/lib/lean:/tmp/PROBE_MARKER
```

Project and toolchain paths sit **before** the marker — **consistent with the
inherited `LEAN_PATH` being appended last**. The earlier "lake appends its paths
LAST" wording **had it reversed**, and this receipt is the thing that settles it.

**Credited at its own scope:** it establishes the order in this receipt. It is
not a general loading guarantee, and it does not by itself license the
shadow-first claim. **No new probe is requested** — do not spend one to defend
prose.

## Standing

**Report a selected-chain prediction as an executed result only when its real
receipt exists.** The O5 earlier-module halt and the shadow-pair sensitivity
remain two different observations until each has its own receipt.

Budget unchanged: **7 new substantive / 8 new targeted**, ceilings **15 / 60**.
No blind kill, restart or reset. End with a terminal **COMPLETE**, a named
**BLOCKED**, or a capacity report.
