# S1 acceptance — candidate `fa01779`

Ticket/epic owner decision, `claude-opus-5[1m]`, `%503`.
**S1 is accepted. Merge is NOT requested and NOT taken — that is the desk's.**

## Verdict trail

| stage | seat | result |
|---|---|---|
| submission 1, `09f8230` | `grok` `%513`, fresh, cold snapshot, 4/5 builds | **PASS**, 6 rows KILLED |
| repair `fa01779` | `muse` `%507`, +8/−4, `Trace.lean` only, 4/6 builds | — |
| submission 2, `fa01779` | `grok` `%520`, **fresh context**, cold snapshot, 2/5 builds | **PASS**, **7 rows KILLED, none left unclosed** |

Loop: submission **2/2**, next submission **FORBIDDEN**. Campaign CLOSED at
set-point. Ceiling raises 0/2, none requested.

## What the second audit closed that the first could not

- **`S1-TOTAL`** — new row, and the reason submission 2 exists. Closed on
  **absence of the panic string**, with the `09f8230` log as its positive
  control. Same frozen instrument, byte-identical: **70 panics → 0**, 7147 lines
  → 7, **exit 0 on both sides**. It further confirmed the detector still fires at
  this toolchain by invoking `Name.getString!` on a `.num` directly, and that 70
  numeric-last theorems remain in the environment while the production manifest,
  TraceTests and cold `just lean` emit zero panics.
- **The repair-envelope invariant.** Verified rather than accepted: TraceTests
  log sha256 identical between `09f8230` and `fa01779`, jsonl byte-identical,
  masked-envelope md5 `4309a735ac6448904abf41cd5e94f197` on parent, `09f8230`
  **and** `fa01779`.
- **`S1-AGREE` against the current resolver**, not a replay: a mutant of the
  match as it now stands (`.str _ s` → `.str .anonymous s`) turns the shipped
  script red.

It also discarded one of its own results as a false green — outer-PATH `lake`
wraps are inert under `nix develop` — and re-ran them inside nix. That is the
behaviour that makes the rest of its receipts worth something.

## The one new candidate invariant, and why I am NOT raising it

`INV-S1-CTOR-GETSTRING` — `elabInversionManifest` still calls `ctor.getString!`
on `iv.ctors`. The auditor proposed ADVISORY. **I accept ADVISORY**, and the
reason matters, because I raised the *previous* `getString!` finding to blocking
and consistency of reasoning is what is being tested here, not consistency of
outcome.

| | `INV-S1-GETSTRING-TOTAL` (raised to blocking) | `INV-S1-CTOR-GETSTRING` (accepted advisory) |
|---|---|---|
| extent of the partial call | **uncontrolled** — every `.thmInfo` in the whole environment | **controlled** — constructors of the inductive explicitly named in `inversion_manifest% X`, which production writes as `Event` |
| occurrences observed | **70 panics, already firing** at `09f8230` | **zero**; `Event`'s 14 constructors are all `.str` |
| introduced by this slice? | **yes** — it replaced the total `declText` | **no** — present at base `e6c5924:173`, untouched by the diff (verified: no `+`/`-` line matches `getString!` for it) |

The third row is decisive on its own: repairing a pre-existing partial call that
this diff never touched would be widening S1, which the desk forbade. Carried
into the **S2/S3** assessment instead, where the resolver's other limits already
go.

If the desk disagrees, a third submission needs a ceiling raise — the loop is
exhausted at 2/2. I am not requesting one.

## Residuals carried, both matching the corrected wording

- **R1 — last-component resolution has no declared precedence.** Scoped as *the
  documented syntactic existence check in this slice*, with **zero** measured
  current `Event` last-component collisions. Not imposed by the unqualified
  *rendering* rule (rendering and resolver are independent choices); the probe
  binds a **dummy** inductive and shows no production hijack. Not a permanent
  waiver; carried into S2/S3. The auditor confirmed the owner's packet wording
  matches this evidence.
- **R2 — the agreement instrument compares three coordinates, not covered-sets
  by name.** A same-count permutation passes it; vacuous at `missing=0`;
  instrument A's name↔hypothesis bind and B's `checkCoveredDeclarationBound`
  reject that class when they run. M2 itself will not see it.

## Gate and CI

- Combined gate hash `ad0a4311ccf2ab46`, independently recomputed by both
  auditors, **byte-identical across `09f8230` and `fa01779`** — the executable
  contract did not move under the repair.
- `expectedDeclarations := 163` blob identical across both; the new script holds
  no extent constant.
- Local `just ci` on `fa01779`: **exit 0**, re-run by me rather than taken from
  the owner's receipt.
- Remote CI on PR #79: "Build and check" **SUCCESS**, "Package the provisional
  linux server bundle" **SUCCESS**, "Sync Cabal version" SKIPPED.

## Housekeeping done

Six audit worktrees retired (~165 MiB): `audit-s1-{09f8230,e6c5924,mutant,ctor15}`,
`audit-s2-{fa01779,mutant}`. `git worktree list` now shows one lane worktree,
`/code/reactivegas-lean-compliance` at `fa01779`, porcelain empty. No stray
`.lean` under `lean/`.

## What S1 does NOT close

S1 is one of five. #66 remains open on S2, S3, S4, S5, the #71 content and the
desk-owned semantic ticket from §10. Nothing here claims otherwise.
