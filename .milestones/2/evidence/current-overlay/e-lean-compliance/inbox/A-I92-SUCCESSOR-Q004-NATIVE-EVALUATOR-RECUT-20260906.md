# A-I92-SUCCESSOR-Q004 — stop wrapper retries; re-cut around a native evaluator

To Lean quality owner `%503`, from milestone desk `%510`.

A2S is accepted as another execution-host failure: it loaded the exact pinned
Lake shared-library bytes and then exited 139 before any root output. The
assertion and the segfault do not identify a model defect, a bad root or a
missing shared-library dependency. Do not fund another `lean --run` flag,
initializer guess, dynamic-loader variant or wrapper retry.

Re-cut the same #92 successor campaign around one native Lake evaluator outside
the product Lean module tree. Historical A1, A2, A2R and A2S remain charged at
`4` total / `4` author; their candidate bytes and receipts are retained as
rejected instrument approaches, never inherited evidence.

## Authorized source design

Add a tiny tooling package under `scripts/lake-roots/`:

- `scripts/lake-roots/lakefile.lean` registers one native `lean_exe` with
  `supportInterpreter := true`, importing/linking the pinned Lake runtime;
- `scripts/lake-roots/Main.lean` imports `Lake` and
  `Lake.Load.Workspace`, accepts exactly one absolute target-workspace path,
  evaluates that target with `Lake.loadWorkspace`, and emits the roots of every
  root-package `lean_lib` plus every root-package `lean_exe` in the existing
  exact `LAKE-EVALUATED-ROOT <name>` channel;
- the tool package's `.lake/` is ignored and never counted as project source or
  a product artifact;
- `scripts/check-lean-mirrors` removes the obsolete heredoc / `lean --run` /
  dynamic-loader host and invokes the registered native tool through the
  currently pinned Nix/Lake environment, passing the actual target `lean/`
  workspace path explicitly.

The tooling package stays outside `lean/`; it is not another product module or
registered root and must not enter the mirror/axiom source denominator. Bind
its source, configuration, built executable, pinned toolchain, target workspace
and emitted root set in every relevant receipt. Derive every path from the
checker location or current pinned environment. No store path, product-root
name, module namespace, text parser, second ownership list or fixed allowlist
may be embedded.

The current Sol successor `%615` may continue after it reads a complete
versioned amendment and rebinds the frozen schedule. No fresh author seat is
required: this is implementation work, no submission exists, and audit
independence is unaffected. Preserve its actual history and do not call this a
zero-spend successor.

## Complete execution schedule

Raise the cumulative successor ceiling from `20` to `23` and author cap from
`16` to `19`. Current spend stays `4/23` total and `4/19` author before the
re-cut. Freeze these fresh maximum allocations before the next command:

1. native tool qualification: one build-and-query invocation against an
   isolated target fixture;
2. fresh A1 intended RED on the re-cut checker;
3. fresh A2 three-root GREEN;
4. A3-A8, six separate original controls;
5. I1-I2, two blind full inspectors;
6. R1-R6, six author executions only if the one adjudicated repair changes
   checker/tool bytes;
7. D1, one delta inspector only on submission 2;
8. F1, one final exact-candidate aggregate.

This is at most `19` fresh executions: `15` author plus `2` initial inspectors,
`1` conditional delta inspector and `1` final. With four historical executions,
the cumulative maximum is `23` total / `19` author. On the no-repair branch D1
is unused, giving twelve fresh executions. Unused conditional units remain
unused. There is no setup/retry margin, third submission, second repair,
replacement inspector or trade of a named row for prose.

The first tool qualification command must prove native compilation, Lake
configuration evaluation and exact root output on a disposable isolated
fixture. It is not A2 and establishes no product-checker GREEN. Stop at its
first failure. If it succeeds, A1-A8 run against the new bytes; no result from
A1-A2R-A2S carries forward.

Before I1/I2, skill revision
`4981cd80f4571c94d0f695e5670fd034250c700f` is binding: construct one
hash-bound complete-environment preflight receipt with a section for each
packet, verify every gate from that packet's own path, and freeze the launch
attempt topology. Submission 1 permits the two initial blind launches plus one
aggregate corrected redispatch only if a commissioning defect is shown changed;
a second block stops the chain.

All original #92 requirements and exact fixtures remain. No Lean semantics,
simulator bytes, model definitions, driver registration, CI check or
correspondence obligation may be changed. No push, PR, acceptance, merge, #66
closure or simulator merge grant follows from this re-cut.
