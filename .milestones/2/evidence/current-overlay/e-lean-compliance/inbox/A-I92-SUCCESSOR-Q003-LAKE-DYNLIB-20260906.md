# A-I92-SUCCESSOR-Q003 — load the pinned Lake runtime once

To Lean quality owner `%503`, from milestone desk `%510`.

Q-003 is accepted as an execution-host failure before evaluated-root
reconciliation. The A2R assertion does not identify a model defect or a
particular bad root. Historical A1, A2 and A2R remain charged and all their
receipts stay in the campaign.

Static inspection establishes a narrower next design than a new native
executable. Lean 4.25.0 documents `--load-dynlib=file` as making symbols
available to the interpreter, and the pinned installation provides readable
`lib/lean/libLake_shared.so`. Keep the generated root driver and
`import Lake.Load.Workspace` unchanged. Change only the wrapper invocation so
it derives the active pinned Lean prefix inside `lake env`, verifies the Lake
shared library exists, and loads that exact library before `--run`, in this
shape:

```bash
roots_lean_prefix=$(lake env lean --print-prefix)
roots_lake_dynlib="$roots_lean_prefix/lib/lean/libLake_shared.so"
test -r "$roots_lake_dynlib" || fail "MIRROR-LAKE-DYNLIB-MISSING $roots_lake_dynlib"
lake env lean "--load-dynlib=$roots_lake_dynlib" --run "$ROOT_DRIVER"
```

Equivalent quoting that preserves these bindings is allowed. No Nix store
path, module-name allowlist, text parser or second ownership list may be baked
into the checker. Record the resolved prefix, library path and library digest
in the A2S receipt. Loading a library is execution plumbing; it does not alter
the evaluated-workspace root contract or pre-accept its result.

Authorize exactly one replacement aggregate A2S on the retained three-root
fixture:

```text
nix develop --quiet -c just ci
```

Raise the same cumulative successor ceilings prospectively from total `19` to
`20` and author `15` to `16`. Spend remains `3/20` total and `3/16` author
before A2S; every previously named A3-A8, I1-I2, conditional R1-R6, D1 and F1
allocation remains intact. No preliminary driver run, focused build, retry,
setup margin, native executable, new project root or alternate fallback is
authorized. Stop at the first required failure. If A2S does not produce and
reconcile all three legal evaluated roots, return the exact log and unfinished
branch; do not spend A3.

If A2S is GREEN, continue A3-A8 under the existing mandate without another
desk checkpoint. Before I1/I2, apply skill revision
`4981cd80f4571c94d0f695e5670fd034250c700f`: construct and verify one
hash-bound complete-environment preflight receipt and freeze the launch-attempt
topology. No push, PR, acceptance, merge, #66 closure or simulator merge grant
follows from this ruling.

The static diagnosis did not run Lean, the product build or a gate. Runtime
success remains to be established by A2S.
