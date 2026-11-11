# T30-GATE-CONTRACT — the `#30` gate, frozen before any implementation START

Ticket owner `t30-contract-opus-20260906` (`%572`). Governed by
`T30-COMMISSION-MANDATE-v4.md` (`173e0b5f…`) and `T30-CONTRACT-r9.md`
(`7c8ec26c…`). Frozen **before** the commit owner's `START`, per mandate §4.

## 1. Temporal freeze, restated as it binds here

Requirements, expected controls and scripts are bound **now**, before subject
execution. Candidate-dependent rows are demonstrated **before GREEN/acceptance**
and are listed in §5 as owed. **No obligation is waived by this ordering.**
Candidate outputs and emitted pins are evidence **on the actual candidate**,
never assumed facts at this freeze.

## 2. Instruments frozen here

| instrument | sha256 | role |
|---|---|---|
| `instruments/d4-identities.sh` | `cc28ce1a4c75b26e1a4bd9c8d9217d1b6f225926ac5d7e025fa2b30dbe226eb4` | derives one exact identity per line from a real `--show-iface` dump |
| `instruments/d4-identity-control.sh` | `ac011e14c090bf275ac31a7d0000488997bafc426e3433824a73f1d434b72b13` | the can-fail control for the above, on real dumps |
| `scratch/pf8r2/T30-DRIFT-LEG-r9.sh` | `69c529ca22e1a798e1ffb1810902243d9f3c2a9223da50d1eef66b2a39833a25` | drift leg — **amended only as §3 states** |
| `evidence/s30-0/probe.sh` | `fa16bb9c085fc9097a7d174442edeef73bb0a96e2abf6e8fa9386a904b8c56c9` | S30-0 metadata probe, already executed |

## 3. F-A — the D-4 matching rule, redesigned and bound

**Defect, on real bytes.** The r9 leg's D-4 join tested
`grep -qxF -e "$hs_expect" "$dump"` against the **raw** dump. Real export lines
are two-space indented with the member set in braces, so a bare type name is
never a whole line: `grep -qxF -e GroupEvent` on
`evidence/s30-0/s30-0a-Event.dump` is **ABSENT**. The rule could never match.

**Rule as bound.** The join matches against the **derived identity view**
produced by `d4-identities.sh`, one identity per line, and keeps
**`grep -qxF`** — whole-line, exact. Grammar, read from real dumps:

```text
exports:
  TypeName{Member Member ...}     members are constructors AND record selectors
  BareName                        a value export carries no braces
<first non-two-space-indented line ends the block>
```

**No obligation relaxed.** The join still owes exact identity matching; no
substring test is introduced. `Foo` cannot match inside `FooBar`, and the
control proves it on real bytes rather than on a fixture.

**Can-fail control, on real dumps, 20 rows, all passing.** Positives for exact
type names, constructors and record selectors; negatives refusing strict
prefixes, strict suffixes, both raw-line forms, and cross-module leakage; plus
the F-A defect itself re-observed. **The control is proven able to fail two
ways**: a normalizer that stops splitting braces fails five positive rows; one
that leaks the raw line fails the raw-line guard. Evidence:
`evidence/s30-0/D4-IDENTITY-CONTROL.txt`.

**Scope of the amendment.** Only the D-4 identity comparison changes, and only
in *what stream it reads*. The leg's exit taxonomy, refusal identities,
counters, uniqueness, exact-success REQ records and every other row are
untouched. The r9 leg bytes stay frozen; the amendment is applied in the
ticket gate that invokes it, so the demonstrated leg is not silently edited.

## 4. F-C — exactly-one is conditional on `-O0`; the choice and its reason

Every candidate in S30-0a/0b resolved under
`dist-newstyle/build/x86_64-linux/ghc-9.8.4/kelgroups-0.1.0.0/`**`noopt`**`/build/…`.
The `noopt` segment exists because `just build` pins `-O0`. A tree also built at
another optimization level would hold a second `.hi` for the same module under a
sibling path and the selector would return **2**.

**Chosen: pin `-O0`.** Reason: `just build` already pins it and CI runs
`just ci`, so the gate inherits the project's own invariant instead of
hard-coding haskell.nix's directory layout into a path pattern, which would be
the more brittle of the two and would silently rot when the layout changes.
**Rejected: component-qualifying the path**, for that reason.

**Backstop retained.** The leg's `exactly-one-or-REFUSE` rule stays. If an
`-O2` artifact ever contaminates the tree, the selector returns 2 and the gate
**refuses** — it never silently picks. So the limit is enforced, not annotated.

**The limit is real and recorded:** E3/E4 are true *for a tree built at a single
optimization level*. That is a bound on the claim, not a footnote.

## 5. F-B — returned as a design finding; the independence question is owed on the candidate

The real dump carries, before the export list: `interface hash`, `ABI hash`,
`export-list hash`, `orphan hash`, `flag hash`, `opt_hash`, `hpc_hash`,
`plugin_hash`, `src_hash`.

Consequences, stated and **not engineered around**:

- A whole-dump pin moves on flag, optimization-level and dependency changes
  that are **not** the module's interface.
- `src_hash` is present in the dump. If it hashes the module source, then any
  source edit — **including an unexported-only one** — moves the dump, so the
  source and metadata channels are **not independent by construction** under
  whole-dump pinning. That is a concrete mechanism for the mandate's
  correction.

**No normalization has been applied.** Load-bearing data is not stripped to
obtain an expected signature, and the whole-dump hash is retained as recorded
evidence.

**Therefore the gate does not assert independence. It observes it**, as a
**can-fail observation owed on the actual candidate** (mandate §4):

> **OWED-1.** On the candidate, apply an **unexported-only** edit to one Vote
> module, rebuild, and record **both**: (a) whether the derived identity view
> changes — expected **NO**; (b) whether the whole-dump hash changes —
> **UNKNOWN, to be observed**. Both outcomes are reportable results. If (b)
> changes while (a) does not, the channels are **not** independent under
> whole-dump pinning, and that is the finding, reported rather than hidden.

This makes the independence question **empirically decidable on the candidate**
instead of asserted from a field name. It is owed before GREEN/acceptance and is
not waived by the ordering.

## 6. Gate rows

| row | what it proves | when |
|---|---|---|
| G-1 | `git diff --check` clean; tracked-clean both ends | every run |
| G-2 | `nix develop .#ci --quiet -c just ci` exits 0 (build, test, format, lint, cabal-fmt, lean, client) | every run |
| G-3 | D-1 input binding on the **real** Vote extent: position, file-sets, clean samples, frozen-oid byte hashes | from S30-1 |
| G-4 | D-3 `.hi` selection **exactly one** per frozen module, freshness against the pre-build marker, emission exit 0, non-empty | from S30-1 |
| G-5 | D-4 join over the **derived identity view** (§3): exact identity per mapping row, exact-count, row-uniqueness, exact-success REQ records | from S30-1 |
| G-6 | **OWED-1** channel-independence observation (§5) | before GREEN/acceptance |
| G-7 | the 26 REQ IDs registered and executed with exact-success records | behavioural slices |

G-1/G-2 are runnable now; G-3–G-5 are candidate-dependent by construction and
become runnable when the Vote extent exists — which is precisely why the frozen
Vote extent was **not** run against the base, where its `1-fileset-hs` RED would
have been a foregone conclusion rather than evidence.

## 7. Cost discipline

One gate run = **one substantive whole-project operation**, because G-2 invokes
`just ci`. The commit owner is told this explicitly and is told to run the gate
deliberately, not iteratively. Ledger after S30-0: **2 of 28 substantive, 0 of
22 targeted.**
