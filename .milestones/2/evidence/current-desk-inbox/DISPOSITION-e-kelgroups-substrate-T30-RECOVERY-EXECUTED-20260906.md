# T30 copied-fixture recovery: executed, GREEN, and what it does and does not mean

Epic owner `%532`, kelgroups `#29` / Reactivegas `#73`. Local file only. This is the executed disposition
NOTE-005 authorized. No new grant is taken or implied.

## 1. Result

The exceptional fifth invocation ran **once**, under my freeze
(`inbox/NOTE-004-freeze-receipt-invocation-5.md`, sha `c1355b3c…`):

```
runner-exit=0
===== SUITE: PASS (baseline=GREEN setup-failures=0 mispredicts=0) =====
```

Counter actual: **exceptional fifth SPENT. Aggregate 5 of 5. No retry reserve remained and none was used.
Product builds 0.** Child disposition `T30-PF8R2-INV5-DISPOSITION.md` sha `d3528a7bf55a6a98…`.

## 2. The load-bearing check, done by me

**The gate leg is byte-identical to the one that failed twice** — `69c529ca22e1a798…`, `cmp`-verified
against `scratch/pf8r/T30-DRIFT-LEG-r9.sh` both before the freeze and after the run. Only the runner
changed, and only in the two authorized deltas. That is what makes this green mean "the fixture was
repaired" rather than "the checks were relaxed", and it is checkable rather than asserted.

Also verified by me directly, not relayed:

- `RUN.log`: **32 AS-PREDICTED, 0 MISPREDICT, 0 SETUP-FAILED**; `BASELINE: GREEN (required GREEN before any
  further case runs)`.
- **A1**: exit 0, zero `DRIFT-FAIL`/`DRIFT-REFUSE`, `FINAL: PASS (traversed=4 frozen=4 leg4pass=3)`.
- **Every deliberate negative control still fired with its intended identity** — the specific risk in
  repairing a fixture is silencing the controls, so this is the row that matters:

| control | exit | observed |
|---|---|---|
| A23a marker removed | 3 | `BUILD_MARKER absent` — FIX-7's guard did **not** swallow it |
| A10 stale interface | 1 | `stale inheritance refused` |
| A27 inherited dump | 1 | `4-provenance: dump for KelGroups.Vote.Types was not emitted by this run`; forbidden `4-type Verdict exact` count **0** |
| A26 dirty tree | 1 | `1-clean-hs: uncommitted bytes in kelgroups tree` |
| A28 overlay base | 3 | `0-overlay-base: export base … (unfounded overlay)` |
| A24 exact vs substring | 1 | `4-type Foo exact` **PASS** *and* `expected exact line [Fo] ABSENT` **FAIL** — genuine discrimination, both directions |

## 3. Newly established — synthetic plumbing only

Count integrity, row uniqueness and the two together (A2/A5/A6); exact-line vs substring (A24); per-REQ
exact-success discrimination (A15); stale-product RED with an intact control (A10); no-inheritance (A11/A27);
baseline GREEN itself (A1). Together with the instrument properties already established in invocation 2
(FIX-1 both directions, FIX-4, FIX-5 precedence, FIX-6 refusal, A21 overlay GREEN, A28, A7), the drift leg's
**synthetic layer is now demonstrated** rather than argued.

## 4. Unestablished — and the first is structural, which changes the P2 argument

- **M22b, channel independence between the source/byte-hash tripwire and the `.hi` tripwire, is unreachable
  in this harness by construction.** In live mode the hash tripwires read through the frozen oid, so a source
  edit cannot make them fire — A17 and A26 both show working-tree edits leaving `1-hash`/`1-hash-hs` green.
  They are falsifiable only in overlay mode, and overlay skips D-3 entirely. **The two channels are never both
  live in the same mode**, so no synthetic fixture can close this. It is a property of the design, not a
  defect any fixture introduced.
- **The entire compiler layer.** Every metadata path runs through a stub `ghc` serving fixture bytes. Nothing
  here says anything about real `--show-iface` output, real `.hi` discovery in a real `dist-newstyle`, or
  hash-pin stability across real rebuilds.

## 5. Exact real-compiler/product prerequisites — a distinct layer with its own cost

Carrying your correction rather than repeating my error: compilation establishes the real interface boundary
and **does not** repair or establish this harness; the two layers are separate and are costed separately.
This slice paid the harness layer. The compiler layer is still entirely unpaid:

- **P1 — B3, one owner build.** Real `.hi` selection uniqueness in a real `dist-newstyle`, hash-pin stability
  across rebuilds, freshness discipline against real timestamps.
- **P2 — B22a + B22b, two owner builds.** Now sharpened by §4 from *the cheapest way* to **the only way**:
  M22b and the `.hi` tripwire's can-fail cannot be reached by any synthetic fixture in this design.

**3 product builds, inside the UNGRANTED owner budget, none payable in this fence.**

## 6. Preservation and fences

Predecessor evidence re-hashed against values I recorded **before** any of this session's child work: pf8
`SUITE.log` `0f2ef27d…`, pf8 `A1/stdout` `543b1e8b…`, `pf8/run.sh` `62025c17…`, leg-r8 `f0afa32b…` — all
unchanged. The invocation-2 failure record is intact and was **not** overwritten by the success:
`scratch/pf8r/ev/SUITE.log` still reads `FAIL (baseline=BROKEN … mispredicts=11)`. `/code/kelgroups` at
`933e385d`, porcelain empty; `/code/reactivegas` at `3590c001` with the pre-existing untracked `sessioni`.

TAXONOMY-v2 remains bound to the synthetic experiment only; the production contract still requires its
separate versioned reconciliation and contract §8 stays unamended. Owner 26/24 and auditor 25/24 remain
**PROPOSALS**; `#30` implementation and audit remain **UNGRANTED**. `#33`/`#34` filed-blocked. No merge,
comment, publication, release or acceptance follows.

## 7. State

Ticket owner `%572` is COMPLETE and idle — nothing running, no children, no dispatch. The `#30`
contract/cost proposal can now be assessed against demonstrated plumbing plus explicitly missing
real-compiler evidence. Awaiting the desk on the commissioning decision; no further invocation is authorized,
requested, or needed at the synthetic layer.
