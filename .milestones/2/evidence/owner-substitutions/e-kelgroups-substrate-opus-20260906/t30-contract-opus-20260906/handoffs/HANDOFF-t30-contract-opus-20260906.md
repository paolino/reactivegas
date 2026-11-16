# HANDOFF — t30-contract-opus-20260906 → epic owner `%532`

Seat: ticket preparation owner, kelgroups #30 CONTRACT PREPARATION ONLY.
Pane `%572`, `claude --dangerously-skip-permissions --model claude-opus-5[1m]
--effort high` (verified from `ps`, not copied). Brief sha256
`06e93291b31f6c93a0347e87ff30599ed6c3ead5cca421b9618746f4a24e0d41`.
Bases re-verified: kelgroups `933e385df2f2a251bb54a08bb7663f0d41fafb64`
(porcelain empty), reactivegas `3590c0015b84fd58004bf6fb44dd18b107304c48`.

## 1. Outcome

The bounded synthetic fixture campaign is **finished as far as this seat may
take it**: the inv1 evidence is consumed independently, the single remaining
repair rerun is **designed, written, and filed for your preflight binding**,
and it has NOT been launched — launch authority is yours.

| deliverable | sha256 |
|---|---|
| `handoffs/T30-INV1-ASSESSMENT.md` | `a665e76574c23a01cf4c519283dca559583946765f831703272b875013187c01` |
| `handoffs/T30-PF8R-REPAIR-PACKET.md` | `299a201e2b2b372978aa87cd2d11fb8cd3860c92db05b8ae27af7efcd21ae74b` |
| `scratch/pf8r/run.sh` | `524f355dd7ca0106d862768042901a4f083ba604e5f078ea54858874a8c4f611` |
| `scratch/pf8r/T30-DRIFT-LEG-r9.sh` | `69c529ca22e1a798e1ffb1810902243d9f3c2a9223da50d1eef66b2a39833a25` |
| `evidence/REPLAY-VERDICTS.txt` | `8eb3e116d57c7ccda87dddf3cd3467491cc197309379622ddd1b6bbdde7f4518` |
| `evidence/replay-verdicts.sh` | `3fb792ef1f6fc097c50d948e5f226f5e266985fca103d3cefa433e70f429f85c` |
| `evidence/PRESERVATION-BASELINE.sha256` | `2cb34c81b59afc1fa2dbe5e804b197706c55946556a7ec6af1ad589d1107943b` |

Decision on the rerun: **run it** (packet §1). Fixing four small defects
unblocks eight required mechanisms that currently cannot execute at all.

## 2. Mechanism status after inv1 (assessment §3 has the evidence)

**ESTABLISHED at line level (13):** empty-mapping vacuity refusal; comments-only
ERE parse; missing/unreadable-input refusals (6 fixtures); duplicate-artifact
refusal; no-producer-evidence refusal; unknown-MODE refusal; empty-dump RED;
leg-4 zero-success RED; dirty-tree clean-sample RED (controlled by A1);
committed-change position RED with frozen-oid content reads unaffected
(controlled); full-oid exactness; overlay lean hash trigger (controlled by
A21); overlay hs source-hash channel (controlled by A21).

**REFUTED (4):** baseline GREEN; no-inheritance of a pre-seeded dump;
TAXONOMY-v1 as bound (12 cases exited REFUSAL carrying rendered verdicts);
the leg's own header executes as shell.

**UNESTABLISHED (9):** count integrity; exact-line vs substring; row
uniqueness; stale-product RED (its control was destroyed by the same fixture
defect); per-REQ exact-success discrimination; overlay GREEN path;
`0-overlay-base` refusal — never falsified in any fixture; source-hash vs
`.hi` tripwire **independence**, unestablished in every mode as run; and the
entire compiler layer.

Five findings go beyond the leads I was given: F-3 every RED in the suite is
over-determined by one fixture defect, so even the fourteen as-predicted rows
overstate attribution; F-4 the join reads a dump bound to nothing and only
incidental ordering prevents a false GREEN; F-5 `0-overlay-base` has no
negative control; F-6 `4-livedir` is a restatement of `OVERALL_FAIL`, not the
live-minus-mapping reconciliation NOTE-007(b)/NOTE-008 asked for — direction B
is in fact carried by the byte layer, which should be recorded as such with
`enforced: NONE` on the report line; F-7 the harness's own per-case output was
never written to disk.

On the counter you asked about: `setup-failures=0` is accurate to its own
definition and is not itself a lie. The defect is that nothing asserted the
fixture it built was sound and nothing made the baseline decide the suite — a
zero on a counter that cannot observe the failure class in play. The repair
adds both (template invariants + BASELINE gate).

## 3. The exact next compiler/product prerequisite, and its cost

**Even a fully green pf8r establishes nothing about a compiler.** Every
metadata path in the campaign runs through the `ghc` shim at
`scratch/pf8/run.sh:144-152`, which serves fixture bytes. The next
prerequisite is therefore not another synthetic rerun; it is the first real
build, and it is already itemized in the frozen r8 map:

**P1 — B3, one owner BUILD.** `nix develop .#ci --quiet -c just build`, plus
pre-build marker touch, build-receipt capture, per-frozen-module
`ghc --show-iface <hi>` emission and hash-pin. This is what establishes, and
nothing cheaper can: (a) that the `find … -name '<Mod>.hi' -path '*<rel>*'`
selector resolves **exactly one** candidate in a real `dist-newstyle` (the
exactly-one-or-REFUSE rule is currently demonstrated only against a
hand-built two-file fixture); (b) that real `--show-iface` output is stable
enough to hash-pin across rebuilds; (c) that the marker/receipt freshness
discipline survives a real build's timestamps.

**P2 — B22a + B22b, two owner BUILDs.** Baseline GREEN plus overlay-edit
build, emission and diff-fire. These are the **only** way the `.hi` tripwire's
can-fail is ever demonstrated, because overlay mode skips D-3 by design and no
live-mode synthetic case can make a real interface drift. M22b (channel
independence) closes here too, and nowhere else.

**Cost: 3 product builds (B3 + B22a + B22b), all inside the UNGRANTED owner
budget.** None of them is payable inside the T30 preparation fence, where
product-build spend is 0 and stays 0. So the prerequisite is an authorization
decision, not a technical one: either commission #30 implementation under the
proposed ceilings, or issue a narrow measurement grant for B3 alone if you
want the selector/hash-pin question answered before commissioning.

Owner 26/24 and auditor 25/24 remain **PROPOSALS**. #30 implementation and
audit remain **UNGRANTED**. No part of this handback changes that.

## 4. Settled rows, restated per the brief (none reopened)

- Composition/lifecycle authority: settled; untouched here.
- `#68` V-2: settled-but-unlanded. Rebind boundary, never anticipation. No
  change binds until the upstream landing is accepted.
- Theta: no shipped default. The threshold is a parameter; exhibits are not
  defaults. Nothing in this work infers one.
- Unruled `notProposer` / `notDesignee` producing semantics: UNSCHEDULED.
  Preserved boundary, no promise, no dependency edge.
- `#76` / `#81` lifecycle and economic content: **neither** implemented ahead
  of Lean **nor** marked permanently out of the eventual substrate contract.
  Both statements hold simultaneously and are recorded as such.
- No Reactivegas economic implementation in kelgroups. No vendoring.

## 5. Residual scope and assurance limits, stated

- pf8r has **not** been executed. §4 of the packet is predictions, not results.
- A green pf8r means exercised shell/git plumbing on synthetic fixtures under a
  stub. Never compiler compatibility, compiler discovery, semantic coverage,
  mapping completeness, or product readiness.
- TAXONOMY-v2 is a **proposed** amendment requiring your re-freeze; contract §8
  and the command-map taxonomy block are NOT edited here. The `TAXONOMY_V2=0`
  fallback and its prediction column are in the packet, so declining costs
  nothing.
- The r8 leg keeps `enforced: NONE` for automatic item-level attribution and
  for direction-B reconciliation as a report line (F-6). Neither is closed by
  this repair.
- Evidence preservation verified: predecessor `scratch/pf8` and `handoffs`
  re-hashed after all work, byte-identical to the pre-work baseline.

## 6. Exact next authorized action

Await your preflight binding receipt in `inbox/` naming the two sha256 values
in packet §0 and the exact command in packet §5 (with or without
`TAXONOMY_V2=0`). On receipt: execute it once, preserve complete raw streams,
exits and identities, journal the actual counter, and report. Nothing else is
launched from this seat.
