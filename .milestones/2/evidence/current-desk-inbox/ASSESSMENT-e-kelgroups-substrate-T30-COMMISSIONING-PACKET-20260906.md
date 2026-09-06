# `#30` commissioning packet: assembled, assessed, reviewable

Epic owner `%532`, kelgroups `#29` / Reactivegas `#73`. Local file only. **Nothing is granted here**, and
nothing was executed: no implementation, compiler execution, product build, audit or dispatch.

Packet: `t30-contract-opus-20260906/handoffs/T30-COMMISSIONING-PACKET.md`
sha256 `cbd749513f54323345910bcc2da58ffdfbc7a1076d6cb2e5a22cee3ab7cec9aa`, 304 lines, six required contents
present, assembled from the existing r8 work with no new planning registry.

## 1. My two corrections, carried rather than repeated

- **"Only way" is withdrawn.** What the executed result and source inspection establish is a limitation **of
  this harness as built** — live-mode hash channels read through the frozen oid so a working-tree edit cannot
  move them (A17/A26 stayed green), and overlay skips D-3 — not impossibility for every alternative
  instrument.
- **The one-build stability claim is withdrawn.** B3 alone establishes emission, selector resolution and
  freshness-against-marker at that moment. A stability claim needs a second independent rebuild, or a prior
  build explicitly identified and valid for the comparison — and this lane has **no prior product build at
  all**, so no such baseline exists today.

## 2. Feasibility — verified at source by me, not accepted from the packet

At `933e385d`: `git ls-files` matches **no path containing "vote" anywhere**; `lib/` holds exactly eleven
modules; `kelgroups.cabal` `exposed-modules` lists those eleven and no Vote. **The drift leg's frozen HS
extent `lib/KelGroups/Vote/{State,Types}.hs` is precisely the missing `#30` candidate declaration.**

I confirmed the two structural claims the packet builds on that:

- `KelGroups/Server/JSON.hs` is genuinely the repository's **only** depth-2 module, so it is the right
  structural analogue for the `KelGroups/Vote/Types.hi` selector question.
- `1-fileset-hs` is a **D-1 `fail`** (leg lines 195–196). So the frozen leg cannot produce a meaningful live
  run against the current base at all — it REDs in D-1, before D-3 is ever reached. Its first meaningful live
  run is against the first candidate. That is a real finding and it was not previously on the record.

| build | on `933e385d` today | on the `#30` extent |
|---|---|---|
| **B3** | **operable now** on `KelGroups.Event` + `KelGroups.Server.JSON` | **impossible** — `find` returns zero candidates for `KelGroups.Vote.Types`; the leg refuses `3-select: ZERO .hi candidates` |
| **B22a** (export + baseline GREEN) | **operable now**, needs no candidate | same; it is the §12(viii) pre-check, blockable and never skippable |
| **B22b** (overlay edit + build + diff-fire) | **operable now** on an existing module, edit applied to an **exported copy**, never the worktree | **impossible** for the Vote extent |

**Transfers** to the `#30` extent: toolchain facts — `--show-iface` is invocable in this build environment,
its output shape, that the selector resolves for a nested module in this project's real `dist-newstyle`, that
marker/receipt ordering survives a real build, that an interface edit moves the bytes. **Does not transfer:**
anything about Vote identities, the mapping rows, the join, the 26 REQ executions, or that the Vote extent's
own `.hi` will be unique — a newly declared module can appear in more than one component's build tree, and
only its own build answers that. Every `#30` acceptance row is **impossible before a candidate exists**.

## 3. Decomposition — compiler boundary at slice 0, scope whole

**S30-0 toolchain preflight** (B3-class + B22a pre-check on the existing module set) is payable **before any
candidate exists**. If the selector is ambiguous in this project's real `dist-newstyle`, or `--show-iface` is
not invocable as assumed, that is known at slice 0 rather than at demonstration time — which is exactly the
failure mode this lane has already paid for three times at the harness layer.
**S30-1** declares the extent (data declarations only) so D-1/D-3 run against the real subject.
**S30-2…n** are the r8 map's own behavioural rows in its own order, each bisect-safe.
**S30-final** is closure, replay, `Trivial`, full `just ci`.

Client (R30-12), integration, replay and closure are **slices in the sequence, not deferred past the compiler
boundary**; R30-9 (`#68`-gated rebind), R30-10U/R30-11, R30-13 and the R30-X fences stay in the mandate
unchanged. This reorders; it does not shrink `#30`.

## 4. Taxonomy reconciliation before freeze

Determined mechanically over the invocation-5 streams rather than argued: with a green baseline, exactly
**two of 31** cases reach a refusal after a rendered verdict (A10, A27) — the only situation where v1 and v2
differ. **Exactly one r8-map row changes: A10**, mapped RED(1), which v1 delivered as 3. No other promised
outcome moves. That is the whole cost of reconciling contract §8 to v2, stated before any freeze.

## 5. Budget — counted, not netted

S28-1 owner **34** builds, auditor 9/7, plus one zero-build invalid admission retained separately;
S28-R1 owner **13**, auditor 10/16, one submission; S28-R2 owner **26** substantive + 4 targeted + 2
diagnostic, auditor 11 + 22, one submission. Synthetic: pf1 **2** (both failed, never refunded), pf7 **0**
(written, never run), pf8 **1**, pf8r **1**, pf8r2 **1** = **5 of 5**, ceiling reached, no retry reserve used.
**Product builds: 0**, across every campaign in this lane.

**Ledger call resolved (the ticket owner correctly flagged it rather than picking).** The "granted 14/24" and
"actual 26" lines are **not** in conflict: the outgoing epic owner's handoff records the consumed grant chain
`S28-R2 14/24→26/24 + auditor 12/24`. 14/24 was the initial grant, **raised to 26/24** by a consumed desk
grant; actual 26 equals the raised ceiling exactly. The 14/24 line is superseded, not contradictory.

## 6. Ceilings — held where held, trimmed where defensible

Unit: one whole-project `cabal build`/`cabal test` invocation. **Owner builds 26 held**, justified
unit-by-unit (B1/B2 + B3 + B4 + B5–B19 fifteen + B20 + B21 + B22a/b + SLIM three): the synthetic PASS touches
none of those units — what it removed is the *risk* of spending owner builds diagnosing harness defects,
which is a risk reduction, not a unit reduction. **Owner probes 24 → 22**, dropping the two dispute probes
because the contract already routes an unresolved dispute to BLOCKED, so the reserve was never the resolution
path; the transient reserve stays, since an environmental re-run is a real campaign-death risk.
**Auditor 25/24 unchanged** — its obligations are untouched by a synthetic result, and trimming them to look
responsive to the green would be unjustified.

I accept this as answering the requirement: it neither inflates because the numbers were proposed before, nor
performs a reduction the evidence does not support.

## 7. Fences

`#33`/`#34` downstream and untouched. **No inferred `#73` closure** — `#29` and `#73` stay open. No comments,
no merge, no publication, no push, no PR, no product or source edits. Both ceilings remain **PROPOSALS**;
`#30` implementation and audit remain **UNGRANTED**; contract stays frozen at r8 with §8 unamended pending
the one-row reconciliation above. Ticket owner `%572` is COMPLETE and idle — nothing running, no children.

## 8. The decision now in front of the desk

Commission `#30` on this decomposition and envelope, or return a narrower one. If a smaller first commitment
is wanted, **S30-0 alone is the natural cut**: it is payable against the accepted base with no candidate, it
puts the real compiler boundary first, and it answers the selector and `--show-iface` questions before any
Vote code is written.
