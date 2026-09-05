# NOTE-031 — #57 is accepted. Dispatch the composition. This is the operator's
# live goal and its last blocker just cleared.

`TICKET-ACCEPTED issue=57 commit=13b44bc`, mutation campaign closed 10/10
killed, 0 residual, 0 open. **The vote machine is accepted.**

That was the single precondition for #54 slice 2, and it has blocked the
operator's goal — *voting in the simulator as a Lean derivation of the two-machine
composition* — since 08:00 this morning.

**Dispatch it when you finish #57's wrap-up.** Do not let it wait for a
convenient boundary; it is the boundary.

## Nothing is left to design

Settled and on the record already:

- **Total three-way route** over all 18 constructors — 12 direct, 3
  base-enacted (`electResponsabile`, `removeResponsabile`, `removeMember`),
  3 app-decided (`grantPermission`, `denyPermission`, `backdonate`).
- **`enact_implies_threshold_met` confined to the base-enacted arm**, not
  stretched over app decisions the faithful machine cannot represent.
- **`Verdict` matched exhaustively**, no wildcard, with a build control that an
  unhandled fourth constructor fails elaboration.
- **`voteDerived` total over `Event`**, so #48's `donate`/`backdonate`/
  `removeMember` must be classified or the build breaks — the omission hazard
  is structural, not vigilance.
- Composition module on the **Reactivegas** side; `lean/KelGroups/**` still
  imports nothing from `Reactivegas.*`, enforced by `R-2` with the `R-3`
  legal-direction control.
- The join binds **production** evidence — `ClosureRecord.verdict` and
  `applyEventDetailed(...).enactment` — never an abstract existential, with a
  control proving the theorem fails if production enactment recording is
  removed.

## Seats

New lane, so the capacity policy applies cleanly: **ticket owner `codex`,
commit owner `glm` first.** GLM has just earned that on #57 — it wrote the RED,
the structural GREEN, challenged the gate rather than editing it, repaired the
finding, and closed a 10/10 campaign.

## Then, and only then

The simulator flips the join from `NON PROVATO` to a citation. That lane is
ready and waiting; it has been derivation-complete on both machines separately
for hours.
