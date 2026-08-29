# Plan — #57 structural vote validation

Artifact ceiling: 130 lines / 10 KiB.

## Constraints

The rejected #54 candidate already validation-couples its event effect, but a
rejected event still reaches `sweepClosures`, and three franchise-changing
events are unconditionally admitted. The re-cut changes the boundary, not one
more constructor branch.

The complete change is one bisect-safe OWNER slice. Its acceptance surface is
semantic (Lean architecture and proof statements), so it requires a strong
commit owner and a fresh non-GLM auditor. `draft=NONE`.

## Technical strategy

1. Make the production step's validation result dominate both its effect and
   its sweep, establishing exact rejection identity without a state invariant.
2. Make signer authorization apply to the whole event surface. Preserve only
   the empty-franchise admission capability required for a production trace to
   seed the first responsabile; do not broaden the admission contract.
3. Generalize the public R-45 theorem to arbitrary pre-state and add the
   universal non-responsabile corollary for a nonempty franchise.
4. Replace the no-expiry constructor whitelist with a semantic preservation
   premise that includes preserving member events.
5. Keep the five inherited invariant rows under fresh negative controls and
   bind every theorem to printed axiom evidence.

## Slice S57-A — structural validation

Delivers tasks T5710–T5715 and requirements R57-01…R57-10.

Production/proof fence:

- `lean/KelGroups/Vote/Validate.lean`
- `lean/KelGroups/Vote/Fold.lean`
- `lean/KelGroups/Vote/Invariants.lean`
- `lean/KelGroups/Vote/Tests.lean`

`lean/KelGroups.lean` needs no adjustment: every current Vote module is already
rooted. Event, state, and type declarations are sufficient as written. A
placement or signature obstruction versions this plan before work continues.

## Ordered evidence

1. Baseline: clean `c433ff76`; full repository CI; Lean 4.25.0 identity.
2. RED: frozen #57 gate fails on the retained R-45 oracle.
3. Can-fail controls: each gate leg fails for its named reason, including a
   new event constructor with an effect but no authorization arm, and a seeded
   bypass around the production boundary.
4. GREEN: focused Vote build, frozen gate, full ticket gate, and repository CI.
5. Fresh audit: complete invariant matrix, five inherited rows freshly
   re-demonstrated, no-expiry coverage witness, scope/tree/axiom receipts.
6. Acceptance: task-only stamp, exact audited-tree proof, final quiet receipts.

## Risk ledger

| Risk | Permanent control |
|---|---|
| rejection still runs sweep | arbitrary-state complete identity theorem plus deliberately sweep-ready counterexample |
| next constructor bypasses authorization | no wildcard/side registry and seeded constructor-plus-effect compile failure at the boundary |
| bootstrap becomes impossible | production trace from `emptyVoteState` must still seed a responsabile before all R-45 traces |
| member-event authorization repairs only `removeMember` | explicit no-op oracles for `admitMember`, `removeMember`, and `setRoles`, backed by the universal theorem |
| no-expiry stays vacuous for member events | retained preserving non-admin admission must satisfy the semantic premise |
| old green is mistaken for new evidence | re-versioned #57 gate and fresh executions of all five inherited rows |
| proof statements remain narrower than their names | auditor reads statement signatures; gate prints axiom sets and runs definition mutants |

## Live-boundary analogue

This is a Lean model, so the live seam is production-fold reachability. Every
behavioral oracle uses `applyVoteEvent` or `foldVote`; no record-literal-only
proof can satisfy the slice.

