# A — Q-001: none of A, B or C. The two channels serve DIFFERENT events, so
# there is no join to prove.

For epic #43, to route to #54's ticket owner. The question is excellent and the
type mismatch is real. The framing it inherited is wrong, and the framing is
mine — I accepted "the join binds BOTH channels" without checking that any
single event needs both.

## The fact that decides it

The faithful machine's proposals are exactly:

```
Proposal = introduceMember | removeMember | changeRoles
```

Grepping `KelGroups/Event.lean` for purchase, permission, grant, collection
returns **zero**.

**So `grantPermission` cannot come from the faithful machine.** It has no
proposal that could produce one. Asking `applyEventDetailed(...).enactment`
plus `enact_implies_threshold_met` to witness a purchase approval was asking a
machine about a decision it cannot represent. That is why no term of one
channel is derivable from the other: they are not two views of one decision,
they are two *different* decisions.

## The ruling — option D: classify, do not join

The composition classifier is **three-way and total over `Event`**, not
two-way:

- **direct** — not vote-derived at all (e.g. `addUser`, per V-6: no vote to
  admit a member);
- **base-enacted** — witnessed by channel 2, the faithful machine:
  `electResponsabile`, `removeResponsabile`, `removeMember`, which correspond
  to `changeRoles` / `removeMember` proposals that machine really has;
- **app-decided** — witnessed by channel 1, the required vote machine's
  `ClosureRecord.verdict`: `grantPermission`, `denyPermission`, and later
  `backdonate`.

Each vote-derived constructor names **which** producer witnesses it. Both
channels are bound by the theorem; **no event is required to bind both**, so
there is no correspondence to hypothesise, no identifier to share, and no
channel dropped.

- Option A's premise is unnecessary — it would be assuming a relation between
  decisions that are not the same decision.
- Option B's coupling is unnecessary, and gate leg 3's separation stands
  untouched. Do not weaken it.
- Option C's loss does not occur; both channels appear, on their own events.

## The bonus, and it is worth stating in the matrix

**Every constructor classified `app-decided` is an event today's kelgroups
cannot produce at all.** That set *is* the `kelgroups#30` requirement, derived
mechanically from Lean rather than asserted in prose — and #47 is the document
that must freeze it. When a new economic decision is added and classified
`app-decided`, the substrate requirement grows automatically and visibly.

## Conditions

1. The classification is **total over `Event`** and over `Verdict`, no
   wildcards. Unclassified constructor ⇒ real build fails; that control stands.
2. A constructor classified `base-enacted` must correspond to a proposal the
   faithful machine **actually has**. If a future event has no such proposal,
   it is `app-decided` — it may not be quietly routed to a machine that cannot
   express it. **That is the exact mistake this answer corrects.**
3. `enact_implies_threshold_met` stays bound, on the base-enacted arm where it
   is true, and is not stretched to cover decisions it does not concern.

Slice A is unaffected and continues. The composition gate may be frozen on this
basis. Report the classification table before dispatch — I want to see which
constructor lands in which arm.
