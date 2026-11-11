# NOTE-016 — grant/deny are unproven. #47 must specify the composition
# requirement. ACTIONABLE — #47 is in audit, this is its gap.

Operator's ruling: a **second Lean machine** for kelgroups (questions, votes,
majority) plus a **composition theorem** — the economic machine only ever
consumes `grantPermission`/`denyPermission` emitted by the vote machine. Their
words: *"it's what kelgroups-mapping.md should have specified."*

## The hole, verified at 54c1543

`grantPermission a c` demands **only** `isResponsabile s a`. Any single
responsabile can set `permitted := true` unilaterally. `denyPermission` is the
same — one responsabile can destroy any collection and refund every pledge.

`permissionToClose col := col.permitted ∧ col.pending = []` asserts the *flag*.
`close_permission_to_close` proves closure implies the flag. **Nothing proves
the flag reflects a majority.** L2's user story in `state-machine.md` says "the
group must grant closure permission"; the machine requires one person.

## What is #47's, and it is #47's alone

The Lean work is a separate ticket, sized like #45. **But the requirement is a
design-record statement and belongs in #47**, which is the document
`kelgroups#28`/`#30` freeze against:

- `grantPermission` and `denyPermission` must be **provably vote-derived**, not
  merely responsabile-authored;
- what `#30` must expose for that derivation to be checkable — the enacted
  verdict's identity, its question, and its provenance;
- that this now has **two** consumers, not one: purchase approval, and the
  voted comune backdonation ruled today. A vote machine serving one consumer is
  a special case; serving two is an interface.

An unnamed requirement is one the substrate will not build. That is
`COMMON.md`'s own stated failure mode, and this is the largest instance of it
in the document.

## The caveat that must appear in the text, not just in my ledger

A composition theorem over two Lean machines proves a property of **the
composition of two models**. It becomes end-to-end only if kelgroups'
implementation mirrors the vote machine, the way child 3 is required to mirror
the economic theorems. kelgroups is a different repo, so that is a cross-repo
obligation — **I am registering and escalating it; you do not.**

Until it exists, the honest status is `enforced: PROVED-IN-MODEL`, not
`enforced`. #47 must not claim more.

## Also: `state-machine.md` is over-claiming, and this is the fourth instance

After the nonexistent `removeUser` (line 76), L5's inverted deposit direction,
and now L2's "the group must grant". The user stories systematically assert
more than the theorems prove, and they are the half humans read. That is no
longer three separate defects — it is a property of the document. `#47` may not
fix it (`COMMON.md` forbids), so fold it into `#48`'s slice 3 and tell me you
have.

Report where the #47 requirement landed: this candidate, a follow-up
submission, or a follow-up ticket. Not unowned.
