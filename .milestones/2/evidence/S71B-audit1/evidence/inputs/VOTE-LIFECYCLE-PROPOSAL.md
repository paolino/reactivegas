# Vote lifecycle — bounded proposal, not an implementation

Epic owner `claude-opus-5[1m]`, for desk `%510`, per NOTE-005. Written
2026-09-05 against `e6c5924`.

**Nothing here is implemented and nothing is decided.** NOTE-005 asked which
rulings *actually require a refusal* versus merely a non-deciding ballot, and
warned against inferring a refusal from a dormant error constructor. That
distinction is the whole content of this document.

---

## The authority, quoted

From the operator's vote-machine rulings V-1…V-7:

> **V-5** — *proposer leaves or renounces ⇒ close*, running the **negative**
> continuation. **Escrow must refund; silent deletion strands money.**

> **per-designee permission** — legacy `Permesso promotore interrogato`, *a
> question one named person answers. Not a majority. Distinct mechanism.*

> **V-3** — recompute on every state change; a question can pass because
> responsabili left.

> **V-4** — one list per responsabile; legacy's guard is commented out and
> dissent-then-assent lands you in both lists, counted twice.

> **V-7** — no expiry for undecided questions; the same threshold closes a
> question negative, so dissent is always available.

---

## The trace: ruled requirement vs current behaviour

| # | behaviour | ruled? | current code | verdict |
|---|---|---|---|---|
| 1 | proposer **renounces** ⇒ question closes | **RULED, V-5** | `renounce` validates, then `effectedState` leaves the question open — a Slice-A no-op | **unfinished against a ruling** |
| 2 | that closure runs the **negative continuation** | **RULED, V-5** | no negative continuation exists on any closure route | **unfinished against a ruling** |
| 3 | escrow refunds on that closure | **RULED, V-5**, and it is the stated *reason* for #2 | no wire from any closure to an economic effect | **unfinished**; same missing composition as `#76` |
| 4 | proposer **departs** ⇒ question closes negative | **RULED, V-5** ("leaves") | `ClosureCause.proposerDeparted` is declared; `closureCause` returns only `.tally` or `.franchiseChange` | **unfinished against a ruling** |
| 5 | a **non-proposer's** renounce is **refused** | **NOT RULED** | accepted, no-op | **open question — see below** |
| 6 | a **non-designee's** ballot on a permission question is **refused** | **NOT RULED** | recorded, non-deciding | **consistent with the ruling as written** |
| 7 | V-3 recompute on every state change | RULED | `baseHook` runs `sweepClosures` against the post-transition franchise | implemented; corpus-covered by `integrated.json` |
| 8 | V-4 one list per responsabile | RULED | a switch replaces the prior ballot | implemented; covered by `VOTE_TRACES_V1` |
| 9 | V-7 no expiry | RULED | no timer anywhere; questions present in the corpora are `.open` | **structural evidence only — not shown implemented**; see below |

### Row 9 is weaker than it looks, and revision 1 of this table overstated it

The earlier table read `implemented` for V-7. It should not have. **Absence of a
timer, plus the questions visible in the corpora being `.open`, does not prove
that an undecided question is retained across every unrelated transition.**
Retention is a property quantified over all transitions; what has been observed
is a handful of states in two corpora and the absence of an expiry mechanism in
the source. Those are structural leads.

A transition that dropped an unrelated open question — a `sweepClosures` edge
case, a payload rebuild that loses an entry, a base hook that rewrites
`s.votes` — would satisfy both observations and violate V-7.

**The retention obligation is `#66` S5 and it is pending.** This row stays
labelled structural evidence until that lands. The earlier reassurance is
withdrawn and is not repeated anywhere in this document.

### Rows 5 and 6 are the ones NOTE-005 asked about, and they differ

**Row 6 — the permission arm is already faithful.** The ruling says the question
is one *a named person answers*, and that it is *not a majority*. `verdictOf`
implements exactly that: for `.permission designee`, only the designee's
presence in `assents`/`dissents` decides; every other responsabile's ballot is
recorded and decides nothing.

The ruling says who *answers*. **It does not say others are refused.** Recording
a non-deciding ballot is consistent with it. `VoteError.notDesignee` expresses
an intention to refuse instead — an intention with no ruling behind it. Per
NOTE-005 that intention is not authority, and this proposal does not treat it
as one.

*Open question for the operator, stated neutrally:* should a non-designee's
ballot on a permission question be **refused** rather than recorded-and-ignored?
Recording it is harmless to the verdict and leaves an audit trail of who tried;
refusing it is a cleaner statement that the question was never theirs. Both are
defensible; neither is ruled.

**Row 5 — genuinely unruled, and V-5 does not settle it.** V-5 names the
*trigger*: the **proposer** renouncing closes the question. It says nothing
about what happens when a non-proposer sends `renounce`. Three readings, all
compatible with V-5 as written:

1. refused (`notProposer`) — the reading `VoteError.notProposer` anticipates;
2. accepted and ignored — the current behaviour;
3. accepted as *that responsabile's* withdrawal from the tally, a different and
   arguably useful event, which V-5 neither grants nor forbids.

**This proposal does not choose.** It records that the dormant constructor is
evidence of an intention, not of a ruling, and that reading (1) off the
constructor would be exactly the inference NOTE-005 forbade.

---

## What the dormant constructors actually tell us

Lean's cross-reference index reports `usages: []` for
`VoteError.notDesignee` and `VoteError.notProposer`, and
`lean/KelGroups/Vote/Validate.lean:19` says why: *"declared here from Slice A so
Slice B …"*. The same shape holds for `ClosureCause.proposerDeparted` and
`ClosureCause.renounced`, which `closureCause` never returns.

**Limit of that instrument:** a usage index is a construction search, not a
reachability proof. It shows no source site builds those values. Read together
with the three fully visible arms of `validateVoteEvent`, it is a strong lead;
the actual evidence would be a corpus row, which is why `#75` records this as a
residual rather than asserting it.

So there are **four** declared-but-unproduced identities, and they divide:

| identity | backed by a ruling? |
|---|---|
| `ClosureCause.renounced` | **yes** — V-5 requires the closure |
| `ClosureCause.proposerDeparted` | **yes** — V-5 requires the closure |
| `VoteError.notProposer` | **no** — unruled (row 5) |
| `VoteError.notDesignee` | **no** — unruled (row 6) |

Two are unfinished work against a ruling. Two are unruled intentions. Treating
all four as one backlog would smuggle an unruled product choice in behind two
ruled ones — which is what revision 1 of the assenso packet did, and what this
document exists to prevent.

---

## Proposed shape, if the desk commissions it

Not a ticket body; the desk decides whether and where this lands.

**Part 1 — V-5 closure, ruled, implementable now.** `renounce` by the proposer,
and proposer departure through the base hook, both close the question and record
the closure with its cause. This needs no product decision.

**Part 2 — the negative continuation, ruled, blocked.** V-5's *reason* is that
escrow is held and silent deletion strands money. The refund is an economic
effect derived from a vote closure — **the same missing wire as `#76`**, in the
opposite direction: `#76` carries a closure into `grantPermission`; this carries
a closure into a refund. Whoever builds one should look at the other, and the
desk may want them in one ticket.

**Part 3 — rows 5 and 6, blocked on the operator.** Not scoped here.

**Acceptance, whatever the shape.** Every row an absence claim needs an
executable negative witness: a mutant that leaves the question open on a
proposer renounce, or closes it positive, or closes it without refunding, must
be shown *failing* before the row passes.

---

## Statement-completeness questions, routed not answered

Per NOTE-005 these go through `#66`/`#71` via the desk; recorded here so they
are not lost:

1. Does the design record state V-5's closure and its negative continuation? If
   it states them, the record currently describes behaviour the machine does not
   have — the over-claiming pattern already found four times in
   `state-machine.md`.
2. Does the record describe permission questions as refusing non-designee
   ballots? If so, it over-claims against row 6.
3. Are `proposerDeparted` and `renounced` cited anywhere as delivered?
