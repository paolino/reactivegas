# Which prefix acceptance receipts actually exist — and the nine that have none

Answers the outstanding half of NOTE-057: *"identify which prefix acceptance
receipts are actually available, and ensure final audit/CI judges the resulting
complete candidate rather than treating patch-ID matching as acceptance of the
prefix."*

A PR on this branch lands **30 commits**, not one. This inventory says which of
them anything ever accepted.

## The headline

**Nine commits between the last acceptance receipt and the C1 slice carry no
acceptance, and the fresh audit does not reach them.**

The audit in flight is scoped `base=dbd1ed859c6eb9510fedc1139c20916db83db572 ..
candidate=af9c1e5091014702c88df89e4b591819aad57979`. That is the C1 slice alone.
Everything below `dbd1ed8` is outside it.

## Acceptance receipts that do exist

Every receipt in the lane journal names a **pre-rebase SHA**, all of which are
now orphaned. Mapped to current identities by patch-id:

| journal receipt | old SHA | current SHA | what it accepted |
|---|---|---|---|
| `SLICE-ACCEPTED` 2026-08-26T16:26:45Z | `88d9a7b` | `1a38ae682` | round 1, interactive simulator |
| `SLICE-ACCEPTED` 2026-08-26T17:37:59Z | `2325262` | `1d8800219` | round 2, direct manipulation |
| `SLICE-ACCEPTED` 2026-08-26T20:15:49Z | `4db3191` | `1eb02e5f3` | governance gap |
| `SLICE-ACCEPTED` 2026-08-27T08:54:42Z | `79d96fc` | `9c447da6e` | round 3, task hats |
| `SLICE-ACCEPTED` 2026-08-29T07:43:35Z | `c520171` | *(dropped — `chore: ignore ticket gate`, untracked gate file)* | provable-only manifest |
| `SLICE-ACCEPTED` 2026-08-29T08:02:21Z | `a0dee83` | `4184b8ef8` | trace provenance |
| `SLICE-ACCEPTED` 2026-08-29T09:30:27Z | `d44d353` | `66b3efc86` | both machines |
| `AUDIT-RESULT` pass + `SLICE-ACCEPTED` 2026-08-29T18:33:02Z | `a9c9462` | `993278ec4` | batch 1 |
| `AUDIT-RESULT` pass + `SLICE-ACCEPTED` 2026-08-30T08:52:50Z | `7923e58` | `af3d25a4a` | strips |
| `GATE-PASS` 2026-08-30T10:47:10Z | `b32ae15` | `9c8ff2418` | machine fidelity |
| `AUDIT-RESULT` pass + `GATE-PASS` 2026-08-30T12:18:46Z | `5e3ebaa` | **`125409b53`** | fidelity recut v2 — **the last acceptance on this branch** |

## The nine with no acceptance

Everything after `125409b53` and before the C1 slice:

| # | current SHA | subject | status |
|---|---|---|---|
| 1 | `4a90e36` | clarify purchase geometry and proof links | **`AUDIT-CLOSED … published=yes accepted=no`** (2026-08-30T17:39:33Z) — explicitly not accepted; its GLM audit was ruled advisory and no eligible re-audit was ever dispatched |
| 2 | `c798bd6` | keep clustered purchases from overlapping | operator-directed, never audited |
| 3 | `efd7b8a` | park member chips outside the purchase ring | operator-directed, never audited |
| 4 | `de89511` | drag purchases around the ring | operator-directed, never audited |
| 5 | `8bc6bbb` | show amounts in euro | operator-directed, never audited |
| 6 | `bffcad6` | drop canale base from the feed | operator-directed, never audited |
| 7 | `d446280` | name the decision, not the proposal id | operator-directed, never audited |
| 8 | `41e5317` | usernames instead of letter badges | operator-directed, never audited |
| 9 | `dbd1ed8` | stack conto and cassa vertically | operator-directed, never audited |

Rows 2–9 are the batch that landed unjournaled on 2026-08-30 between 18:56 and
19:32, which NOTE-048 was written about and which the lane recorded
retroactively. They were journaled. They were never *accepted*.

Row 1 is worse in one respect and better in another: it was published to the
preview under operator order, and the journal is explicit that publication did
not rest on an acceptance — `accepted=no`.

## What currently judges them, and what does not

| instrument | reaches the nine? |
|---|---|
| fresh independent audit (`%517`, scope `dbd1ed8..af9c1e5`) | **no** |
| frozen gate v12 on `af9c1e5` | **partly** — it runs against the whole working tree, so their *surviving effect* is exercised by the oracle, the corpora and every sub-gate; it does not review them as changes |
| full repository CI on `af9c1e5` (pending, NOTE-055) | **partly**, same sense |
| the patch-id / byte map | **no** — it says nothing about acceptance and must not be read as covering them |

So the honest position: the **resulting tree** at `af9c1e5` is heavily verified,
and the C1 slice is independently audited. The nine commits are verified only
through their surviving effect on that tree; none was reviewed as a change, and
one is on record as explicitly not accepted.

## What I am not doing about it

- not re-running old audits for a SHA rewrite — NOTE-056 forbids it and it would
  answer the wrong question anyway;
- not widening the live audit's scope underneath it — NOTE-055 forbids amending
  beneath a running audit, and re-cutting its scope mid-flight would void it;
- not quietly letting a full-branch PR imply the prefix was accepted.

## The decision this needs from the desk

A PR on this branch lands all thirty commits. Three options, and the choice is
the desk's:

1. **Land as is, with the gap stated in the PR body.** Cheapest. The tree is
   well verified; the nine are operator-directed UI/geometry work whose effect
   the gates exercise.
2. **Widen the next audit's scope** to `125409b53..af9c1e5` so one independent
   pass covers every unaccepted commit. Costs a fresh dispatch and budget; the
   current audit finishes first either way.
3. **Accept the nine explicitly** on the record, on the strength of the gate and
   CI over the resulting tree, without a per-commit review.

I recommend **(1) plus an explicit line in the PR body**, escalating to (2) only
if the desk wants per-change review of operator-directed UI work. My reason: the
nine are visual/copy changes whose behaviour is fully exercised by the frozen
oracle and the teaching gate, and a retrospective per-commit audit of superseded
UI states buys less than it costs. But the gap must be *stated*, not inherited
silently — which is the whole point of raising it here rather than at merge.
