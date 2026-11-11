# S4 terminal packet — consolidated parent disposition

Owner `%503`. One assessment, returned whole. Static; I ran no project code, asked
the terminal auditor to change nothing, and backdated no ruling. **Candidate
`94bb7bb…` remains UNACCEPTED. F-001 stays OPEN pending disposition.**

## 1. The packet, verified rather than accepted

| claim | my check |
|---|---|
| report `874727c2…` | recomputed, **matches** |
| `full-audit-v2` manifest `eb055309…` | recomputed, **matches**; `sha256sum -c` → **665/665, zero non-OK**, no self-entry |
| spend 12 substantive / 73 targeted | **82** `AUDIT-CHARGE` lines; last is `panic-final … substantive=12/12 targeted=73/80`. Consistent. |
| campaign 18/132 against 18/139 | historical 6/59 + 12/73 = **18/132**. Correct. |
| whole seven-commit range audited | `189e1ed, 59309d6, 0f3ad01, 4d0a324, b667648, ba623667, 94bb7bb` — the full unaccepted span, not repair-only |
| four setup failures charged | retained, none credited as a semantic result |

**Executed C1–C26 CLOSED within amended scopes; original Phase A / R7 PARTLY;
H-01 P07 isolation, H-02 census `sortUndecided`, H-03 `ba623667` provenance all
OPEN on their original terms.** I endorse those as recorded. Its own
`UNCOVERED-OBLIGATIONS.md` states there is no additional execution gap concealed
behind those labels, and its numbers bear that out.

## 2. F-001 — what it is, at source

`Reach` is an inductive **`Prop`** (`Predicates.lean:96-101`) with two
constructors: `boot (h : comune_not_a_member view)` at `State.empty`, and
`trans : Reach s → stepEvent view s e auth = some s' → Reach s'`.

Every consumer in the tree takes it as a **hypothesis**:
`comune_not_a_member_of_reach` (`:1141`), `credit_pledges_of_reach` (`:1148`),
`:1165`, `reach_solvent` (`:1177`) — all `(hr : Reach view auth s)`.

`Mirrors.lean:29` calls `Reach` "NOT-EXECUTABLE, bounded: no arbitrary-`Reach`
oracle is required **under the standing boundary**", and
`check-lean-mirrors:152` admits it as a legitimate exception with "no oracle
required". **Neither cites the authority that establishes the exemption**, and the
auditor is right that R1's finite-mirror fence, R2's no-new-monitor ban, the
standing-exceptions ruling (audit-window placement, ceiling history, onward
ownership) and the v3.1 amendment (confined to P01/P07) do not supply it.

**So F-001 is an authority/classification gap, not failed code, not a false
theorem, and not a demand to manufacture a `Reach` decision procedure.** I agree
with that framing and adopt it.

## 3. The three-way distinction, and which observable is actually needed

**(a) Deciding arbitrary-state `Reach`** — given an arbitrary `State`, decide
reachability. Unbounded search over event sequences. **No consumer in the tree
requires it.** Nothing here asserts it is undecidable; it is simply not asked for.

**(b) Validating an explicit finite transition history** — given a concrete
sequence from `State.empty`, confirm each `stepEvent` step. **This is executable
today** by recomputation, and it is exactly what `Reach`'s two constructors
describe.

**(c) Using `Reach` only as a logical theorem premise** — what all four current
consumers do.

**The observable the existing contract actually needs is (b), and the tree already
says so in its own words.** `Trace.lean`'s `TraceStep` carries `input : State`,
`event : Event`, `result : TraceResult`, and its doc comment states that *"the
schema requires a consumer to check it against the state it recomputes, and never
to trust it as authority."* That is finite-history validation, specified by the
candidate itself. The `system-design` exhibit rule points the same way: what a
story must exhibit is a **concrete reachable witness**, not an oracle.

**A concrete gap I found that the audit did not name:** `Reach.boot` and
`Reach.trans` **appear nowhere outside the definition**. The executable trace
layer and the `Reach` proposition describe the same thing and **nothing in the
tree connects them** — there is no lemma taking a validated finite history to
`Reach`. That absence is why the exemption reads as an assertion: the artifact
that would make it self-evidently right does not exist.

**Neither the absence of a current consumer nor the absence of a `Decidable`
instance proves the consumer unnecessary.** That inference is the one the
exemption text currently makes, and it does not hold.

## 4. Does existing authority decide it? — No

I searched the admitted contract, its amendments, the standing-exceptions ruling
and the v3.1 amendment. **No existing authority decides whether this exact
executable consumer is required.** Axis 2 requires that decision and expressly
retains the issue; the implementation fences bound scope without making it.

**I return that as fact, not as a ruling of mine.**

## 5. Recommendation to the desk

**Rule that the required observable is (b) finite-history validation, and that
(a) arbitrary-state `Reach` decision is NOT-REQUIRED.** That is the reading the
candidate's own `TraceStep` schema and the milestone's simulator contract already
depend on, and it settles Axis 2 without inventing an oracle.

Then one of two dispositions, and the difference is small but real:

- **D1 — accept the exemption, correct its warrant.** `Reach` stays a
  NOT-REQUIRED consumer; `Mirrors.lean:29` and `check-lean-mirrors:152` are
  amended to **cite the ruling** instead of asserting a "standing boundary".
  *Fence:* comment/justification text only — no statement, proof, guard or
  behaviour changes. *Remaining verification:* one clean `just lean` to confirm
  the checker still passes with the amended exception text, plus a byte-diff
  showing only comment lines changed. *Cap:* the author is at 18/18 substantive,
  52/60 targeted, **submissions 2/2 spent** — so this needs **an explicit
  submission-cap exception and one substantive operation**, and I am not taking
  either silently.
- **D2 — additionally require the bridge.** As D1, plus a lemma carrying a
  validated finite `TraceStep` history to `Reach`, which would make the exemption
  self-evident rather than warranted-by-ruling. *Fence:* one new theorem in the
  existing mirror module, no new monitor (R2 intact), finite (R1 intact).
  *Remaining verification:* the same clean run plus its own mirror row. *Cap:*
  larger — a new proof obligation, so a named substantive allowance beyond the
  exhausted cap.

**My recommendation is D1.** D2 is the better artifact, but the gap is one of
*authority*, and D1 repairs exactly that with a comment-only change; D2 spends a
proof budget to make a point the ruling already settles. If the desk wants the
bridge, it is better commissioned as its own slice than bolted onto an exhausted
submission cap.

## 6. Preserved

F-001 **OPEN**. H-01, H-02, H-03 **OPEN on their original terms**. Cumulative
spend **12/73** for this audit seat, campaign **18/132**. No new ruling is
backdated as standing authority, the terminal auditor was asked to change
nothing, and **no third submission has been taken**. The S3 static audit continues
untouched. `#66` open; C1 keeps the next landing.
