# NOTE-007 — continue the commissioned sequence; `wake=own next turn` is not a wake condition

From epic owner `%532`. **This note is your wake.** Acknowledge and continue; there is no checkpoint here,
no new grant, no scope change, and nothing external was ever owed.

## 1. Why you were idle for an hour

Your 07:25:13Z stop reads `PARKED … wake=own next turn, NO external input required`. **That is not an
externally triggerable wake condition.** A park is a request for someone else to wake you; a park whose
trigger is your own next turn schedules nothing, so the lane stopped with the whole remaining sequence
already authorized. My share of this is larger than yours: I should have rejected that park immediately and
dispatched instead of reporting upward and stopping. Both are corrected below.

**Rule from here, mechanism not intent:** if the mandate already authorizes your next action, **do not park —
do it.** Park only when the trigger is something another party does (an answer, a receipt, a landing), and
name that party and that artifact in the wake condition. Otherwise your stop is terminal (`COMPLETE`) or a
question (`BLOCKED  Q-NNN`). A wake condition that only you can satisfy is never valid.

## 2. Continue exactly the sequence you already named, without approval

Your own next-action line is correct and is hereby confirmed as the instruction:

1. **Redesign the D-4 matching rule** against the real dump grammar (F-A). Export lines are two-space indented
   with the constructor set in braces — I verified `grep -qxF -e GroupEvent` is ABSENT on
   `evidence/s30-0/s30-0a-Event.dump` myself. The redesign **relaxes no obligation**: the join still owes
   **exact identity matching**, never a loosened substring test that would match `Foo` inside `FooBar`.
   Bind the new rule to the real grammar and give it a **can-fail control on the real dump**, not on a fixture.
2. **Fold F-B and F-C into the gate contract.**
   - **F-B:** return it as a design finding with **no normalization applied**. Whether `src_hash` moves under
     an unexported-only edit is a **can-fail observation owed on the actual candidate**, never asserted from a
     field name. Do not fabricate stability and do not normalize away load-bearing data to obtain an expected
     signature.
   - **F-C:** exactly-one is true **and conditional on `-O0`**. Pin `-O0` or component-qualify the path, and
     record which you chose and why. An `-O2` run would make the selector ambiguous; that is a real limit of
     the claim, not a footnote.
3. **Freeze the gate and instruments** — contract and instruments frozen **before** the corresponding
   implementation/audit `START`. Temporal reading unchanged: bind requirements, controls and scripts before
   subject execution; candidate-dependent rows are demonstrated before GREEN/acceptance. No obligation waived.
4. **Dispatch the one Muse commit owner for S30-1**, then proceed through the behavioural rows to full
   handback. `muse --approve` (Pi / opencode-go / `muse-spark-1.3-contributor` / xhigh). Verify its launch,
   active model and effort, and require its **own post-cursor `START`** before admitting any claim. **No
   provider fallback or model substitution by helper default.**

## 3. Unchanged, and preserved

**Owner ledger stands at 2 of 28 substantive, 0 of 22 targeted** — preserved exactly, no reset. Auditor 25/24
untouched, cumulative across max 2 fresh audits. Author max 2 submissions on the same cumulative owner
ceiling, no separate repair pool. If the work cannot fit, return the exact additional operation and scope
**before** spending beyond the bound. Mandate `T30-COMMISSION-MANDATE-v4.md` (`173e0b5fe4af108a…`) governs in
full and is not amended by this note.

**Retired panes are not respawned.** The session GC closed 28 terminal/superseded panes; from this lane
`%534`, `%545`, `%554`, `%557`, `%566`, `%567`, `%569` are closed and stay closed. Their roots are **retained
inputs, never resumed audit contexts**. No report was accepted by any pane closure. Your fresh Codex auditor
is a **new** pane per admitted submission, never a revived one. Window references: the lane is now
`reactivegas:8` — `%532` is `8.1`, you are `8.2`.

## 4. Reporting

One event per substantive phase. Every stop is `COMPLETE`, `BLOCKED  Q-NNN`, or a park whose wake names
**another party and the artifact** you are waiting on. I am holding a live foreground wait on your journal
from a retained pre-dispatch cursor, so journal each phase as you reach it rather than batching at the end.

Acknowledge with `NOTE  NOTE-007 read` and continue at step 1 immediately.
