# NOTE-002 — instrument amended to v2. Two of your controls tested the wrong thing.

**Authoritative instrument is now
`../handoffs/S4B-ACCEPTANCE-INSTRUMENT-v2.md`, sha256 first16
`2214ff8a0d25f47a`.** v1 (`44c48239d9b62ef7`) is preserved **with its inaccurate
statements intact**, not erased.

**Verify the v2 hash yourself and journal it. Your spend is retained — verify and
state your current exact figures; my read is 11/60 targeted and 0/6 substantive
at the point before these controls, and your journal shows 31/60 after C5–C23.
Reconcile and report the true numbers.** **No forced restart, no automatic extra
budget.**

**Both errors originate in my v1 wording, not in your compliance with it.**

## Amendment 1 — C5–C23 sensitivity

v1 said *"dependent theorem mutated to fail"*, and you planned **P01/P07
statement mutations**. **Withdrawn.**

**Mutating the theorem only tests a different statement.** It does not establish
that the original correspondence **detects an implementation defect** — which is
the whole purpose of the row.

**Corrected:** mutate the **executable definition or input**; the **mutated
definition stays well-typed**; the **production statement is preserved, never
mutated**; and you observe **the original dependent theorem stop elaborating**,
in **isolated scratch under a temporary control-only exception**.

**P01/P07 reuse existing expressions — so mutate that expression or its operative
definition**, keep the production statement intact, and observe the original
theorem fail.

**If a definitional-equality row genuinely needs a different kind of sensitivity
evidence, state that accurately** — do not present theorem mutation as a
proof-strength claim.

**The nineteen is not an allowlist.** Bind spellings mechanically, but **derive
the actual required identities and reconcile**.

**Your C5–C23 rows must be re-established wherever they mutated a statement
rather than an executable definition.** That costs targeted budget: **measure it
and return the gap before overrun. Do not absorb it, and do not drop rows.**

## Amendment 2 — C4

You read C4 as **deleting the checker executable to get 127**. **Exit 127 proves
missing-tool failure only.**

It does **not** test a **checker disabled to unconditional success**, nor
**removal or bypass of the mandatory invocation**.

**Corrected:** an executable control that makes the checker **or its invocation
ineffective while present**, detected by the **permanent mandatory mechanism**
with **correct failure attribution**.

What is being established is that **the actual invocation is operating** — not
that the pipeline breaks when a file disappears. **127 does not close this row.**
**Green-but-disabled acceptance is forbidden.** Choose the mechanism freely;
**do not weaken it to fit your existing draft.**

## And a framing correction you should not inherit from me

**v1's requirements prose is not, by itself, an executable frozen gate.** It fixes
identities and expected failure classes. **The executable mechanism is `just
lean` / `just ci` plus the checker, and its evidence is the run.** Do not cite the
instrument as pre-proved executable evidence.

## Accepted base has moved

`master` is now **`d67032313acf3699cc50358a057391b88d002192`** (PR #87, exporter,
parent `4a6cd87`). **It changes `lakefile`, `justfile` and CI, and expands the
compiled inventory.**

- **Plan final accepted-base integration BEFORE the final independent audit.**
- **Preserve your full owned diff and controls** across it.
- **Do not expect 1213 or 23 to remain constants** — they described a superseded
  tree.
- **Account for the incoming bytes** and **re-establish the mandatory controls at
  the actual final candidate**.
- **Return a measured remaining-command gap before cap overrun. No reset.**

Note your sibling `%544` also faces this rebase; **neither of you overwrites the
other's `justfile` lines**.
