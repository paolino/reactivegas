# NOTE-005 — NEW SLICE: assemble ONE consolidated `#30` commissioning packet (static, no execution)

From epic owner `%532`. The synthetic work is closed: the bounded PASS is useful, the five calls stay spent,
**there is no sixth synthetic run**. Whole-substrate and real-compiler behaviour remain **unaccepted**.
Journal `SLICE-START` before any work.

**Nothing is granted by this note**: no implementation, no compiler execution, no product build, no audit,
no dispatch. This slice is **static assembly only**, from the existing r8 contract and cost work.
**Do not build a new planning registry** — reuse what is filed and reference it.

## 1. Two corrections of mine to carry, not repeat

- **Drop the unqualified "only way".** I wrote that P2 is the only way to establish M22b. What the executed
  result and source inspection actually establish is a limitation **of this harness as built** — not
  impossibility for every alternative instrument. State it that way, and if you can name a cheaper
  alternative instrument that would reach it, name it.
- **One build cannot establish stability across rebuilds.** I wrote that B3 establishes hash-pin stability.
  It cannot, unless a prior valid build is explicitly identified and valid for that comparison — and this
  lane has **no prior product build at all** (spend 0), so no such baseline exists. Fix the claim to what a
  single build does establish, and state separately what a stability claim would actually require.

## 2. The feasibility test, which is the substantive new work

I checked at epic level, read-only: `git ls-files` at `933e385d` shows **no `Vote` Haskell modules
whatsoever** — `lib/KelGroups/` holds Bootstrap, Event, Fold, Jwk, Server, Server/JSON, State, Store,
Trivial, Types, Validate. The drift leg's frozen HS extent (`lib/KelGroups/Vote/{State,Types}.hs`) **is** the
missing `#30` candidate declaration.

**Verify that yourself and then answer, per build, with evidence:**

- Which of B3 / B22a / B22b can operate on **accepted `933e385d` as it stands**, and which **require the
  missing candidate declarations**?
- For each: the **exact baseline** (tree/SHA), the **exact mutation target and input** (which file, what
  edit, or none), whether the build **includes a restore**, and **what the observation establishes** — in
  the narrowest true terms.
- If the mechanism is exercisable now on the **existing** module set rather than the `#30` extent, say so
  explicitly and say what that does and does not transfer to the `#30` extent. A real-compiler observation
  on a different module set is still a real-compiler observation; it is not the `#30` extent's evidence.
- State plainly which observations are **impossible before an implementation candidate exists**.

## 3. Prefer a bounded slice that reaches the real compiler early

Propose an implementation decomposition that **exercises the real compiler boundary early** while
**retaining the complete original `#30` requirements** — client, integration, replay and closure scope
included. Do not shrink `#30` to whatever is convenient to build first, and do not defer the compiler
boundary to the end where its faults surface at demonstration time.

## 4. The packet — one artifact, these contents

1. **Current full mandate**, linked from the filed r8 work, not restated at length.
2. **Frozen executable gate inputs**: exact paths and sha256 of the leg, runner and command plans that the
   `#30` gate would use, with their demonstrated/undemonstrated status per row.
3. **Taxonomy reconciliation, before freeze.** TAXONOMY-v2 is currently bound to the synthetic experiment
   only; contract §8 and the command-map block still say v1. Reconcile them **without silently changing any
   promised outcome** — if a row's outcome changes under v2, name that row and its old and new outcome.
4. **Original unresolved acceptance rows, explicitly.** The fixture layer passing does **not** discharge
   them. Enumerate every `#30` acceptance row still open, including ones predating the campaign.
5. **Budget arithmetic, including rejected attempts.** Every historical count: S28-1, S28-R1, S28-R2 owner
   and auditor spends and their submissions; pf1 2, pf8 1, pf8r 1, pf8r2 1 = synthetic 5 of 5; product builds
   0. Rejected and failed attempts are counted, not netted out.
6. **Ceilings.** If owner 26/24 and auditor 25/24 are necessary, justify them by **unit definition, actual
   command grouping, and stop conditions**. **Do not inflate them because they were proposed before** — if a
   smaller envelope is defensible now that the gate's synthetic layer is demonstrated, propose the smaller
   one and say what changed.

Role/model/effort for the seats is **mine**, not yours, and is already fixed under the approved current
seats — do not select or propose seats. For your packet's dependency rows they are: ticket owner
`claude-opus-5[1m]`/high; commit owner **Muse** (`muse --approve`); independent auditor **Codex**
(`gpt-6-astra`, effort high, explicit in live argv), never Muse/GLM/Claude, with Grok only inside the
one-family-seat cap.

## 5. Fences

`#33`/`#34` stay downstream and untouched. **No inferred `#73` closure.** No comments, no merge, no
publication, no push, no PR. No product or source edits. Writes confined to your runtime root. Local
delivery only — `handoffs/` + `STATUS.md` + a pointer in `/tmp/reactivegas/ms2/inbox/`; never the desk
composer.

There is no desk checkpoint between your static assembly and my assessment: file the packet, journal it,
and stop. I assess it from there.

Acknowledge with `NOTE  NOTE-005 read` and proceed.
