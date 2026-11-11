# BRIEF — independent STATIC audit, S3 Phase 1 assessment

Fresh independent auditor. **You inherit nothing.** Prior reports, corrections
and desk observations in `admitted/` are **inputs, never accepted conclusions**.

Load the shared `auditor` role and `lean-auditor`.

## ZERO-EXECUTION AUTHORITY — read this before anything else

**Builds: 0. Lean elaborations and queries: 0. Runtime mutation and probe
executions: 0.**

**Permitted:** read-only source, artifact, dependency-graph, hash and JSON
reconciliation, and writing your own local review evidence.

Three consequences, all binding:

- **No semantic code-review claim requiring a compiled witness may be declared
  established from source alone.** Say what source inspection shows and stop
  there.
- **Historical executed evidence may be evaluated for its stated identity and
  scope — never called an independently repeated execution.**
- **Missing required evidence is a FINDING with an owned next disposition. There
  is no presumed PASS because compilation is unavailable.**

Any irreducible need for new execution is **returned concretely, unfunded**.

## Subject

The **complete Phase 1 assessment** against:

- its original brief — `admitted/PHASE1-BRIEF.md`
- **`admitted/S3-MANDATE-rev3.md` — operative revision 3.** Known artifact
  defect, disclosed: its opening line still reads *"revision 2"* and its
  preservation note mentions only v1, though a v2-superseded file exists. **The
  header lags the content. Read it as revision 3.**
- all amendments; accepted source **`3590c0015b84fd58004bf6fb44dd18b107304c48`**
- the bound OP-10 raw output, source inventories, receipt inventory and actual
  costing evidence

**Your checkout:** `/code/reactivegas-66-s3-audit`, detached at that SHA,
verified clean. **Treat it as read-only** — you have no build allowance to use it
with.

## Frozen packet — `admitted/`, 16 entries, `MANIFEST.sha256`

Reports `PHASE1-REPORT.md` and R2–R5, `CORRECTIONS-008/009/010`,
`FINAL-RECEIPT.md`, `OP10-identities.txt`, `P1A-qualified-classified.txt`,
`P1A-classification-working.txt`, the Phase-1 brief, the mandate, the desk's
independent identity map, and `COMMISSION.md`.

Verify with `sha256sum -c MANIFEST.sha256` from that directory.

## You may challenge every row

Including — explicitly — **any alleged absence, any exactness claim, any cost
claim, any provenance claim, and the sufficiency of name-based classification.**

**Do not silently narrow the assessment to the last correction.** The subject is
the whole Phase 1 assessment, not the delta since R5.

Live threads you should not treat as settled merely because they were discussed:

- the **974** remainder: the coarse partition `163 + 76 + 1 + 12 + 961 = 1213`
  adds up, but **GEN-OTHER 961** is one broad bucket whose census is explicitly
  **non-exclusive with overlap**. Whether that constitutes per-identity
  classification is yours to judge.
- the **Validate cost class**: a three-node closure charged **4 (3 rebuilds + 1
  check)** while a sibling three-node closure is charged **3 (2 + 1)**, and the
  text elsewhere calls 3 the worst case.
- the **4-against-3 build overrun**, retained and not refunded.
- whether **name-based recognition** can carry the classification weight placed
  on it, with its limits stated.

## Deliver

One **terminal report**, a **per-requirement ledger**, and **hashed local
evidence**.

Per requirement: **CLOSED / OPEN / PARTLY**, each with what establishes it and —
given zero execution — whether that is **inspection**, **retained-evidence
evaluation at its stated scope**, or **unestablished**. Those three are not
interchangeable.

## Boundaries

Record at START from live inspection: PID/PGID, full argv (model and effort are
argv-pinned — verify), cwd, `git rev-parse HEAD`, `$TMUX_PANE`, wall clock. Your
START must postdate this brief.

**Local files only.** No contact with the implementation owner or any other seat.
**No code edits, pushes, PRs, comments or merges.** No build, elaboration or
probe of any kind.

Report what you find, including that the assessment is sound if it is. A row
closed by the owner's assertion is not closed.

## Dispatch bindings — verified placement, stated so you need not reconstruct them

| field | value |
|---|---|
| ticket / slice | **#66**, **S3 Phase 1** (assessment review) |
| `ticket_owner_pane` (commissioner) | **`%503`**, window **named** `lean-quality` |
| `owner_pane` (Phase-1 implementation owner) | **`%558`**, window **named** `rg-s3-phase1` |
| your pane / window | **`%568`**, dedicated window **named** `rg-s3-static-audit` |
| model / effort | `codex -m gpt-6-astra -c model_reasoning_effort=high` — both argv-pinned; verify from `/proc` |
| report path | `handoffs/AUDIT-REPORT.md` (yours) |
| per-requirement ledger | `handoffs/REQUIREMENT-LEDGER.md` (yours) |
| onward-discovery path | `handoffs/ONWARD-DISCOVERIES.md` (yours) — **write it even if the witnessed set is empty; say so explicitly** |
| named census/backlog owner | **`%503`**, reached **only** through the commissioning chain |
| submission | **1** — no prior static audit of this assessment exists |
| **execution budget** | **ZERO builds, ZERO elaborations/queries, ZERO probes.** Read-only reconciliation and local writing only |

**Window placement, stated rather than inferred.** Commissioner `%503`, the
Phase-1 owner `%558`, and you each sit in **separate named windows** of session
`reactivegas`. **Indices are volatile** — root reorganises windows — so bind by
**name** and re-resolve with a targeted `tmux list-panes` query against these
exact pane IDs, never an untargeted sweep.

The dedicated audit window is the **deliberate placement** for this commission,
consistent with the same exception already authorized in this ticket: the rule's
purpose — a distinct, non-reused seat, not shared with owner or commissioner — is
satisfied more strongly, not less. **Record the placement and this reasoning in
your report.**

There is **no ceiling-raise ledger** to reconcile here, because **this audit has
no execution allowance at all**. The Phase-1 owner's own history — including the
retained **4-against-3 build overrun** — is an **input you may challenge**, never
your budget.

If any dispatch field is still missing, **return it as a contract gap** rather
than reconstructing it.
