# C1 successor campaign S62-SIM-C1R — AUTHORIZED (NOTE-075), amended and frozen

Authorized by NOTE-075 `ad36e82c...` with five amendments, all applied below.
A **new named campaign** — `S62-SIM-C1R` — under existing #70. The retired 2/2
campaign gets no third bounce; all historical spend, roots and rejected
candidates are retained.

**Up to TWO submissions. No automatic raises.**

Candidate `280b67f14fa74d352b36bca98f87f03a3819308b` remains **unaccepted**.

---

## 1. Campaign state — two conflicting statements, then a disposition

**Amended per NOTE-075.1.** My first draft harmonised these by saying one meant
"rows" and the other "campaign", and concluded there was no contradiction. That
was wrong: the report header **literally labels the campaign OPEN**. I do not
get to reinterpret an auditor's words to make them agree with its journal.

**The two statements, recorded as they stand:**

1. `report.md` line 14 — *"Campaign: **OPEN** — ended by none; two BLOCKING rows
   remain BLOCKED. No third submission."*
2. terminal `STATUS.md` — `MUTATION-CAMPAIGN state=closed stopped=set-point
   rows=11 killed=9 residual=0 blocked=2 open=0`

**They conflict.** The auditor's evidence is unchanged and neither line is
edited.

**Owner scheduling disposition, mine and labelled as mine:** the campaign is
**closed for scheduling** — it is retired, it has no third submission, and no
further work happens inside it — while the **two blocking obligations remain
open** and are carried into the new campaign below. This is a disposition, not
a reading of what the auditor meant.

**`CLOSED` is not `accepted`.**

## 2. Credit that survives — and what does not

| retains credit | why |
|---|---|
| 9 of 11 rows KILLED by real mutants | independently established at `280b67f` |
| cold-CI `EXIT=0` from absent `.lake`, `5a22a2fe…` | auditor's own build, not inherited |
| gate v14 `EXIT=0` in the auditor's tree, `890d522b…` | same |
| F1, F2, F3, F5, F6, F7 closed | verified by the auditor, not by author claim |

**No inherited acceptance.** Neither the prior PASS rows nor a new campaign
grants acceptance to `6879970f..candidate`. The whole unaccepted range —
including prefix guarantees and actual final-master integration — is the audit
subject again.

## 3. The two classes the repair must close

Written as executable acceptance, because the last two attempts both closed
*instances* and the mandate never said the class checkably. That is the defect
this section exists to fix.

### C-KEY — substrate string keys survive every UI control

**Not** "fix `:4541`."

- **Derive** the extent from the **actual handlers**, not from a memory: every
  control whose handler writes `nav().u` or an event `user`/`target`. Today that
  is at least `data-goto-person` (`:3752`, `:3877`, handler `:4540–4541`) and
  `data-act` `dataset.u` (`:4528`);
- **reconcile derived controls to exercised witnesses**: every derived control
  must have a witness that actually drove it. A derived control with no witness
  is a **coverage failure**, reported as such;
- **executable omission control**: remove one control from the harness and the
  reconciliation must go red. A coverage claim that survives dropping a control
  is not measuring coverage;

**Limit, stated rather than glossed (NOTE-075.3):** this is *not* exhaustive
traversal of all reachable application states, and no receipt may claim it is.
A finite journey suite cannot establish that. What it establishes is that every
**derived** control was exercised, and the derivation is from real handlers.
Controls reachable only through state the suite never constructs are **outside
the witness set and must be named as such**.
- identities preserved exactly: **leading-zero** (`"01"`), non-numeric, and
  Unicode;
- **numeric collection IDs remain numeric** — this is not a blanket
  de-numbering;
- an **intentional coercion mutant** must be caught **on actual interaction**,
  not by reading source.

Acceptance: a control that coerces a key fails the gate. A control the harness
never drove is a **coverage failure**, not a pass.

### C-CHROME — ordinary rendered chrome, with the evidence still present

**Not** "fix `:3931`."

- Cover the **enumerated meaningful render classes**, derived from actual
  rendering rather than assumed: **cards** (incl. `#govcard`), **dialogs/pop**,
  **refusal messages**, **toasts**, **feed entries**, **teaching strips**. Each
  class is a required row; a class with no witness is a coverage failure;
- **same limit as C-KEY**: enumerated classes exercised, **not** a claim of
  exhaustive traversal of every possible view state. Name what the suite cannot
  reach;
- **without removing `.mono`.** `economics-simulator.html:5123` currently does
  `c.querySelectorAll('.mono, script, #pop, .toast').forEach(el => el.remove())`
  before its vocabulary scan, which is why the heading could not fail. A check
  that deletes the evidence before looking is not a check;
- **visible proof-state text derived from the actual receipt**, and tested in
  **both** states — a corpus that is fully `provato`, and one with an
  `enunciato` — so the sentence is proven to track the receipt rather than
  happening to match it today.

- **no `.mono` / `#pop` / `.toast` erasure may hide ordinary text.** The scan
  runs against text a reader actually sees;
- **technical identifiers stay available where the approved proof and citation
  surface permits them** — R-CIT requires linkable Lean identifiers, and this
  class must not become a ban on them (the NOTE-059 correction, still binding).

**Explicitly forbidden:** hiding the sentence, or deleting the user-visible
proof explanation, to pass. The explanation is part of the artifact's purpose.

### C-USERID — stale commentary

Numeric-`UserId` commentary in the owned HTML may be corrected. Cosmetic,
in-fence, no semantics.

## 4. Fixture-shape advisory — explicit disposition, and a correction to me

**A parked token is not an executable retired branch.** The advisory is bounded
and is **not** a blocking row.

**Correction I owe:** I have described the probe's exemption as covering
"a Lean-emitted fixture constant whose bytes the trace gates verify". **The
regex does not bind that.** It is
`/^const\s+[A-Z][A-Z0-9_]*\s*=\s*\{"/` — a test of **shape only**. Byte
verification happens in a different gate, and my wording implied the probe
established a link it does not establish. I withdraw that claim.

**Disposition, per NOTE-075.5: accepted as a NAMED BOUNDED LIMITATION for this
successor campaign. Its cleanup is NOT required here.**

- it is **not** a blocking row and does not enter the row ledger;
- the exemption's linkage to byte-verified constants **is not claimed** — the
  regex tests shape only, as corrected above;
- **the live-retired-branch tests stay**: the dispatch-extent, refusal-validity
  and execution layers are untouched and remain the real protection;
- it travels in the acceptance packet as an open limitation with its honest
  limit, not as something closed.

## 5. File fence

**Exact paths, named before freeze (NOTE-075.4).**

*May modify, in the repo:*

```
economics-simulator.html          key preservation, receipt-driven chrome, stale comments
economics-simulator-core.mjs      ONLY as required for key preservation and receipt presentation
economics-simulator-ui-gate.mjs   NEW — the C-KEY / C-CHROME gate
```

*New versioned successor instruments, in the lane root — the old four stay
immutable and are NOT edited:*

```
handoffs/ui-surface-probe-v1.mjs        NEW — derives controls/render classes, reconciles to witnesses
handoffs/gate-v15-one-membership.sh     NEW — v14 plus the C-KEY/C-CHROME rows
```

Immutable, retained, never edited: `gate-v14-one-membership.sh`,
`retired-surface-probe.mjs`, `oracle-one-membership-v2.mjs`,
`derive-cited-sources.mjs`.

**Core edits are fenced to key preservation and receipt presentation. No
economic semantics change.** All inherited guarantees and full CI are retained.

*May not touch:* `lean/` **in any form** — Lean source edits are forbidden,
except a later separately accepted-base integration if one becomes required,
which is recorded as such and is not part of this repair; `docs/en/design/`
(frozen while #71 is open); any sibling lane's source; master-side scripts;
`.github/workflows/`; `flake.nix`; `cabal.project`.

## 6. Audit scope — whole candidate, again

Extent `6879970fdb1a797263843387e14704eaa1e3a2e7..<successor candidate>`, plus
integration against the then-current accepted master. Prefix guarantees
R-GEO / R-CIT / R-ITA / R-LAY are re-audited; **the nine remain unaccepted**.

## 7. Seats

| seat | family | model / effort | note |
|---|---|---|---|
| commit owner | **muse** — fresh root, fresh context | wrapper-fixed; argv read at launch and recorded | preserves `%527`'s root, evidence and commits |
| auditor | **codex** | model and effort pinned explicitly in the brief, verified against live argv before START | **grok's ticket allowance is consumed** |

Family fence: **codex or grok only**; grok spent; therefore **codex**. The
auditor's project outputs must be **genuinely cold** — absent project `.lake`,
built in its own tree. I will not warm it this time; that was my error last
round and it cost a note to undo.

## 8. Budgets — NOTE-075.2, two counted kinds

**Counting rule.** A **substantive** invocation is a full `just ci`, a full gate
run, or a full suite run — **counted whether warm or cold, and whether it passes
or fails**. An outer wrapper does **not** erase nested substantive commands:
running the gate inside a script still charges the gate. A **targeted**
invocation is a single probe, a single mutant, or one focused command.

| counter | owner | auditors (both submissions) |
|---|---|---|
| substantive | **12** total | **8** total |
| targeted | **40** total | **60** total |
| spent at authorization | 0 | 0 |
| ceiling raises | **0**, no automatic raise | **0**, no automatic raise |

Auditor allocation is **explicit before each fresh START** — the split across
submission 1 and 2 is named in each brief, not drawn down ad hoc.

**Redundancy rule:** do not repeat an invocation once the evidence suffices. The
budget is a ceiling, not a target.

**Separate books.** The retired campaign's **5/30** and the **UNKNOWN**
prior-owner accounting are preserved as their own records, with **no implied
subtraction** from these counters and no netting in either direction.

**Feasibility, stated before execution as NOTE-075.2 requires.** I judge 12
substantive sufficient for the owner: roughly one full gate per class once
built, one full `just ci` at the freeze, and one re-run after any correction —
about 6, leaving 6 of headroom. 8 substantive across two audits is 4 each,
covering one cold `just ci` and one full gate per submission with 2 spare.
**I see no gap.** If one appears mid-campaign I report the specific gap **before**
spending past the ceiling, not after.

## 9. Historical attempts — enumerated, not zeroed

Recoverable from durable roots on disk:

- **commit-owner roots: 21** — 17 archived, 4 current
  (`commit-owner-fable*` ×13, `fidelity*` ×3, `codex-italian-copy`,
  `geometry-permalinks-glm`, `one-membership-glm`, `s62sim-muse-successor`);
- **commit-auditor roots: 11** — 8 archived, 3 current.

Receipt coverage is **partial**: roots exist for all of the above, but
per-attempt build charges are not recoverable for the pre-S62 campaigns. Those
are reported **UNKNOWN with partial receipt coverage — not zero.**

## 10. Sequencing

C1 landing keeps priority over #68/#69; this proposal is urgent because it holds
their integration order. **C2 preparation may continue; no C2 implementation on
a rejected C1.**

## 11. What I am not doing

No implementation dispatch, no seat launched, no builds, no candidate edits, no
push, no PR, no merge, no publication. `%527` remains live and uninterrupted,
`%315` parked, `%535` complete with its worktree retained pending your word.
