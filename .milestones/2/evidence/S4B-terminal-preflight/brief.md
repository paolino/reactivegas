# BRIEF — fresh independent audit, S4-B (#66)

Fresh independent auditor. You inherit nothing. No owner claim and no earlier
result closes a row here, and you may reject any row you judge unestablished.

Load the shared `auditor` role plus `lean-auditor` and `commit-auditor`.

## Subject

- **Candidate: `189e1ed306f8f8e8bcdd11eeab4fc5657a518fc8`** (branch
  `chore/66-s4b-mirrors`, **unpushed** — local only, no PR exists).
- **Base: `3590c0015b84fd58004bf6fb44dd18b107304c48`** — accepted `master`, the
  landed S2R slice.
- **Your worktree:** `/code/reactivegas-66-s4b-audit`, detached at the
  candidate, verified clean, **zero oleans** at your START. Yours alone; the
  implementation owner works elsewhere and must not be contacted.

Subject is the S4-B slice: Prop/Bool mirrors, correspondence proofs, and the
mandatory checker — the full candidate against that base.

## Frozen authority — `admitted/`, with `MANIFEST.sha256`

| document | sha256 |
|---|---|
| `SUBMISSION-ADMITTED.md` | `363999bddcface9dbc856b9d060340ec689eb5840f734569facdcce618747b61` |
| `OWNER-BRIEF.md` | `b4a792017caa8847855d9e79924ed5f4434f489ec1c84da88cd62b58efbcd9da` |
| `OWNER-EVIDENCE.sha256` | `47acc2fd248c838ebbe255e580c0ae2faa3fedea3a0be9a37ce9307ab5f45dd4` |
| grant note | `94bd00f60a4a43400fcaf7aa867e478f2cbac37e31b32f7c918e3aaf07fcddbc` |

The owner's raw evidence is archived at
`/tmp/reactivegas/ms2/e-lean-compliance/commit-owner-s4b-muse/handoffs/evidence/`
(186 files, `EVIDENCE.sha256` above). **I archived it from `/tmp` myself** and
verified every copy byte-identical to its original. The packet's own text still
cites `/tmp/...` paths; treat the archived copies as the durable evidence and
say so if you find any divergence.

## Budget — reconcile before START

**8 substantive, 60 targeted — TOTAL across BOTH submissions**, not per
submission. A substantive invocation is a whole build (`just lean`, `just ci`,
`lake build`) under any wrapper, `nix develop -c` included; targeted is a
single-file elaboration or driver probe. **Failed and warm calls count.** Reads,
greps, `git` interrogation and hashing are free.

**Enumerate your full scope commands before your START** and return a concrete
gap **before** overrunning if the set does not fit. Do not reclassify a
substantive row as targeted to make arithmetic close, and do not compress
distinct rows into one invocation.

## What to press hardest

**1. The nineteen correspondence rows.** Each claims a statement, evidence, and
a falsification control. **Each retains its own falsification observation** —
neither the grant nor an aggregate green closes any row by implication. A row
whose control you cannot reproduce is OPEN, whatever the packet says.

**2. C4's scope, which the owner states carefully — check it holds.** C4 is
claimed to establish **execution-enforcement only**: the checker neutered to
unconditional `exit 0` while present, through the mandatory path, firing
`MIRROR-RECEIPT-ABSENT: checker did not operate`. **Semantic sensitivity is
claimed to rest on C2/C3/C5–C23, not on C4.** Verify that division is real and
that neither side is quietly carrying the other's weight. The packet says "127
never claimed" — check no exit-127 shortcut is doing hidden work.

**3. The spend ledger.** Claimed **8/8 substantive exact, 42/60 targeted**, with
S1, C1, C1r, C1g, C2, C3, C4, C26 all genuine and **no overrun**. Three of those
were reds, two of them avoidable (a tracked-source mismatch and a recipe-CWD
error). Confirm none was refunded, relabelled, or quietly folded.

**4. C26.** Cold `just ci`, exit 0, at the exact clean committed candidate.
Archived log sha256 `699792e4efa56b354bb3c3173751e538bc1739adbe91893199ec93b688978841`,
17,987 lines — **I verified that hash against the archive independently**;
verify it yourself rather than taking either of us on trust.

**5. Tree hygiene.** Probes were restored via `git checkout` and the tree
claimed clean after every control. Check the candidate bytes are actually intact.

## Method discipline

Label every conclusion **new-execution**, **unchanged-input with explicit byte
identity**, or **inspection**. Those are not interchangeable. Reading and hashing
retained output does not make it new execution.

Deliver `handoffs/AUDIT-REPORT.md`: one verdict per row — CLOSED / OPEN / PARTLY
— each with the command that establishes it, the observation, and its method
label. A row closed by the owner's assertion is not closed.

## Boundaries

Record at START, from live inspection: your PID/PGID, full live argv (model and
effort are argv-pinned — verify), cwd, `git rev-parse HEAD`, olean count, wall
clock. Your START must postdate this brief.

**Local files only.** No typing into any other pane or human composer. No push,
PR action, issue or review comment, gist, publication, deployment, candidate
edit, or merge — the branch is unpushed and must stay so. Do not contact the
implementation owner or any other seat.

Report what you find, including that the candidate is sound if it is.
