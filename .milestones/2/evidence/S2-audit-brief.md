# Auditor brief — S2 candidate `5745a2c`, FRESH FULL audit

**Seat:** `codex`, model **`gpt-6-astra`**, launched with `-m gpt-6-astra`
explicitly. Fresh context. The author is `muse`; `grok` audited both S1
candidates; a `codex` context audited the epic owner's judgement long ago and is
gone. You have seen none of this slice. **Verify and state your live model
identity in your first journal line.**

**Read-only. No repair. No contact with the commit owner.** Take your own
detached snapshot — the shared tree is live.

**Deliver your report LOCALLY**, to `handoffs/AUDIT-REPORT.md` in this
directory, and hash it into `handoffs/HASHES.txt`. **No gist, no push, no
external artifact, no publication of any kind.** There is no
external-publication authorization in this lane. Two prior audits stalled at an
approval overlay after inventing a publication step; this line exists so you do
not.

## Identity and the complete final contract

| field | value |
|---|---|
| base | `4a6cd87fcbc3e4a536bbc9f240f5efe5704022af` (landed S1 on `master`) |
| **candidate** | **`5745a2c`** on `chore/66-s2-axiom-gate`, worktree `/code/reactivegas-66-s2` |
| diff | 6 files, +359/−29 |
| **mandate v2** | **`7cfb7aec95a37448`** = `../commit-owner-s2-muse/brief.md` **+** `../commit-owner-s2-muse/inbox/NOTE-001-amendment-1-remove-the-quota.md`, concatenated in that order |
| mandate v1 (superseded, preserved) | `0a1db9887ccc9d8f` = `brief.md` alone |
| frozen gate at base | `39d6aa4e2c0c0170` |
| versioned packet | `../handoffs/S2-FROZEN-PACKET.md` — read **both** versions |
| submission | **1 of 2** |
| **owner build budget** | **8, all 8 spent.** A repair needs a ceiling raise; there is no slack |
| your build budget | **5** |
| ceiling raises | 0 / 2 |
| owner packet | `../commit-owner-s2-muse/handoffs/SUBMISSION-1.md` + `AXIOM-IDENTITIES.txt` |
| owner journal | `../commit-owner-s2-muse/STATUS.md` |

Ticket/epic owner `claude-opus-5[1m]` `%503`; commit owner `muse` `%523`.

## What the slice does

Three rows. **A** adds `scripts/check-lean-axioms`, a total axiom gate over a
discovered extent, wired into `just lean`. **A2′** removes
`expectedDeclarations := 163` and its two `== 163` assertions from
`check-reactivegas-inversion-coverage`, replacing the quota with reconciliation.
**B** renames three checks that were green under names describing properties they
do not compute, deletes three dead re-exports, and binds each real obligation to
its actual enforcer. **C** fixes one doc-comment path.

## Rows to attack — re-derive every one at `5745a2c`

Do not let the packet's own evidence close its own rows.

1. **Three genuinely independent derivations.** The gate claims **S** from
   `git ls-files` cross-checked against a `find` walk, **B** from
   `env.header.moduleNames`, **T** from `thmInfo` via each module's own
   `constNames`. **An import list checking itself cannot detect its own
   omission** — verify the independence is real and not two views of one
   inventory. Break each derivation separately and require the gate to notice.
2. **No quota anywhere.** No expected-count constant in gate, driver, script,
   `justfile` or `lean/`. Counts reported; reconciliation decides. Verify by
   reading the diff, not the claim.
3. **A2′ constraint 3** — a real added valid theorem/module **passes**.
   Re-derive; the owner shows `declared=164` green.
4. **A2′ constraint 4** — omitting it from the compared extent **fails**, with
   its identity.
5. **A2′ constraint 5, the load-bearing one** — a theorem depending on `sorry`
   or a non-standard axiom fails **for its dependency**, not because a count
   changed. The owner's Run B2 claims the quota stays blind (`163` unchanged,
   `inversion-audit: ok`) while `check-lean-axioms` catches `sorryAx`. **This is
   the amendment's justification; re-derive it yourself.**
6. **A6 axiom policy.** Allowed set exactly `propext`, `Classical.choice`,
   `Quot.sound`, justified as the existing `permittedAxioms` at
   `check-reactivegas-inversion-coverage:101` extended from six inversions to the
   whole extent. Is that faithfully what the gate enforces?
7. **A6 control shape** — the non-standard-axiom control must be an `axiom`
   **plus a theorem that uses it**. A declared unused axiom never enters
   dependency collection. Confirm the owner's control has the using shape and
   actually exercises collection.
8. **A4 nonzero discovery** — zero S, zero B or zero T fails.
9. **A8 totality** — the wrapper asserts absence of `PANIC at` in **both**
   streams and inspects results, not just exit status. The rule this lane
   established: 70 panics once coexisted with exit 0. Show the assertion can find
   a panic.
10. **A5/A7 added-module control through the MANDATORY path** — `just lean` /
    `just ci`, not a probe.
11. **Row B behaviour preserved bit-for-bit.** Same Bools, same values, same
    `by decide` proofs, only names changed. Evaluate both sides; do not read it.
12. **Row B deadness** — the three `TraceTests` re-exports were deleted as dead.
    Verify they were dead **before** the deletion.
13. **No model change, no theorem statement change.** Verify across the whole
    diff.
14. **Fence** — 6 files: `justfile`, `Invariants.lean`, `Predicates.lean`,
    `TraceTests.lean`, new `scripts/check-lean-axioms`,
    `check-reactivegas-inversion-coverage`. Anything else is a finding. Nothing
    under `docs/`.

## Declared limits — press on these hardest

The owner declared four. Declared limits are where untested claims hide.

- **L1: def-shaped `sorry`/axiom dependencies with no theorem use are outside the
  `thmInfo` sweep**, argued inert because `collectAxioms` is transitive, with a
  transitivity control. **Is the transitivity argument sound, and is the control
  the right one?** Construct a case where a def-shaped dependency reaches a
  proven statement and is *not* flagged, or establish that none exists.
- **L2: the `B \ S` branch never fired in any run.** An untested branch in a new
  gate. Fire it.
- **L3:** `specs/62-one-membership-model/functions-model.md` still names the old
  `checkI57*` identifiers in historical prose, left untouched as outside the
  fence. Judge whether leaving it is right, or whether a stale identifier in a
  spec is a finding.
- **L4:** stale `.lake` artifacts hold old symbol names. Cache only?

Also press: the **removed-module refinement** the owner recorded rather than
hid — dropping `Reactivegas.Trace` alone stays green because the umbrella
re-imports it transitively, so the check targets *environment reachability*. Is
that the semantically correct target, or does it hide a class of omission?

## Rules

- Evidence-bound. A finding restated without new evidence is not a finding. **A
  check whose subject is source text is a lead, never evidence** — it cannot
  close a row, stand as a control, or enter a RED bundle.
- Anything you build, show it can fail.
- Budget **5** builds; report the spend. Your snapshot starts cold, which the
  cold-provenance row needs anyway.
- **Do not paste panic backtraces.** Cite path, count, first line.
- No stray `.lean` under `lean/` in the shared tree.
- Local `just ci` on `5745a2c` is being re-run by the ticket owner independently
  of the owner's receipt; its result is the owner's to weigh, not yours to wait
  on.

## Return

One integrated verdict. Findings ranked, each with the command that produced it.
An explicit list of rows you could **not** independently close. Name the
worktrees for the ticket owner to retire. "The candidate is sound" is acceptable
if the evidence says so — but say what you tried that could have shown otherwise.
