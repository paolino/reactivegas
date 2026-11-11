# Brief — inspector `s2-insp-semrec3` (kelgroups #30, slice S30-2, submission 1)

Replacement seat covering the two scenarios submission 1 has **no independent
verdict on**. You inspect and report; you ship no code, edit nothing, accept
nothing, never contact the commit owner. Load `commit-auditor`, `auditor`,
`worker-protocol`, `verification`, `invariants`, `haskell`, `gate-script`.

This packet is derived from the `auditor` skill's own required-authority list,
read at dispatch time. Three seats on this ticket have refused on packet
defects and two of those were mine; if something below is still missing, refuse
— that is the contract working, not a nuisance.

## Required authority packet

| item | value |
|---|---|
| **commissioning owner** | ticket owner, pane `%572`, family `claude`, model `claude-opus-5[1m]`, effort `high` |
| **author of the subject** | commit owner, pane `%607`, family `muse`, harness `pi`, provider `opencode-go`, model `muse-spark-1.3-contributor`, effort `xhigh`; state `PROOF-COMPLETE submission=1`, parked and write-idle |
| **your seat** | Codex `gpt-6-astra`, effort `high`, pane recorded at dispatch |
| **frozen candidate** | `b7cca4e109c213f7c344ee23ea7f654461654ea8` |
| **RED** | `f5259b5d30ebe49f6caecb29a4bb6191de5702fa` |
| **base** | `9762ad4db50f370348ea71abd44f7e969349d4b4` |
| **submission** | 1 (not a re-audit; no rejected SHA, no prior open finding set is yours to carry) |
| **mandate (external truth)** | `handoffs/T30-S30-2-MANDATE.md` sha256 `9a4fbd2c9a61ed64354bc8b5f5c1b1ad3de09d5c91c9839de809339f0b8b2cdb` |
| **oracle** | `instruments/s30-2-oracle` sha256 `f885af5667f9df5a2b95ed05a1d7af1c17bdf15ec51e7eef8e213eb9018beb57` |
| **frozen gate** | `/code/kelgroups-s2i6-b7cca4e/gate.sh` sha256 `1c1889b679e8cc3a89acaa645e4003b67ff007e02f40f919016084855c2ca580` |
| **REQ evaluator** | `instruments/req-records.sh` sha256 `297a72593cca7247a2af89970612a2f46b4f9c0ba09e1dcc2fb551bda521ba8f` — **recorded only after the freeze**, see §Disclosures |
| **verification commands** | `./gate.sh` in the lane; `nix develop .#ci --quiet -c cabal test invariants -O0`; read-only git over the candidate |
| **allowed scope** | read the candidate, the frozen Lean at reactivegas `3590c001`, retained evidence; build probes and harnesses under your own runtime root |
| **forbidden scope** | any edit to the candidate, gate, oracle or instruments; contact with the author; push, PR, merge, comment; `lean/**` edits in either repo |
| **runtime root** | `/tmp/reactivegas/ms2/e-kelgroups-substrate-opus-20260906/t30-contract-opus-20260906/s2-insp-semrec3/` |
| **worktree** | `/code/kelgroups-s2i6-b7cca4e` — clean, detached, read-only to you |
| **evidence path** | `evidence/` under your root; **report path** `handoffs/REPORT.md` |
| **resource budget** | **2 executions**, from the shared S30-2 audit counter (8 total, 4 spent, 2 committed to a later delta seat, these 2 are the remainder). One `./gate.sh` run is one execution. Instrument runs over synthetic fixtures that invoke no compiler are **uncharged** |
| **stopping rule** | at 2 executions you stop and report what you have, naming the gap; you do not overrun and you do not invent authority |

## Coverage denominator — finite, and it is these seven rows

| row | severity | meaning |
|---|---|---|
| `INV-S30-2-SINGLE-SITE` | BLOCKING | `verdictOf` is the only place a verdict is decided; no other export returns a `Verdict` |
| `INV-S30-2-PARAM` | BLOCKING | threshold explicit at every call; no default shipped; `legacyThreshold`/`zeroThreshold` unexported |
| `INV-S30-2-ORDER` | BLOCKING | collective: assent arm tested before dissent against the **same** `required`; both reaching it yields **positive** |
| `INV-S30-2-PERM-NEVER-TALLIES` | BLOCKING | permission verdict depends only on the designee's presence in assents/dissents |
| `INV-S30-2-CURRENT-FRANCHISE` | BLOCKING | franchise size read from the canonical view at evaluation time; no stored snapshot |
| `INV-S30-2-SHARED-ADMIN` | BLOCKING | `isResponsabile` reuses `isAdminInView`/`hasAdmin`; no parallel admin notion |
| `INV-S30-2-REQ-RULE` | BLOCKING | the G-6 evaluator actually discriminates: accepts only `PASS: <ID> OK` as a full line at column 0; rejects `FAILED`, `SKIPPED`, an indented record, a title mention; **refuses** when no REQ rows are registered |

Undeclared is BLOCKING. Ground for rows 1–6: frozen Lean `3590c001`,
`lean/KelGroups/Vote/State.lean`.

## Your two scenarios, and why you have both

**A — semantics against the frozen Lean** (rows 1–6). Does `verdictOf` match
the Lean exactly: threshold consulted at the *current* franchise size, assents
before dissents against the same `required`, both-reach yielding positive, and
a permission arm that consults neither threshold nor tally? Is any franchise
value stored rather than read? Has a parallel admin notion appeared?

**B — the REQ evaluator's behaviour** (row 7). Re-derive it from scratch
against synthetic streams. **Do not trust its bytes on my word** — see
disclosures.

You hold both because the seat that had A refused on my packet defect and the
seat that had B was defeated by a bug in its own harness. **Combining them
costs independence between A and B**, and I am telling you so rather than
pretending it is free: weigh them separately in your report.

## Disclosures — mine to make, not for you to discover

1. **I did not hash-bind `req-records.sh` at freeze time.** The hash above was
   recorded afterwards, so it proves nothing about what ran during the
   candidate's gate. Verify behaviour, not provenance, for row 7.
2. **The RED failed via `ErrorCall` from stubs**, not wrong values. A property
   that has only ever observed an exception is not known to discriminate
   values.
3. **A sibling seat has already found one surviving mutant** against the
   permission-invariance property (`test/VerdictSpec.hs`). You are **not**
   asked to re-derive that, and you must not treat it as settling row 4's
   semantics: a property can be mutation-weak *and* the implementation still
   correct, or the reverse.

## Output

`findings.jsonl` — one JSON per line with `fault_class`, `location`,
`severity`, `inspector`, `evidence` **as a non-empty string or array of
non-empty strings** (an object is rejected by the collector), and the property
class. Plus a hash-bound `handoffs/REPORT.md`. Milestones in the **TAG column**:
`START`, `AUDIT-RESULT`, `COMPLETE`.

You recommend; the ticket owner adjudicates once over the union. Report and
exit. You are not alone in the codebase; do not revert edits made by others.

---

# Replacement dispatch — the environment is built and proven, not asserted

Your predecessor `s2-insp-semrec` refused correctly at 0/2: the gate hash it
was given matched, but **the file was not at the required path in its
worktree**, and its brief forbade substitution or repair. That was my defect,
and a general one — **`./gate.sh` is untracked and gitignored by design, so no
fresh worktree ever contains it.** Placing it is a dispatch step. I have now
done it and proved it before writing this line:

| proof | result |
|---|---|
| gate present at `/code/kelgroups-s2i6-b7cca4e/gate.sh` | yes |
| sha256 equals the frozen `1c1889b679e8cc3a89acaa645e4003b67ff007e02f40f919016084855c2ca580` | **match** |
| executable mode | `555`, executable |
| write refused — **attempted**, not assumed | `Permission denied`; hash unchanged after the attempt |
| worktree still clean (gate is gitignored) | `porcelain=0` at `b7cca4e` |

Run the gate **from your own worktree** so it evaluates your isolated copy.

## Two things about your siblings you should know and not re-derive

- **`CTRL-1` is an existing BLOCKING finding** from `s2-insp-controls`:
  `permission-threshold-value-coverage-survivor` at `test/VerdictSpec.hs`,
  one surviving mutant against the permission-invariance property. **Do not
  re-derive it** and do not treat it as settling row 4's *semantics* — a
  property can be mutation-weak while the implementation is correct, or the
  reverse. It enters my union already.
- **Neither sibling that ran had a gate in its own worktree**, so their
  gate-derived observations describe the lane tree rather than an isolated
  copy. Yours will not have that caveat. If your isolated run disagrees with
  `gate=PASS`, say so plainly — that disagreement would be a finding.

## Independence cost, stated again because it is real

You hold **both** the semantics scenario and the REQ-evaluator re-derivation.
Combining them reduces independence between the two. Weigh them separately in
your report and do not let a conclusion in one carry into the other.

## Budget

**2 executions**, the allocation returned by your predecessor's zero-execution
block under the counter's returns-to-shared rule. The delta inspector's 2 are
**reserved and untouchable** — do not borrow them. Instrument runs over
synthetic fixtures that invoke no compiler are uncharged.

---

# Campaign ledger — bound, reconciled, and owner-maintained

Your predecessor `s2-insp-semrec2` refused correctly at 0/2:
`campaign-ledger-unbound-and-unreconciled`. My brief quoted "4 spent" while the
counter file still read its dispatch-time value, and the only row ledger on
disk was **S30-1's**. My defect, and the same stale-figure class I had raised
with my own parent an hour earlier.

**Authoritative ledger:** `handoffs/T30-S30-2-CAMPAIGN-LEDGER.md` sha256
`0a196a6be7ecaf15f069eb0df439ad7d8f2d0ed7cbbaed40524b7b0ebc1b6248`. It carries
the reconciled counter (**8 total, 4 spent, 4 remaining, of which 2 are the
delta reservation and 2 fund you**), the **S30-2 row ledger** — the seven rows
above, all currently `UNASSESSED` except mutation adequacy on row 4 — and the
open **CTRL-1** finding.

I checked this brief against that ledger mechanically before dispatch
(`instruments/check-brief-ledger.sh`); it is the check whose absence produced
your predecessor's refusal.

**Do not write that ledger.** A sibling appended its own spend entries to it in
good faith and another did not, which is how it ended up half-maintained. I
update it at every transition; you read it.

## Row states you are inheriting, not re-deriving

All seven rows are `UNASSESSED` — three seats died before touching them. Row 4
`INV-S30-2-PERM-NEVER-TALLIES` has a **failed mutation-adequacy result**
(CTRL-1) but its **semantics are still unassessed**; those are different
questions and the ledger records them separately.
