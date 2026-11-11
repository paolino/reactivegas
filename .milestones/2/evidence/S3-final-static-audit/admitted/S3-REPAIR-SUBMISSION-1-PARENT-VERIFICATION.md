# S3 repair submission 1 — parent verification

Owner `%503`. Static; I ran no project code. Verified at source, not accepted on
the owner's summary.

## Mechanically confirmed

| claim | result |
|---|---|
| `handoffs/MANIFEST.sha256` | **6/6 OK, no self-entry** |
| `OPMAP-v8` line count | **207** |
| verdict distribution | **exact**: KILL 68, ELAB-STATIC 60, OPEN-KILL 31, OBSERVED 31, RECOVERED 9, PREDICTED-SURVIVE 3, WITHDRAWN-DUPLICATE 2, STATIC 2, ACCEPT 1 = 207 |
| effective requirement set | **158** distinct tokens excluding the 2 withdrawn — the correct authored count |
| phantom identities | **withdrawn in place, not deleted** — both `KelGroups.baseHook_votes` and `KelGroups.base_change_recomputes_votes` are `WITHDRAWN-DUPLICATE` citing "phantom `KelGroups.*` namespace; sole declaration at `Reactivegas/Invariants.lean`". History preserved. |
| `OP-62` citation | **repaired** — now names `KelGroups.tryEnactDetailed_enactment_threshold_met` with its consumption site, replacing "upstream threshold lemma" |
| observed-kill claims | **zero** — no `OBSERVED-RED`, no "actually killed", no "executed kill" anywhere in 207 rows |

The 51 (a) rows, 43 GREEN ELAB rows and 31 OPEN rows — the 94 I handed off plus
the OPEN extents — were all assessed, producing 3 `PREDICTED-SURVIVE` rows that
**withdraw former (a) kills**, each pointing at its assessment line. That is the
honest outcome of actually doing the review.

## One defect: notation is inconsistent where precision was demanded

Of the 68 KILL rows, **49 say `PREDICTED-RED` and 19 say a bare `RED`** with no
prefix anywhere in the row — `conservation_preserved`, `step_accept_inv`,
`step_refuse_inv`, `step_correct_inv`, `step_close_inv`, `step_fail_inv`,
`step_open_inv`, `step_deposit_inv`, `step_withdraw_inv`,
`step_transferCassa_inv`, `step_backdonate_inv`,
`effectedState_preserves_qid`, `effectedState_tally_growth`,
`membership_growth_is_direct_admission`, both `app_members_preservation_holds`,
`comune_cannot_authorize`, `productionWellFormed_holds`, `questions_partition`.

**Stated at its true strength**, because I nearly overstated it: the owner's
claim of "zero un-prefixed **observed**-RED" is **literally true** — nothing
claims an observed kill. The substantive requirement is met. What is wrong is
that one status is written two ways in one file, in exactly the place where
predicted-versus-observed precision was the point. **Notation defect, not a false
claim.** It goes to submission 2.

## Cost model — the definitional repair I asked for, delivered

`COST-MODEL.md` **defines every counted unit** (`U-COLD`, `U-CHAIN`, `U-CHECK`,
`U-REPLAY`, `U-RESTORE`, `U-OP-EXEC`), marks each **MEASURED or UNMEASURED**
against retained receipts, and binds the one measured anchor to actual bytes:
`R-BUILD2` with its exact one-line guard diff, per-module timings, and the exact
RED at `Invariants.lean:407`. Only `C-STEP` is anchored; the other eight chains
are **UNMEASURED** and say so.

It states in its own words that no new measured cost is claimed and that a
statically repaired formula is not a measurement, keeps the R-BUILD2 isolation
gap (`LEAN_PATH` naming the candidate worktree) open as F-07, and adds the
restraint that R-BUILD2's RED is **mechanism corroboration for OP-22-class rows,
not a receipt for any OPMAP mutant atom**. The multiplier prose is withdrawn.

## The measurement request is honest and I cannot grant it

`MEASUREMENT-REQUEST.md` is **one** frozen request with an exact prerequisite (a
**separate** scratch checkout of `3590c001`, the repair worktree never touched,
porcelain verified before and after each cycle), exact commands, and kinds
**reported separately, never averaged**.

It says plainly that its cost **exceeds** the existing ceiling and requires
explicit numeric authorization — it did not silently fit itself inside the
budget. That is the right behaviour.

**It asks for Lean builds, which this commission forbids absolutely.** I have no
authority to grant it and have escalated it unchanged. Dependent costing stays
stopped.

## Disposition

Submission 1 is **verified as returned** — not accepted. Acceptance is the fresh
independent static auditor's to inform and the desk's to decide. One submission
of at most two remains. Spend unchanged at 5 substantive / 3 targeted.

---

## CORRECTION (append-only) — two of my gradings above were too generous

### 1. "zero un-prefixed observed-RED" is a FALSE row-level claim, not a notation defect

I graded it "literally true" by reading `observed-RED` as a compound token that
appears nowhere. That reading was too charitable. **A bare `RED` in an exported
row is an un-prefixed RED claim**, and the exported artifact is what a reader
consumes. **Sidecar global disclaimers do not make a row-level exported claim
sound** — that principle is the whole point of the predicted-versus-observed
correction, and I applied its opposite.

18-19 KILL rows export a bare `RED` while 49 export `PREDICTED-RED`. The claim in
the submission event line is therefore **false on the frozen bytes**. Not a
notation inconsistency — a false claim, and it goes to submission 2 as such.

Per row, submission 2 must distinguish four things and never blur them: **actual
receipts**, **predicted theorem failure**, **predicted proof-script failure**, and
**static cascade**. Historical observations are retained only at their actual
input identity.

### 2. I forwarded the measurement request without checking its own arithmetic

I verified it stated its excess and called it honest. I did not check whether its
numbers are internally consistent. They are not:

- it opens with **7** unmeasured chains and then enumerates **8**;
- 8 cycles × (mutant build + restoration rerun) = **16**, plus one cold build and
  one check elaboration = **18** timed invocations;
- it proposes a ceiling of **12**.

**Its own proposal cannot contain its own instructions.** I ran exactly this
arithmetic check against the S3 envelope in Addendum 1 and failed to run it here.

It is also not yet reviewable: exact diffs are to be frozen *at authorization*
rather than now, the commands say "run timed incremental build" rather than argv,
and the isolated `decide` target is unnamed. **Exact argv, cwd, prerequisites,
actual one-atom diffs and distinguishing observables must come BEFORE asking for
execution.** Existing anchors may justify a narrower *representative* measurement
only if it still establishes the required costing with its limits stated —
**do not sample away an unmeasured required class**, and preserve the classes
rather than calling them equivalents.

### 3. Two substantive row defects I verified at source

**`canonical_economy_holds` IS killable — `ELAB-GREEN-43` and `OPEN-EXTENTS-31 #6`
are wrong.** `checkCanonicalEconomy` (`Invariants.lean:1959`+) seeds
`conti := [(comuneId, 100)]` only, calls production `stepEvent … (.backdonate
"alice" 1)` with `fun _ _ => true`, and demands `alice == 1 && bob == 1 &&
comuneBal == 98`. `Step.lean:91-92` supplies exactly
`members.foldl (fun acc u => bump acc u w) (bump s.conti comuneId (-(n*w)))` —
the distribution that the existing **OP-23** atom proposes dropping. Replacing
just the distribution with the retained comune debit is a well-typed shape and
leaves both members at 0, so the member equalities are false and the `decide`
fails. **Static semantic argument, not an executed kill, and not a blanket
closure of the other OPEN rows.**

The reasoning error to fix is general: "checks aren't downstream" conflates
*reachability under an already-failing build* with *definitional insensitivity*.
An earlier proof failing can prevent reaching the check — that is an
execution-order and isolation issue, **not** proof that the definition is
insensitive. Audit every other `NO-MUTANT` justification with that distinction.

**`A-ASSESSMENT`'s conservation-across-all-14 assertion is wrong for OP-11.**
`Step.lean:55` is `pure { s with collections := { col with permitted := true } ::
rest }` — it touches **only** `collections` and **no money field**. Dropping
`permitted := true` cannot change conservation. A later proof failure arriving
through an inversion dependency is not the conservation proposition becoming
false. This is the same shape as the already-corrected `credit_pledges_step` /
OP-25 mistake.

### 4. My scratchpad placement error, fixed

The hung-parser receipt lived only in my session-private scratchpad, so the
artifact was not durable and the desk could not find it. Promoted to
`handoffs/s3-hung-parser-receipt/` — `argv.txt` (the full 2506-byte cmdline
captured before any signal), `ps-record.txt`, `README.md`, `MANIFEST.sha256`,
3/3 verifying.
