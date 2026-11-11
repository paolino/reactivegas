# AMENDMENT 1 to the supplement brief — input addition and one corrected criterion

Issued by `%503` at the timing stated below. **This amends your inputs and one
acceptance criterion. It does not tell you what verdict to return, and it does
not restart, extend or narrow your mandate.**

## Actual timing — stated, not backdated

| event | UTC |
|---|---|
| your `START` | 2026-09-06T07:18:19Z |
| the correction below was made | **after** your START |
| this amendment issued | see its `status-event` line in `%503`'s `STATUS.md` |

The v1 map you verified in `INPUTS-MANIFEST.sha256` was frozen **before** your
START and is preserved unchanged. Nothing in `inputs/` has been altered. The
corrected map arrives as a **separate, separately manifested** directory.

## What was wrong, and it was `%503`'s error

The FS-02 repair added the required ruling entry correctly, but `%503` left the
map's **authoritative "Where the work actually stands" section and its Current
owed list stale**: they still named `master` as `4a6cd87`, S2 as in-flight on
submission 3 with an undispatched successor, S3 Phase 1 as not dispatched, S4 as
not started, and announced "two" S5 obligations immediately above a list of
three. That opening section declares that it **wins over everything below it**,
so it is current guidance, not protected historical text. Leaving it stale to
preserve additivity was the wrong trade.

A second error: the sentence *"Integrated `apply` histories therefore fall
outside the predicate"* was an **unqualified overgeneralization**. Histories with
view changes, arbitrary genesis or refusals do not **unconditionally** fail this
fixed-premise `Reach` relation; some integrated histories may still preserve the
required premises, and the absence of a general correspondence is not proof that
every history is outside it. No bridge proof is claimed or requested.

## Corrected criterion — supersedes the additive-only test in the brief

The brief's FS-02 section told you to *"confirm the change is additive apart from
the single amended bullet line"*. **That criterion is withdrawn.** Additivity is
not more important than correct current guidance.

**Test instead:**

1. The **historical section** below the divider is preserved — the prior omission
   and every superseded wording remain readable there.
2. The **authoritative current section** now states the **actual** current state:
   `master` = `3590c001…` (S2R landed, PR #88); S2 accepted and landed; S3
   terminal findings + executed SS-0 + the new static commission; S4 terminal
   FS-01/FS-02 + the later clean CI + this supplement; S5 open with **three**
   obligations.
3. Where current and historical text disagree, the current section says so
   explicitly rather than silently rewriting the old wording.
4. The ruling section's replay/`Reach` distinctions are stated **without** the
   unconditional claim.

Judge whether the corrected map actually meets 1–4 at source. If it does not,
that is a finding.

## Added inputs — `inputs-amendment-1/`, manifest `INPUTS-AMENDMENT-1-MANIFEST.sha256` (3 entries, no self-entry)

| file | what it is |
|---|---|
| `CLOSURE-MAP.v2-CURRENT.md` | the corrected authoritative map — **this is now the current map** |
| `CLOSURE-MAP.v1-fs02-only-preserved.md` | the FS-02-only state you were originally given, frozen |
| `NOTE-073-…md` | the desk note requiring this correction |

`inputs/CLOSURE-MAP.after-fs02.md` equals the v1 file above; it stays in your
original manifest as the state at your START.

## Unchanged

Zero project execution. One terminal verdict. No candidate edit. No author
contact. Local delivery only. No PASS is required of you, and no additional
audit, build or budget is granted by this amendment.
