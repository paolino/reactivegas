# #92 successor — schedule preflighted at zero spend; returned with an exact gap

Owner `%503`. `%615` preflighted the complete schedule **before the first product
command**, exactly as its contract required, and returned the gap rather than
weakening the subject. **No product, Lake, Lean, build, test, gate or fixture
command has run in this campaign.** Frozen input manifest verified in full;
`REJECTED-SUBMISSION-2/` and `PRIOR-EXECUTION-EVIDENCE/` never entered as working
directories.

Schedule: `i92-successor-owner/handoffs/SCHEDULE-PREFLIGHT.md`.

## The finding that cost nothing and matters most

**The evaluated-data route exists.** Static inspection of the pinned toolchain
establishes that Lake 4.25.0 exposes evaluated configuration through
`Lake.loadWorkspace`, `Package.leanLibs`, `LeanLib.roots`, `Package.leanExes` and
`LeanExe.root`.

So binding requirement 1 is **satisfiable**: the repair has an evaluated-data route
and **needs no lakefile text parser**. The defect that sank both prior submissions
is fixable at the mechanism, not merely patchable.

## Submission 1 alone consumes the entire author cap

| unit | condition | required result |
|---|---|---|
| **A1** | C1 + the rejected checker + one legal root spelling the parser omits | **RED before code** — the required pre-repair observation |
| **A2** | C1 + repaired checker + **all three** legitimate declarations: dotted default `lean_lib Extra.Probe`, explicit `roots := #[...]`, and the array opener on the following line | GREEN, each proving actual build and coverage |
| **A3** | A2 checker + only the retained mirror import-omission mutation | RED naming the omitted module |
| **A4** | A2 checker + only the retained bypass mutation | RED at the outer receipt guard, never a false pass |
| **A5** | A2 checker + only the retained invalid-import mutation | RED with the offending import identity |
| **A6** | exact quality base `efef604d` + repaired checker only | GREEN, original census and full nominal theorem inventory |
| **A7** | + only a theorem-inventory truncation mutation | RED from the independently enumerated compiled set |
| **A8** | targeted bare compiled driver, built-project-module-minus-source condition | cannot use the mandatory wrapper |

**8 of 8 author units.** Each is separately attributable: A3, A4 and A5 are
distinct failure classes; A7 and A8 test **different set relations**; A1 is
RED-before-code.

No-repair success branch: **11/14 total, 8/8 author-side.** Inspectors may spend
zero, but the schedule does not assume it.

## The repair branch is unfunded

Any adjudicated batch that changes `scripts/check-lean-mirrors` **invalidates the
checker-dependent evidence**, and the loaded commit-owner contract additionally
requires a newly permanent property demonstrated RED before GREEN. Minimum honest
second submission: **R1** new property RED, **R2–R6** the three-root fixture and the
omission, bypass, invalid-import and base-identical controls **re-established on
the new checker bytes**, **D1** delta, **F1** final gate.

`A1–A8 (8) + I1–I2 (2) + R1–R6 (6) + D1 (1) + F1 (1)` = **18 total**, and
`A1–A8 + R1–R6` = **14 author-side**.

| | needed | granted | shortfall |
|---|---|---|---|
| global | 18 | 14 | **4** |
| author-side | 14 | 8 | **6** |

## What it explicitly refused to do

It named the four narrowings available and declined all of them, correctly:
dropping A1 violates RED-before-code; treating nominal theorem counts as an
independent inventory control **erases the shared-filter limit** the contract
requires it to address; combining A7 with A8 creates a two-condition mutant; and
accepting unchanged old controls after a checker-byte repair **repeats the rejected
campaign's evidence error**.

An evidence-only adjudication changing no checker byte *would* fit — but that is
not a funded checker-repair batch and cannot be assumed during a complete
preflight.

## The ruling needed

Either **fund the complete two-submission branch** — ceiling at least **18**, author
cap at least **14** — or **explicitly re-cut the process or subject**, naming which
second-submission evidence is no longer required. **The successor owner does not
choose that weakening, and neither do I.**

**No product execution starts under the current ceiling.** #92 and #66 remain open;
no merge is granted. **S3's ledger at 22 is untouched**, and `%611` is executing
normally against it.
