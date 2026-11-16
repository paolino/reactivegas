# Addendum to auditor-s2 packet (brief sha256 26e3f2c8…) — parent corrections

The brief is frozen and stays frozen; this addendum corrects two parent
label errors for the record and acceptance. The auditor's own evidence is
preserved verbatim below and is the authority.

## 1. Profile deviation (desk NOTE-010, authorized for this seat only)

Desk offered Grok4.6/xhigh; the ticket owner pinned `-m grok-4.6
--reasoning-effort high` (argv verified live via /proc + TUI footer).
Desk accepts `high` for auditor-s2 as a profile amendment: identity
explicit and observed, family eligible, scope complete. This is recorded
as a deviation, NOT called xhigh, creates no precedent for future silent
profile choices, and required no seat restart or repeated build.

## 2. Swapped data/tasks hash labels (caught by the auditor)

Brief §"Submission under audit" (and the ticket START echo quoting it)
states `data cdb2a133, tasks 45c112c8`. The assignment is SWAPPED. Correct
labels, verified against `git hash-object` output order and the auditor's
independent hashing:
- spec.md: 988b2ab4 (as stated — correct)
- data-model.md: 45c112c8
- tasks.md: cdb2a133
Artifacts match the brief's hashes when assigned to the correct files;
no artifact is wrong, only the parent's labels. Root cause: misread of
ordered hash output by the ticket owner. Auditor evidence (own journal,
FROZEN-INPUTS 11:04:45Z): "spec blob 988b2ab4; data blob 45c112c8; tasks
blob cdb2a133 (brief swapped data/tasks labels — artifacts match when
assigned to correct files)".

## 3. Rebased RED ancestry (caught by the auditor)

Brief §"Submission under audit" says "RED lineage 2aabe20". Post-rebase
that is stale: submission-2 RED is 3c39014 (replayed onto the 4a6cd87
lineage with zero conflicts); 2aabe20 is NOT an ancestor post-rebase.
Auditor evidence (same FROZEN-INPUTS line): "RED lineage rebased to
3c39014 (2aabe20 not ancestor post-rebase)". Pre-rebase GREEN e9db9a1
stands rejected as stated.

## Binding unchanged

Full original+corrected scope, fresh cold compiled provenance, and the
independent just-ci rerun remain binding on auditor-s2. No verdict,
requirement, fence, or budget is altered by these label corrections.
