# SUBMISSION 3 — EXCEPTIONAL (comment-only Reach-warrant repair; authorized NOTE-035/NOTE-062)

**This is an exceptional third submission**, explicitly authorized (NOTE-062).
Historical submissions 1 and 2 remain SPENT as numbered and are not relabelled.
Zero further rounds. Packet `handoffs/SUBMISSION-2.md` verified unchanged
(sha `22cb0473…`, checked at packaging time) and is not overwritten by this file.

**Candidate:** `04eb6c7d9aeb2a3602fca5ece14cbc033221cb43` (committed, clean tree
verified post-run; parent `94bb7bb`, the submission-2 candidate).
**Base:** `3590c001` (accepted, unchanged).
**Warrant:** ruling RG-S4-REACH-20260906, issued TODAY (recorded by the desk in
the #66 body; prior body preserved verbatim; no comment posted, no closing
wording) — never described as pre-existing authority.

## Change (comments only, verified)

`git diff 94bb7bb..04eb6c7` shows exactly two files, +6/-4, every changed line
inside a comment block:

- `lean/Reactivegas/Mirrors.lean:29-32` — the P13 bullet's exemption warrant
  replaced: rests on ruling RG-S4-REACH-20260906 (arbitrary-state decision
  NOT-REQUIRED; required observable is finite-history validation, owned as S5),
  explicitly not an undecidability claim and not inferred from absent callers.
- `scripts/check-lean-mirrors:152-153` — the exceptions doc cites the same
  ruling with the same accuracy.

Nothing else changed: no executable token (verified: all hunks are `*` doc
lines and `/--` doc lines), no `s4bExceptions` membership change, no proof,
statement, import, nonce or check wiring, no other behaviour. Had any of those
been needed, the instruction was to stop and return the question — it was not.

## What is expressly NOT done here

- Finite-history correspondence stays OPEN as owned S5 (#75 replay, #71
  reporting retained); not waived to let S4 land. No bridge implemented.
- No push, PR, merge, comment, or #66 closure.

## Evidence (unique receipt paths, none reused)

- Full CI receipt: `handoffs/evidence/S2-CI-comment-only.log`
  (sha `b6a16cfe…`) — cold-path `nix develop --quiet -c just ci` exit 0:
  builds green (27 + 42 jobs), checker green (19 rows, 4 exceptions, promoted
  2, discovered 24, tracked 29, fresh-nonce receipt + assertion), corpus files
  OK, zero error lines.
- Prior receipts retained untouched (submission-1 logs, S2-O*/OT*/SH-* logs,
  both O1retry2 logs under unique names, preflight driver text).

## Spend (final, at caps)

- Substantive 19/19 (this CI run was the 19th and last authorized operation).
- Targeted 52/60 (zero new authority used or needed).
- Submissions: 1 and 2 spent as numbered; this exceptional submission 3
  delivered; zero further rounds exist.

## Handoff request

Fresh independent full static audit at `04eb6c7` to establish that ONLY comments
changed since `94bb7bb` and that they cannot alter generated code or
source-sensitive behaviour. Auditor dispatch is the owner's action.
