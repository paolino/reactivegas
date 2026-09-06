# NOTE-035 — NEW authorized task: comment-only Reach-warrant repair. Third submission.

**Your prior `COMPLETE` stands.** This is a **new, explicitly authorized bounded
task**, not a resumption of your old mandate and not a re-opening of any verdict.
The terminal `AUDIT-FINDINGS` from the independent audit is preserved untouched.

## The ruling that is now your warrant

**RG-S4-REACH-20260906**, issued today — never describe it as pre-existing
authority. I have already recorded it in the **#66 issue body** (prior body
preserved verbatim, ruling appended, read back and verified; no comment posted,
no closing wording): https://github.com/paolino/reactivegas/issues/66

> An arbitrary-state decision procedure for `Reach view auth s` is **NOT-REQUIRED**
> for this milestone. The required executable observable is **validation of a
> supplied finite history**, with its initial-state and fixed view/auth premises
> established. Logical `Reach` premises remain legitimate proof inputs. This is not
> an undecidability claim and not an inference from absent callers or instances.

The finite-history correspondence stays **OPEN** as an owned **S5** obligation
(#75 replay, #71 reporting retained). It is **not waived** to let S4 land.
**No bridge is implemented here.**

## The change — comments only, two sites

1. `lean/Reactivegas/Mirrors.lean:29-32` — the `Reach` (P13) bullet currently says
   the oracle is "required **under the standing boundary**". Replace that warrant
   with a citation of **RG-S4-REACH-20260906**. Keep it accurate: the exemption
   rests on the ruling that arbitrary-state decision is NOT-REQUIRED, **not** on
   an undecidability claim and **not** on the absence of callers.
2. `scripts/check-lean-mirrors:152-153` — the doc comment calling the named
   exceptions legitimate, "P13 bounded, no oracle required". Same correction: cite
   the ruling as the warrant.

**Nothing else may change.** No executable token, no `s4bExceptions` membership,
no proof, no statement, no import, no nonce or check wiring, no other candidate
behaviour. If a correction seems to need any of those, **stop and return the
question** — do not take it.

## Verification you owe

- `git diff` showing **only comment lines** changed in exactly those two files.
- Then **ONE** substantive operation, and only one:
  `nix develop --quiet -c just ci` — the full local CI, retained to a **unique**
  receipt path. Cumulative owner ceiling is now **19**; historical 18 substantive
  and 52 targeted remain spent. **No additional targeted operations and no retry
  reserve.**
- I verified the fit before authorizing: the identical recipe already passed at
  this exact candidate (`S2-O6.log` ends `Build completed successfully (42 jobs)`,
  both corpus files `OK`, `corpus-check … live-bound`, zero error lines), and a
  comment-only change cannot touch `build`, `format-check` or `hlint`.
- **If it fails at a step unrelated to your change, that is a SETUP FAILURE:
  report the actual blocker.** It still consumes the operation. Do not retry.

## Then

Commit the comment-only change and return the final SHA plus the CI receipt.
Preserve all prior text, history, receipts and counters. No push, no PR, no merge,
no comment, no `#66` closure, no author contact outward.

A fresh independent full static audit follows at your final SHA; it will establish
independently that **only comments changed since `94bb7bb`** and that they cannot
alter the checker's generated code or any source-sensitive behaviour. Make that
easy to verify.
