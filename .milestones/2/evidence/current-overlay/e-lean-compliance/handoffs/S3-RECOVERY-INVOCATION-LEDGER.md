# S3 recovery — invocation ledger, reconciled against the TEN shared ceiling

Owner `%503`. **Append-only correction under NOTE-082.** Both the original count
and the classification change are shown; nothing is rewritten.

## The rule I applied wrongly, and the one that governs

I ruled that the failed E2 attempt did not consume a slot because *"the slot is
defined by the stage."* **NOTE-082 rejects that reasoning explicitly**: multiple
real test/build invocations in one stage are still multiple invocations, and a
slot is never defined by its stage label.

The governing axis — in force before these attempts, not new — is **setup failure
versus real failure**. Applying it changes the arithmetic in both directions from
what I reported.

## The ledger

| stage | invocation | compiler work | classification | charged |
|---|---|---|---|---|
| E1 | cold product build at `efef604`, `m00-cold.exit=0` | yes, 27 jobs / 25 oleans | real | **1** |
| E2a | synthetic plumbing suite, attempt 1 | **none** — glob expanded against cwd before the assertions; zero product contact, scratch porcelain 0 | **setup failure** | 0 |
| E2b | synthetic plumbing suite, successful consolidated run | none (stubbed by design) | real | **1** |
| E3 | pilot mutant build, `m-C-VALIDATE.exit=1` | yes, Built 2 / Replayed 0 | real — the expected span-bound RED | **1** |
| E3n | `git apply`, `m-C-VALIDATE-apply.exit=0` | no — patch application | not a build/run | 0 |
| E4n | `git checkout`, `m-C-VALIDATE-restore-checkout.exit=0` | no — restore | not a build/run | 0 |
| E4 | restore rebuild, `m-C-VALIDATE-restore.exit=0` | yes, GREEN in 2193 ms | real | **1** |
| E5 | check elaboration, `m09-check.exit=0` | yes, 1730 ms, silent | real | **1** |
| E6 | `corpusExport` production, `m10-replay-prod.exit=1` | **yes — elaborated every module including `CorpusExport`, then died linking** | **setup failure**: ran outside the required Nix shell, so `cc` was absent | 0 |

**Original reported charge: 6 (E1–E6).**
**Corrected charge: 5.**

The two uncharged attempts are **retained in full**. E6 did real compiler work and
that work is logged honestly; it receives **no semantic and no successful-artifact
credit**, and the runtime layer remains unestablished.

Receipt-level reconciliation, including nested stages: **7 `.exit` receipts**, of
which **5 are compiler invocations** (`m00-cold`, `m-C-VALIDATE`,
`m-C-VALIDATE-restore`, `m09-check`, `m10-replay-prod`) and **2 are git stages**
(`apply`, `restore-checkout`) that are not build/run invocations. `m10` is the
uncharged one. **No other substantive invocation exists that would make this
arithmetic false**, and no extra allowance is invented.

## Against the ceiling

| | |
|---|---|
| charged so far | **5** |
| two initial blind inspections (E8, E9) | 2 |
| corrected E6R + E7 | 2 |
| reserved delta inspection | 1 |
| **total** | **10** |

It fits exactly. **No raise, no stage bundling, no waiver.**

## Why E6 failed, and what the corrected retry buys

The seat reported *"could not execute `cc` (no C compiler on PATH); environment
cause."* Right about the symptom, and right that the product is innocent — but the
cause is narrower and it is **ours**. Outside the dev shell there is no `cc` or
`gcc`; **inside `nix develop` both exist** at
`/nix/store/vr15iyyykg9zai6fpgvhcgyw7gckl78w-gcc-wrapper-14.3.0/bin`. E6 ran with
bare `lake build`, outside the shell.

E1–E5 passed because Lean elaboration needs no C compiler. **E6 is the first stage
that links**, so it is the first stage where the missing shell bites. This is an
instrument **transport** defect of exactly the class as the SS-0 v1 runner that
echoed its cwd and never `cd`-ed.

So a corrected E6R buys an actual artifact rather than another link failure — which
is why it is worth its slot.

## Sequencing — unchanged by the accounting

My **procedural** decision stands: **no pre-adjudication retry, and no edit to the
frozen submission while the inspections run.** The proper-shell invocation and the
runtime producer-plus-replay work **fold into the ONE adjudicated repair batch**,
alongside every other inspector finding and the complete original requirements.

My earlier decision to *decline* E6R is superseded on the accounting alone. Nothing
here pre-judges the inspectors' verdict, and the unfinished runtime layer is **not
accepted**.

If the repair work that actually emerges cannot fit, I return **the exact branch
still unexecuted** before spending anything.
