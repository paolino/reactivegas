# Retained evidence inventory

Administrative provenance inventory only; no subject execution. SHA256 values measured at handback. Every retained regular file under this runtime root is listed except this inventory itself, whose self-hash cannot be included. Empty control directories contain no files. STATUS hash includes terminal COMPLETE. Detached checkout build outputs are retained separately and not inventoried as evidence.

| File | Bytes | SHA256 | What it shows |
|---|---:|---|---|
| STATUS.md | 4599 | c9d330a25bd1ef9aacab8621a160d013dba3299565e9fe28950cb8bf188838b2 | Append-only acknowledgement, completed-command receipts, partial verdict and stop state; hash includes terminal COMPLETE. |
| brief.md | 10908 | 90867a56a017fa2a0b1eb32d96238c96fe33a68e67827679239481358c5bdfd4 | Commissioning brief including amendment 1, full scope and budgets. |
| evidence/S1-cold-ci.log | 792684 | 32f82fb0bb4844adbaf738bd53c170c3536b7bb644a469abaf9cd81c3426086e | Cold project CI execution, exit 0, including Lean and corpus live binding. |
| evidence/S2-v16.log | 18153 | ebb228d65c37c73012b7f4e846142c8aa9a7a3973ec3ccd669df7d431ad9d79b | Frozen v16 execution, exit 0; internal controls and six named suite receipts. |
| evidence/T4-geometry.log | 836 | 79a7bf0556840cd25d9198f3d5544613744fb5b2de0b1e1ada3bac29cfd88d20 | Reachable counts 1..10; separation control at eight and overlap at ten; stable members, deterministic output, verified export. |
| evidence/T5-derive-bracket.log | 933 | 8cdb391765de370565d83c1ed64a6ae27205c986b5e1a6db1042596f4491b507 | Bracket-read added control: derivation GREEN, admitted exact key, click loses person navigation. |
| evidence/T6-derive-dot.log | 1005 | 378cb93b93cd1221085604574abd8220423576be41b6411cb00819551ef345fa | Dot-read calibration: derivation RED for unknown vip, handler still executable. |
| evidence/T7-authority.log | 277 | 4de61e7480942ed572e5ffbf90e42c9851df0e6d24796ae9ab81354cae4eef4d | Ordinary non-admin refusal; author override accepted and adopted as Anna purchase, signer still non-admin. |
| evidence/authority-forged-session.json | 5589 | a00067f9acea2473f4690356fdb9c840b2c4bf0a82e91f7d8fc6d4408c00c9fd | Complete retained session accepted by production replay/governance and adoption in T7. |
| evidence/derive-bracket.html | 338479 | 5faf6999a926d83533b7e1aefdfde8c8a106c4be20f734a4cc1e21e44b8d1835 | Auditor mutant consumed by T5; added bracket-notation key control. |
| evidence/derive-dot.html | 338476 | b29c9ab156b10179cd85bdc2054c745046b747b045e7831c3df7ec89a2313112 | Auditor calibration mutant consumed by T6; added dot-notation key control. |
| evidence/ledger-after-handback.md | 8397 | 6404fde2f7e3ad8fd0ba353db1cd4331ba51401839a26126314ecf2068d1ef96 | Ledger snapshot after append of all 17 auditor row dispositions. |
| evidence/ledger-before-handback.md | 4533 | c2e5628318bdbac950ef7d1401564f16d1ecf0fbdb77287b0ba32267dbd681b8 | Unchanged owner ledger before §B append; matches binding hash. |
| evidence/v16-suites/derive-only.log | 680 | 8b269a22153273fc0318b5d132b3550103819dcc4047ca64bf45ecc88757aef5 | Nested focused derivation, 14 required controls/22 read names, exit 0. |
| evidence/v16-suites/omit-K2-noop.log | 2548 | 569bbb8a1bee53d06d1064d71ad993202df3026609be8e29958f50d11498dd3e | Full neutralized-discard suite; env invocation retained, K-2 witness kept, exit 0. |
| evidence/v16-suites/omit-K2.log | 2506 | d4399515fcf788e78df4c312630c7a98a74ef67ed16f9994d90bc668bbd204f9 | Full omission suite, K-2 witness discarded; named ordinary coverage failure, exit 1. |
| evidence/v16-suites/ordinary.log | 2518 | ef90bf0b31e90965b69e7bbe72febe8481135543d283f0f075da3ca73287ade1 | Full ordinary UI suite, invocation header, 14 named witnesses, exit 0. |
| evidence/v16-suites/vocab-expect-red-clean.log | 357 | 87f3b618649e423d2fee5089b8a82db26a8c14b61f3a90b6c6f4caf4c69cad28 | Nested expected-word control on clean page, exit 1 because banned word absent. |
| evidence/v16-suites/vocab-only.log | 374 | 56cc0e0ef904c1acb12abc5da12ae9f73deefa5a8d59a2dbec5f94e64f7e09db | Nested clean vocabulary check, exit 0. |
| inbox/NOTE-A1-administrative-handback.md | 2646 | 8e0ac145cb927ed59a3dfb6b2acf3b48a675b6bdd3c96f185610afd223f6a437 | Administrative-only stop and handback authority. |
| launch-command.txt | 139 | 65e6a0b91d4aecc5737f9d404db8b8c7f55b8956a4352f60addf9b28811e8605 | Parent-authored Codex launch identity. |
| probe.mjs | 6840 | ce66e7dac7bd71b6adc66ef9d4fc5183cfea841d2cbe91c0cbcc2b7987901b7c | Final frozen probe source. geometry/derive-bracket/derive-dot/authority executed; chrome NEVER executed. Pre-run per-version hashes unavailable. |
| receipts.json | 2789 | 7a493657cc163ea651878fd080f1919b24d6d74510084f7e228b320a1f3d0a25 | Transcription of completed run-receipt metadata and actual budget; not a rerun. |
| report.md | 15430 | bfc2b8c2fe3da9e7b3c4100740df622e1c9e09fa359f731d52ea0d4ada2173fb | Terminal PARTIAL FINDINGS report: three findings, four failed rows, thirteen unjudged rows. |

Provenance comparison of existing files: ordinary and omit-K2-noop bodies after their three-line headers are byte-identical: true. Their invocation headers differ and their whole-file hashes differ. This is an artifact comparison, not another suite run.
