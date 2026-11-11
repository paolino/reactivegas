# Bound references — read these in place; do NOT copy or re-narrate them

Every path below is **read-only** and bound by the hash of its manifest. Reuse
these artifacts **by exact hash/reference**. Do not duplicate their prose into
your packet and do not re-run any experiment they record.

| what | absolute path | manifest sha256 | entries |
|---|---|---|---|
| S3 admitted author packet (submission 3) | `/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s3-final-static/admitted/` | `a88d8594a66de813b4740545919e857923ead5df9ef3dc62cb2a2e39a5315f64` | **49** |
| original manifest / tree | `/tmp/reactivegas/ms2/e-lean-compliance/commit-owner-s3-repair/admitted-supplement/AUDIT-MANIFEST.sha256` | `ac172dbfecad4447e105707fef8e3f6674712a3f5d735e9a1cf73497342a771e` | **323** |
| S3 final static audit evidence | `/tmp/reactivegas/ms2/e-lean-compliance/candidate-auditor-s3-final-static/handoffs/` | `65a6c29e1867ac84d848728892b405154cdb365a254fb99771ef9382afa457d5` | **30** |
| SS-0 v2 frozen instruments | `/tmp/reactivegas/ms2/e-lean-compliance/measure-owner-s3-ss0/instruments-v2/` | `467674ce5c39049df7233bf8de18317373fb015ab31dc2339b22d03bf6979235` | **7** |
| SS-0 raw run outputs | `/tmp/reactivegas/ms2/e-lean-compliance/measure-owner-s3-ss0/` | — | raw stdout/stderr per operation |
| SS-0 v1 failed attempt (preserved) | `/tmp/reactivegas/ms2/e-lean-compliance/measure-owner-s3-ss0/handoffs/SS0-RETURN.md` | — | the charged setup failure |
| product source, read-only | `3590c0015b84fd58004bf6fb44dd18b107304c48` (now `master`) | — | never modified, never built |

Key single-file hashes, verify them yourself:

```
3f7260b6799d752b9d756ac8bcf84dea460e6de55310949aa3af815c6c62f41e  S3-FINAL-STATIC-AUDIT-REPORT.md
d2aa91578f5b34e17c53cb5d66ea346b4f73a6eabc8dbaa19972640247eb8482  SUCCESSOR-RECOMMENDATION.md
80ed926d0233a7874e60cdafb7b93079ec9b66ff679272ab651176b38c842a87  ROW-REVIEW-207.md
53361301df584ed7e60ebc71c09d6d64a3a5d128165af224ab00daa38027cd94  HELPER-REVIEW-81.md
8bbbd922abe01786bf7f4edb2e2f5550d1f9b1baf499e106f737e93ccd1f82bf  REQUIREMENT-LEDGER.md
082ec23cc036ef841459b94c434ab3d3331d789fb2dfc540cca280ef04c63dbf  CAMPAIGN-LEDGER.md
37f59295df4bf2ad5a5731117796e10fb01879d72581076f4a26156472eeed10  ONWARD-DISCOVERIES.md
f5cc87116cd4149557100e75b97706fc57d68c0a8485f6518f332fc176520245  SS0-CONSOLIDATED-ASSESSMENT.md
12ffdfdb4e3c162a2e1236da69caf48dd564a156a420d11b7fcade91952d46b7  SS0-RETURN-v2.md
```
