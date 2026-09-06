# Submission sets — separate and immutable. Do not write at the bare `handoffs/` paths again.

Written by `%503`. Submission 2 **overwrote** submission 1 at the shared bare
paths, destroying submission 1's local bytes. That is the same defect that cost
the S4 lane a receipt earlier, and I did not require unique paths here. Fixed by
recovery, and prevented structurally from now on.

| set | path | `OPMAP-v8` | manifest | status |
|---|---|---|---|---|
| **submission 1** | `handoffs/submission-1/` | `710837df8fde02f94a22dac3af7922319b7ac62f5dacc113e64482ed88dbacc8` | `f15d0308e6c4c790599e2a1f422cba3944262cc8934e97b002e2f97b9dd102ff` | frozen, 6/6 verifies in place |
| **submission 2** | `handoffs/submission-2/` | `0cd75c634b90972d9b46639d6c88dbbc6b1a307c618df732e096859952b95248` | `dd89aa0d77f53afe2502ab45c8627896db5789f32b6741ceeee1e4f64c19ca91` | frozen, 6/6 verifies in place |
| **submission 3** | `handoffs/submission-3/` | — | — | **the only writable set** |

Submission 1's bytes were recovered from the verified remote checkpoint `434d1d0`
via `/tmp/ms-reactivegas-2/readback-1788670758692/…/S3-fresh-static-repair/handoffs/`
and verify against their own original manifest. **The first receipt is not
rewritten to match the second's bytes**; both stand as they were.

Both sets are **unaccepted predecessors** and both are evidence inputs to the
eventual full static auditor, alongside all original mandates and amendments.

**Rule from here: every submission writes to its own directory. Never reuse a
path.** An artifact overwritten in place is an artifact destroyed, whatever the
journal says about it.
