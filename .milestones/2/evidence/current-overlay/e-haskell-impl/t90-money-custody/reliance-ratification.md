# #90 reliance ratification

Input: commit-owner/reliance.md,
SHA256 718d1eb4feb22aa933c8e84027b5c7837aac2dcc8890c3c319d312f3d6fc05a9.
Parent accepts the following scoped reliance declarations; no new audit of
the entire upstream substrate is commissioned and no budget is added.

| Declared row | Disposition / existing mandate |
|---|---|
| INV-90-GROUPVIEW-CANON | Ratify reliance on the accepted GroupView readers at the pin; INV90-QUERIES/IDENTITY. The phrase about no second store is scoped to this adapter/core boundary; upstream whole-repository correctness remains accepted substrate reliance. |
| INV-90-SUBSTRATE-PIN | Ratify; INV90-WIRING/QUERIES. The actual compiled source identity still needs candidate evidence; a tag/hash declaration alone does not establish which source compiled. |
| INV-90-KERIHS-PIN | Ratify; INV90-WIRING. Verify source-resolution identity with the actual build. |
| INV-90-LEAN-AUTHORITY | Ratify; INV90-SCOPE and selected-arm rows. Gate byte identity is a provenance check, not semantic proof. |
| INV-90-CORPUS-FROZEN | Ratify; INV90-REPLAY/SCOPE. Retain existing emitter verification and stored-input replay. |
| INV-90-LEGACY-UNTOUCHED | Ratify advisory reliance with stated PARTIAL limit. Existing mandatory CI and no legacy-source edits remain binding. |
| INV-90-INDEX-RESOLUTION | Ratify advisory unverified assumption, enforced NONE before build. A resolution failure is an environment result, not semantic RED. Do not pre-claim compatibility. |
| INV-90-TOOLCHAIN | Ratify advisory assumption with declared PARTIAL limit; candidate build evidence remains required. |

The prospective A2/A4 enforcement references are intended checks, not completed
receipts. The original INV90 blocking acceptance matrix, scope, gate and
signature contracts remain unchanged. No extra semantic rows or execution
attempts have been introduced by this reliance ratification.
