# #90 data model

| ID | Type / fields | Invariant |
|---|---|---|
| D90-KEY | Key, a Text alias | Identical to accepted KelGroups key representation; arbitrary Unicode retained without conversion/normalization. |
| D90-EVENT | CustodyEvent: Deposit user amount; Withdraw user amount; TransferCassa from amount; Donate amount | Closed selected surface. Signer arrives separately. Amount is Integer. |
| D90-QUERIES | Queries: memberQuery :: Key -> Bool; adminQuery :: Key -> Bool | Immutable functions, no member/role data store. Production values derive from one canonical GroupView. |
| D90-STATE | State frame: conti :: [(Key,Integer)]; casse :: [(Key,Integer)]; untouched :: frame | `frame` carries all other application fields, including collections/votes. It is preserved for arbitrary values, without reconstruction. Account representation preserves pinned Lean list semantics. |
| D90-REFUSAL | Maybe (State frame) | Nothing is the one economic guard refusal; Just carries the complete successor. No silently accepted unsupported operation. |

The caller's opaque frame is the integration boundary, not a claim that all
fourteen Lean arms are closed over two account lists. Tests instantiate it
with nonempty collections/votes and extra fields; the production adapter
preserves it parametrically. The GroupView is read-only input and cannot be
returned or changed by this transition. Full integration with voting/base
changes remains outside the ticket.
