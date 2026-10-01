# Money custody economic core

`Reactivegas.Economic.Core` implements the first production slice of the new
KelGroups-based economy. It is a pure transition over two ordered account
lists and an opaque caller-owned frame. `Reactivegas.Economic.KelGroups` binds
its membership and admin queries to one read-only `GroupView`.

```haskell
stepInView
    :: GroupView
    -> State frame
    -> Key
    -> CustodyEvent
    -> Maybe (State frame)
```

The `Key` type is `Text`; the adapter does not normalize or reinterpret it.
The complete supported event surface is:

- `Deposit user amount`: requires an admin signer, a member user, distinct
  keys, and a non-negative amount; it increases both the user account and the
  signer's cash box.
- `Withdraw user amount`: requires an admin signer, a member user, distinct
  keys, sufficient user balance under the ordered first-match lookup, and a
  non-negative `comune` balance; it subtracts the amount from both accounts.
  The Lean guard permits zero and negative withdrawal amounts.
- `TransferCassa from amount`: requires distinct admin source and signer keys
  and a positive amount. It does not impose a source-affordability guard and
  preserves `conti`.
- `Donate amount`: requires an admin signer and a positive amount; it credits
  the signer's cash box and the reserved `comune` account, so it can cure a
  negative common-fund balance.

The result is `Nothing` for any failed economic guard. `Just` contains the
complete successor state. Every successful event preserves `untouched`
structurally; rejection never returns a modified success. Account updates
change only the first matching entry and append when absent, retaining duplicate
and zero entries.

## Verification boundary

The permanent `money-custody-tests` component exercises the pure transition
and the real `GroupView` adapter. Its direct witnesses cover every guard and
effect, Unicode identity, arbitrary-size integers, duplicate entries, and a
nonempty frame. Compiled query-fault controls cover all four event arms while
preserving each mutant's other guards and effects.

The frozen Lean corpus is replayed from each selected row's stored input. The
currently discovered selected extent is seven deposits, one refused withdrawal,
one donation, and no cash-box transfer. The transfer and a successful withdrawal
therefore rely on direct witnesses. These nine rows are partial, step-addressed
coverage: they are not a quota, a complete trace replay, proof of all economic
constructors, or evidence of a wasm build or full coordinator integration.
