# Money custody — #90

Authority: [issue 90](https://github.com/paolino/reactivegas/issues/90),
Lean `efef604de87b2a1efae51e84d1a9150e585c1db0`, Step.lean/State.lean;
KelGroups `933e385df2f2a251bb54a08bb7663f0d41fafb64`, Types.hs.
Ceiling: this artifact 100 lines / 8000 bytes; every invariant BLOCKING.

An application integrating the first economic core can deposit, withdraw,
transfer between cashiers and donate against the accepted canonical GroupView.
Each selected arm agrees with the pinned Lean transition, including refusal
and preservation of all state it does not change. Later integration extends
this core. This slice neither replaces the legacy server nor implements the
other economic or voting arms.

| ID | Observable acceptance |
|---|---|
| INV90-IDENTITY | Keys are lossless Text, identical to the accepted Haskell substrate key representation. No nickname, numeric narrowing, normalization or second membership/role store. |
| INV90-QUERIES | All seven query occurrences below use the read-only canonical view adapter; no inferred subset or independently reconstructed role lists. |
| INV90-DEPOSIT | Exactly the pinned deposit guard and effects, including zero amount and nonmember/nonadmin/self/negative refusals. |
| INV90-WITHDRAW | Exactly the pinned withdrawal guard and effects: successful withdrawal plus every guard/refusal and balance effect, including zero/negative amounts under the actual Lean guard. |
| INV90-TRANSFER | Exactly the pinned transfer guard and effects, directly exercised because the corpus contains no transfer step. No invented source affordability restriction. |
| INV90-DONATE | Exactly the pinned donation guard and effects, including its ability to cure a negative comune account. |
| INV90-FRAME | Every untouched field survives structurally unchanged, including nonempty collections/votes and opaque future payload fields; transfer also preserves conti. Rejection never returns a modified success. |
| INV90-REPLAY | Discover and replay all covered steps independently from their stored input. Require nonzero extent per represented selected constructor; compare complete applied state and refusal guard identity at the boundary below. |
| INV90-CONTROLS | All four arms have compiled guard-mutant evidence through real transitions and the same behavioral oracle, with an actual distinguishing witness. A setup failure or unreachable mutation is never a kill. |
| INV90-WIRING | New production core and real GroupView adapter compile through Cabal/Nix; permanent tests execute in committed CI; pure package has only minimal dependencies. |
| INV90-SCOPE | No unsupported event is accepted as a successful no-op; no duplicate production economy, legacy server rewrite, Lean/corpus mutation or claim of full-core/wasm acceptance. |

Query inventory from the selected Step.lean arms (isResponsabile is isAdmin):

| Arm | Canonical queries |
|---|---|
| deposit | isAdmin signer; isMember user |
| withdraw | isAdmin signer; isMember user |
| transferCassa | isAdmin signer; isAdmin from |
| donate | isAdmin signer |

The remaining guards and complete update semantics are the pinned Lean arms,
not a new product policy. Money is arbitrary-precision Integer. The ordered
balance association lists retain Lean first-match lookup/update and append
behavior, including duplicate/zero entries; do not silently normalize them.
`comune` remains the reserved account key within conti.

The comparison boundary is the JSON corpus's economic Event result. A selected
event returns the complete stored applied state, or refused with the same event
guard id/declaration. The Option-shaped core has one refusal, not invented
per-guard reasons. Unsupported corpus constructors are explicitly outside the
partial replay, never stepped as successful no-ops. A closed four-constructor
production event type makes unsupported operations unrepresentable here.

Observed pin extent, zero-based (discovery remains authoritative): deposit at
(0,0), (0,1), (1,0), (2,0), (3,0), (4,0), (4,1); donate (0,6); withdraw (4,6),
refused. transferCassa has zero steps. Nine is an observed extent, not a quota,
complete trace replay or complete conformance. Preserve and test nonempty
frames even where these nine rows do not carry every frame value.

For every negative control, name the altered guard, prove the mutation affects
the actual transition, identify the witness and require the same assertion
that accepts the real transition to reject the mutant. Guard-fault injection
at the declared query boundary must retain every other guard and effect;
test-side fake output or a duplicated economic implementation is insufficient.
If the frozen corpus has no distinguishing step, say so and supply a direct
witness. Do not claim the common suggested deposit positivity mutant is
distinguished by a positive corpus amount.

The pure package's base/text (and containers only if needed) choice supports a
future wasm port; it is not a wasm build. #82, #83, #84, #73 acceptance and
#67 D2–D5 outcomes remain unchanged. No merge or deployment in this ticket.
