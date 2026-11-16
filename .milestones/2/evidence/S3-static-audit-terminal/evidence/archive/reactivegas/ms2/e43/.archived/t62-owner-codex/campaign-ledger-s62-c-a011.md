# S62-C A-011 fresh campaign ledger

This is a new campaign authorized by A-011, not submission 3 of A-010.

- Pre-slice base: `bf027aabb764a006604ad5e88b4780c42d264011`.
- Accepted S62-B candidate carried onto release:
  `fe2ca5129daf65d0313437496f87d26aef1babeb`.
- Released master ancestor:
  `d7a3e05116f40920f3d78daf3e1818ad17c74a74`.
- Build budget: `0/40` substantive builds.
- Audited submissions: `0/2`.
- Ceiling raises: `0/2`.
- No row may become `RESIDUAL`; all rows are `BLOCKING`.

| Row | Severity | Initial state | Bound evidence / obligation |
| --- | --- | --- | --- |
| `G62-C-THEOREMS` | BLOCKING | `KILLED` (inherited) | Historical declaration `ab9b4aadb52fbbcdb62bb8de39f62acbc76f0ffbfa4c8eeb5d1d79f6fff334f4`; static/axiom receipt `968fc50fbeb0c3f47759e3cdf2de2ff324c740b2c3ea199ee3342435c2b21a71`. Do not reopen or rebuild. |
| `G62-C-ECONOMY` | BLOCKING | `KILLED` (inherited) | Non-degenerate canonical economy focused proof `76c32a87d1099fe0d3fb3cbae84fa9e408afaa6e9d454a433e4e6c02999b5b21`. Do not reopen or rebuild. |
| `G62-C-EXHAUSTIVE` | BLOCKING | `KILLED` (inherited) | Constructor controls plus full-ticket receipt `90c28b3780d17c2970231b2f880dd1a6d558491a81d5064c854b394ac4947521`. Do not reopen or rebuild. |
| `G62-C-TRUST-CI` | BLOCKING | `KILLED` (released dependency) | Live pin/runtime and mismatch-control receipt `b6117b60dfe019f8dd528dbcfd77de5b2c0fa23135c0c3af37bb1ee4096d5d15`. Preserve exact release blobs. |
| `I57-01-BOUNDARY` | BLOCKING | `OPEN` | Keep signer identity constant, reach the duplicate arm, and permanently prove exactly one production validation decision dominating effects/sweep; both reached-duplicate and bypass mutants must fail. |
| `G62-C-INHERITED57` | BLOCKING | `OPEN` | Exercise franchise cast-admission and policy-free threshold threading through the integrated production path with self-applying targeted mutants. `I57-06-DISJOINT` remains inherited `KILLED`. |
| `G62-C-TRACE` | BLOCKING | `OPEN` | Serialize/deserialize signed sequential events and complete integrated state, replay through production, compare every stored value, and kill all-error, reordered, altered-state, same-length, and per-coordinate mutants. |

The A-010 terminal report and frozen seed instrument remain read-only inputs:

- report sha256 `384f5e7c7e06655f821091c73cf1d8818329e8bff5faec692d615c77b8dc8df2`;
- instrument sha256 `e586bc2cc11749c0653cbd4c3c2dadab893cfbe5a1341cab634af27bfdfec451`.

Starting seat counts before the A-011 attempt: Grok `4`, GLM `1`, Codex `6`,
Claude `3`, plus GLM provider-intake-without-START `1`. Every terminal receipt
repeats all five counts. The first authorized `glm --approve` attempt consumes
the epic one-shot allocation even if it fails before START.
