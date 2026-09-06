# kelgroups

A polymorphic Haskell library for **KEL-based group management**.

kelgroups provides the infrastructure layer for managing groups via a Key Event Log (KEL) — an append-only, hash-chained, signed event log. The library is generic over application event types: the base system handles members, roles, voting, and bootstrap, while applications supply domain-specific semantics.

## Packages

| Package | Language | Role |
|---|---|---|
| `kelgroups` | Haskell | Polymorphic base system library |
| `kelgroups-server` | Haskell | Server parameterized by application plugin |
| `kelgroups-client` | PureScript | Client-side KEL handling, API, and state |
| `kelgroups-trivial` | PureScript | Halogen reference UI |

## Dependencies

| Dependency | Language | Provides |
|---|---|---|
| [keri-hs](https://github.com/paolino/keri-hs) | Haskell | KERI events, CESR encoding, Ed25519 crypto, KEL primitives |
| [keri-purs](https://github.com/paolino/keri-purs) | PureScript | KERI events, CESR encoding, Ed25519 crypto, KEL replay |
| [keri-lean](https://github.com/paolino/keri-lean) | Lean 4 | Generic KERI types (`Digest`, `SAID`, `Key`, `KELEvent`, `hashChainValid`) |

## Documentation

- [Design Document](design.md) — system invariants, base events, bootstrap mode, architecture
- [Implementation Plan](implementation.md) — modules, types, store, Lean proofs, QuickCheck properties
- [Properties Catalog](properties.md) — Lean theorems and QuickCheck properties cross-reference
