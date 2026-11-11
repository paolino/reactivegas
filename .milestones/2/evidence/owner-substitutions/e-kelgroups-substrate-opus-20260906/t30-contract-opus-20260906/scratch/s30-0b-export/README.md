# kelgroups

Polymorphic Haskell library for managing groups via a KERI hash-chained
Key Event Log (KEL). Generic over application event types — the base
system provides group infrastructure while applications supply
domain-specific semantics.

## Features

- **KERI event format** — events are KERI inception/interaction events
  with group events as JSON anchors, serialized via
  [keri-hs](https://github.com/paolino/keri-hs)
- **Server identity** — server generates its own Ed25519 keypair on
  first start; L1 event 0 is the server inception, establishing the
  group identifier (SAID)
- **Hash-chained storage** — every event carries `priorDigest`, forming
  a tamper-evident chain backed by SQLite
- **Ed25519 signatures** — all submissions are signed and verified
  against CESR-encoded public keys
- **Admin majority voting** — proposals require majority approval;
  single-admin proposals are enacted immediately
- **Bootstrap mode** — passphrase-gated first admin introduction
- **Stale-tip detection** — concurrent submissions rejected with 409
  when `priorDigest` doesn't match the current chain tip
- **SSE streaming** — real-time event notifications via Server-Sent Events
- **PureScript client** — browser client library using
  [keri-purs](https://github.com/paolino/keri-purs) with a Halogen
  reference UI
- **Lean 4 proofs** — 9 proof files covering invariants, validation,
  transitions, and KEL append; generic KERI types imported from
  [keri-lean](https://github.com/paolino/keri-lean)

## Components

| Component | Description |
|---|---|
| `lib/` | Haskell library (9 modules): types, fold, validate, store, server |
| `app/` | `kelgroups-server` executable (WAI/Warp + SQLite + SSE) |
| `test/` | 87 tests: QuickCheck properties, integration, multi-client E2E |
| `client/kelgroups-client/` | PureScript client library (API, codec, fold, state) |
| `client/kelgroups-trivial/` | Halogen reference UI |
| `lean/` | Lean 4 formal proofs (9 files, 17 build jobs) |

## Documentation

- [Design document](https://paolino.github.io/kelgroups/design/)
- [Implementation plan](https://paolino.github.io/kelgroups/implementation/)
- [Verification properties](https://paolino.github.io/kelgroups/properties/)
- [Roadmap](https://paolino.github.io/kelgroups/roadmap/)

## Quick start

```bash
nix develop -c just ci                          # format + lint + build + test + lean + client
nix develop -c just serve                       # run server on port 8080
nix develop -c just serve 10001 my.db secret    # custom port, db, passphrase
```

## License

Apache-2.0
