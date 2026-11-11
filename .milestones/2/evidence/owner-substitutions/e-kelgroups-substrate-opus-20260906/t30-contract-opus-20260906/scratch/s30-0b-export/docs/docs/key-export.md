# Key Export/Import (JWK)

Ed25519 private keys can be exported and imported in the
standard JSON Web Key format
([RFC 7517](https://datatracker.ietf.org/doc/html/rfc7517)
with the OKP key type defined in
[RFC 8037](https://datatracker.ietf.org/doc/html/rfc8037)).
This enables backup, migration, and multi-device use of
private keys.

## Format

A private-key JWK for an Ed25519 key:

```json
{
  "kty": "OKP",
  "crv": "Ed25519",
  "x": "<base64url-encoded public key>",
  "d": "<base64url-encoded private key>"
}
```

Both members are unpadded base64url (RFC 7515) and hold
exactly 32 bytes.

## Server identity (Haskell)

The server Ed25519 identity lives in the SQLite
`server_identity` table. The `kelgroups-server`
executable provides two subcommands:

```sh
# Print the server private key as JWK JSON on stdout
kelgroups-server export-key kelgroups.db

# Install a JWK file as the identity of a fresh database
kelgroups-server import-key new.db key.jwk
```

`import-key` refuses databases that already contain an
identity or events: an imported key can never silently
replace the signer of an existing chain.

## Member keys (PureScript client)

The client package mirrors the codec in
`KelGroups.Client.Jwk`: `keyPairToJwk` exports a NaCl
signing key pair (the JWK `d` is the 32-byte seed, i.e.
the first half of the TweetNaCl secret key), and
`jwkToKeyPair` rebuilds a key pair from a seed with
full validation.

## Validation rules

Import rejects, with clear errors and without echoing
key material:

- wrong `kty` (anything but `OKP`) or wrong `crv`
  (anything but `Ed25519`)
- invalid or padded base64url, non-canonical trailing
  bits
- `x` or `d` that do not decode to exactly 32 bytes
- a public key that does not match the private key

## Encrypted export

Encrypted export (passphrase envelope) is tracked
separately in
[#10](https://github.com/paolino/kelgroups/issues/10).
