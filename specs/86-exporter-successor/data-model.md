# Data model — #86 exporter successor (extends D74, no widening)

Wrapper shape unchanged: economic `{view,auth,traces}`, integrated
`{initial,auth,steps}`. Binding the values of fields already present is not
widening. Exact key-set checks stay mandatory for their shape scope.

## D86-ECON-VIEW — economic `.view` live-bound

- Value: exactly `seedView` via `Lean.toJson` at this call site.
- Kill: `.view.members[0].key` → `"ZZZ"` must fail compiled `check` AND the
  shipped `jq` path must not rescue it (separability: key-set checks pass,
  value check fails — the right row kills).
- Bounded claim: call-site independence only (both sides share the derived
  `ToJson`); serializer-instance independence NOT established by this method.

## D86-INT-INITIAL — integrated `.initial` live-bound

- Value: exactly `corpusInitial`.
- Kill: `.initial.members` → `[]` must fail compiled `check`.

## D86-AUTH — both `.auth` identities live-bound

- Values: `econAuthIdentity` / `intAuthIdentity` strings matching the
  refusing probes under which emission ran (no backdonate event in corpus).
- Kills: (3) economic `.auth` → permissive string must fail; (4) integrated
  `.auth` → permissive string must fail. Four named controls, none left as
  "the fourth the audit measured".

## D86-ARITY — malformed check writes nothing

- `["check", onePath]` is a malformed `check` invocation, never a write to
  a file named `check`. Exits non-zero; pre-placed sentinels + full
  directory comparison prove no mutation (exit code alone insufficient).

## D86-MANIFEST — manifest + bytes

Unchanged: one SHA-256 per file over exact bytes; verify re-emits to temp,
`cmp`s both, `sha256sum -c`; any drift fails closed naming the file.
