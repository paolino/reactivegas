# Data model — #71, version 2

- **citation**: a Markdown code span with the marker prefix `lean:` followed by a
  Lean name. Valid when the name resolves to a constant in the built library.
- **declaration-like claim**: a code span without the marker whose text Lean
  resolves to a constant of the library under the resolution rule the record
  states. Invariant: none exists in the record.
- **discovered extent**: the set of citations and claims found by scanning every
  Markdown file under `docs/en/design/`. Invariant: non-empty.
- **checker verdict**: zero exit only when the Lean import succeeded, the extent
  is non-empty, every citation resolved and no declaration-like claim exists.
