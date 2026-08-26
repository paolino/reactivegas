# Research — #54 (ticket-owner discovery; not sent downstream wholesale)

## Fidelity sources read in full

`/code/kelgroups` at `368b596`, clean tree. Five files named by the issue plus
`lib/KelGroups/Bootstrap.hs`, which the issue omits but `Validate.hs` imports.

```text
c187d4360ed9fa54a212bd6a77717ce062cc337668d5c55f7cdb2a0e70624ce8  lib/KelGroups/Types.hs
c1d7a0064eb0b61a55bdf89e21bafe399e5fa3445de68b6a7f484510f837104b  lib/KelGroups/Event.hs
63c807fdf06f29ed7fc0dee18cf67894004e00a7ed30c901252c67f099b470bc  lib/KelGroups/State.hs
3aedf556323db8b197b194502a2d47e093345a0b69e3623ea64b59253684994a  lib/KelGroups/Fold.hs
d82110e8c3ca8f1ea51447b4b45023fecc2085d2f21008fe05a83baf78fb1790  lib/KelGroups/Validate.hs
```

`Bootstrap.hs` is 33 lines: `AuthMode = Bootstrap | Normal` and
`authMode gs = if adminCount gs == 0 then Bootstrap else Normal`.

## Repository facts established

- `lean/lean-toolchain` = `leanprover/lean4:v4.27.0`; `lean/lake-manifest.json`
  has an **empty** package list — no mathlib, Lean core only.
- `lean/lakefile.lean` declares exactly one `lean_lib Reactivegas` with
  `srcDir := "."` as the sole `@[default_target]`.
- CI's Lean leg is `nix develop --command just lean` → `cd lean && lake build`.
  A new library that is not a default target is therefore invisible to CI.
- `/gate.sh` is **not** in `.gitignore` (which lists `dist-newstyle/`, `result`,
  `result-*`, `.direnv/`, `*.sqlite`, `*.db`, `tarogas.tgz`, `lean/.lake/`).
- `docs/en/design/` contains only `state-machine.md`; `mkdocs.yml` nav has a
  `Design:` section with one entry. #47's unmerged `kelgroups-mapping.md` will
  add an adjacent nav line — a trivial future conflict.
- Baseline `nix develop --quiet -c just ci` at `3ed9107` is **green**
  (exit 0, 92s).

## Awkward Haskell behaviours found by reading, not assumed

These drove requirements R-7, R-11..R-15, R-18, R-22, VI-6, VI-7.

1. `applyPropose` uses `Map.insert`, so re-proposing an existing id **discards
   accumulated approvals** and reseats the proposer.
2. `tryEnact` reads `majority gs` on the state *before* `enact`, with the
   proposal already inserted.
3. Enactment deletes only the enacted proposal; siblings keep stale approvals
   against a threshold that may have moved. This makes the intuitive invariant
   "pending ⇒ below threshold" **false**, reachably: 3 admins (threshold 2), P
   pending with 1 approval, Q enacts a member removal → 2 admins (threshold 1),
   and P now sits at 1 ≥ 1 while still pending.
4. `enact` uses `Map.adjust` for role changes — a **silent no-op** when the
   member is absent — and `Map.insert` for introductions, which **overwrites**.
5. `applyApprove` on an unknown id returns the state unchanged; duplicate
   approval is idempotent in the fold and rejected only in `Validate`.
6. `validateBootstrapProposal` **never inspects the signer**. During bootstrap
   a complete stranger may propose. That makes "every approver is an admin"
   false even for fully validated traces, since propose auto-approves.
7. `checkRole`/`checkRemoval` return `Right ()` for every admin role, so nothing
   prevents a proposal stripping the last admin and returning the group to
   bootstrap (EP-LAST-ADMIN).
8. `validateRoleChanges` re-looks-up a member `requireMember` already proved
   present, making one `MemberNotFound` branch unreachable
   (EP-REDUNDANT-LOOKUP).
9. There is **no dissent, rejection, expiry, or withdrawal event anywhere**.
   `Propose` and `Approve` are the entire base vocabulary. This is EP-DENY and
   it is the Slice-2 blocker.

## Superseded instructions (kept for provenance)

Epic NOTE-017 and the original issue body required a **mutual** import ban plus
a third `Composition…` module family. Epic NOTE-018 (operator overrule) and
ticket NOTE-001 replaced this with a one-way rule; the corrected issue body was
published and hashes to
`81a94a3b209ecf520be635af439d9be8c9d445ac0b5ac5e60ad501ebb7f774f7`.
