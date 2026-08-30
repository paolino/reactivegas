# Tasks — #62 one membership and role model

Artifact ceiling: 90 lines / 7 KiB. Only the ticket owner checks behavior-task
boxes after a fresh audit passes the exact candidate.

## Planning and gate

- [x] **T6200** Verify issue/A-009/draft/discovery hashes, exact base/tree,
      branch cleanliness, and clean baseline CI.
- [x] **T6201** Freeze the compact module/data/function contract and the three
      OWNER slice boundaries.
- [x] **T6202** Freeze the ignored ticket gate, per-slice gates/manifests, and
      named negative-control receipts without production/proof edits.

## Slice S62-A — canonical integration boundary

- [x] **T6210** Introduce `GroupView`, distinct integrated app-state/app-event
      types, payload-only fold result, and the atomic integration boundary.
      (R62-02, R62-04)
- [x] **T6211** Make `GroupState.members` the sole writable membership/role
      store; remove economic/vote copies. (R62-01)
- [x] **T6212** Replace every Reactivegas identity with `KelGroups.Key`; reserve
      and exclude the comune key with no bridge. (R62-03)
- [x] **T6213** Make vote state membership-free and bind franchise/verdict
      observations to the canonical view. (R62-11)
- [x] **T6214** Prove app-event member preservation and falsify a member-writing
      app-fold mutant. (R62-04)
- [x] **T6215** Preserve the historical fold/theorem bytes while proving the
      integrated Reactivegas production root does not call them. (R62-05)

## Slice S62-B — one transition system

- [ ] **T6220** Add the sole current-admin direct admission command with
      duplicate and reserved-key rejection plus reachable positive/negative
      controls. (R62-06)
- [ ] **T6221** Close `Reactivegas.Proposal` without admission and falsify a
      seeded `introduceMember` reintroduction. (R62-07)
- [ ] **T6222** Remove all four app membership/role events and all three
      vote-local member/role events; signer is the sole author. (R62-08,
      R62-11)
- [ ] **T6223** Seal economic cleanup and vote recomputation into every real
      base member/role transition. (R62-09, R62-10)
- [ ] **T6224** Re-demonstrate V-3 from a real base transition with unchanged
      tallies and no new ballot. (R62-11)
- [ ] **T6225** Make all app/direct/proposal/base-change/verdict classifications
      exhaustive and falsify every seeded constructor. (R62-12)
- [ ] **T6226** Pass the S62-B gate, focused build, proof-debt checks, full CI,
      and fresh independent audit.

## Slice S62-C — proof, mutation, trace, and observable closure

- [ ] **T6230** Rebind every member-scoped economic guard/predicate and
      backdonation cardinality/distribution to canonical membership. (R62-13)
- [ ] **T6231** Add concrete integrated base-to-hook cleanup/recompute theorems
      and production witnesses without editing the historical theorem.
      (R62-14)
- [ ] **T6232** Re-prove and freshly falsify all #57 rows and inherited
      PARTITION/DISJOINT/NOSTALE/FRANCHISE/POLICYFREE obligations on the
      integrated path. (R62-15)
- [ ] **T6233** Emit/replay one integrated JSON corpus covering both admission
      outcomes, role/member transitions, every cleanup effect, and
      franchise-only closure. (R62-16)
- [ ] **T6234** Close live app/base/proposal/change/verdict and theorem
      inventories with seeded failure controls. (R62-12, R62-16)
- [ ] **T6235** Record zero escape hatches, allowed axiom sets, statement
      hashes, production witnesses, and definition-mutant RED receipts.
      (R62-16)
- [ ] **T6236** Pass focused Lean build, all frozen slice/ticket gates, clean
      full CI, exact-tree proof, and fresh independent audit. (R62-16)
