# Reliance declaration — commit-owner-s30-1 (kelgroups #30, slice S30-1)

Declared before the RED bundle per `commit-owner` §0. Reliance, not audit:
only what this slice's declarations rest on. Verified by direct read of
`lib/KelGroups/Types.hs` and `lib/KelGroups/State.hs` at base `7c67c81`.

```text
INV-30-KEY-TEXT
invariant:  every key-like value in the shared substrate is Text
            (CESR-encoded public key): Member.memberKey, GroupView.gvMembers
            keys, PendingBase.pbProposer/pbApprovals, PendingProposal
            proposer/approvals. This slice's QuestionId, designee payload,
            proposer, and tally lists as Text denote that same key space;
            no parallel Key/Designee type is introduced.
severity:   ADVISORY (no chain state, money, or signature is touched by
            declarations alone; a wrong-space key would misroute a future
            permission verdict, which later slices own)
enforced:   NONE — nothing in this repository would catch a violation;
            S30-1 ships declarations only and the slice gate checks
            identities, not field types
```

```text
INV-30-COUNT-INT
invariant:  substrate cardinalities are Int (adminCount, majority in
            KelGroups.State). This slice's Threshold = Int -> Int plugs
            into that space, so a future verdict site reads the franchise
            size without conversion.
severity:   ADVISORY (same reason: declarations only, no evaluation)
enforced:   NONE — same reason; the future verdict slice type-checks it
```

Neither reliance was found false. No CONTRACT-CHALLENGE.
