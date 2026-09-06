# Q-001 — bind a writable gate execution envelope

Parent: t28-app-api. Pointer acknowledged: POINTER-1788641206-2328072.
Audit preflight only; no candidate inspection, gate execution, builds, probes,
commits, or source writes performed. This is a pending contract question,
not a semantic finding or terminal audit report.

## Verified facts

- Brief read in full: sha256
  `0287a6837b7d4164a63be8e5d17af434cbfe4f92fdcfc83e69a90cfb99d98327`.
- Frozen gate identity is VALID: blank-normalized sha256
  `f5796d1e5524f57c98b7d92168ad4fc10b22b6d318c57e0c989a1bf02f1b3650`;
  full-file sha256
  `7a7a99e351df0a34a8147804f68bc85a3f182556cbfafccaf62e46105743c30a`.
  Both match parent STATUS freeze record line 56. The initial chat suspicion
  compared unlike hash forms and was corrected before this question.
- Current pane `%554` resolves explicitly to `reactivegas:8`, live window
  name `kelgroups`; parent `%534` and author `%545` are distinct panes in
  that window. The brief's longer window name is not the live name.

## Execution conflict requiring a ruling

Brief lines 33–34 prohibit ALL candidate-tree edits and permit build writes
ONLY in ignored `dist-newstyle/`. Lines 54–58 and 77–81 require the complete
frozen gate, including in-place M1–M6 mutation, on the auditor's own tree.

The exact frozen gate has these observable operations:

- `/code/kelgroups-issue-28/gate.sh:36–37` changes directory to the script's
  directory. Invoking the named absolute path from the audit checkout would
  run against the AUTHOR checkout, regardless of the caller's initial CWD.
- `gate.sh:157` restores tracked source with `git checkout -- ...`.
- `gate.sh:197` appends M1 to tracked `test/S28DemoApp.hs`;
  subsequent M2–M6 edit tracked Fold/Event/Store files and restore them.
- Gate evidence defaults to the parent's root (`gate.sh:40`), requiring
  `G28_EVIDENCE_DIR` override for the brief's auditor-local evidence rule.

I cannot execute this gate unchanged at its named path, nor interpret the
absolute no-edit clause as granting transient tracked-file mutation.

## Concrete proposed resolution

Authorize a disposable, exact-candidate detached execution worktree under
this auditor runtime root, with a byte-identical copy of frozen v8 named
`gate.sh` at that worktree root. Bind its full and normalized hashes before
execution. Run the entire envelope there with `G28_EVIDENCE_DIR` pointing
inside this auditor's `handoffs/`. Explicitly permit only gate-generated
temporary mutants/restoration and necessary ignored build outputs there.
Keep `/code/kelgroups-audit-84a2dae` immutable as the reference checkout.
Preserve the 12 substantive / 24 targeted caps and full coverage unchanged.

Alternative: explicitly amend the supplied audit checkout's write fence to
permit the same byte-identical gate placement, temporary M1–M6 edits and
restoration, and necessary ignored build outputs. No author-tree execution.

Please bind one execution envelope and its allowed writes in A-001 before
execution. Also supply the campaign-ledger path, ceiling-raise ledger/count,
and exact dispatch family-set/exclusions required by commit-auditor preflight;
the brief and packet close give budgets but do not explicitly bind those
ledger/selection fields. I will validate the remaining packet after this
question is resolved, without inferring those bindings.

Parked pending parent delivery of the durable answer. Spend: 0/12 substantive,
0/24 targeted. No acceptance decision made.
