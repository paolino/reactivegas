# Corpus coverage — `reactivegas#74` slice S74 (ticket-owner artifact, routes to #71)

Promoted from `commit-owner/handoffs/coverage.md` (submission 1, candidate
`fed19b3`) and amended by the ticket owner per NOTE-003: the freeze list is
now three inputs, and the `UNPROVED` mislabelling is stated as a known-wrong
claim in the frozen bytes. All counts measured from the emitted frozen bytes
(`lean/corpus/economic.json` sha256 `91526dc6…f586`,
`lean/corpus/integrated.json` sha256 `1f173ae…0367`).
Per NOTE-001 this lives here, not in `docs/` (#71 owns the record).

## What each corpus covers

**Economic corpus** (`economic.json`: `{view, auth, traces}`) — 5 traces,
32 events, every trace from `State.empty` under `seedView` (admins `1`,
`2` + ordinary member `3`) with the refusing probe `seedAuth`
(`fun _ _ => false`):

- donation prefix: deposits, `openPurchase`, pledges, `acceptPledge`,
  attested `donate` (mixed accepted/pending collection);
- pledge corrected downward and upward (`correctPledge` both directions);
- closure driving the referente's cassa negative, including a *refused*
  `closePurchase` before `grantPermission` (refusal with accepted inversion);
- denial refunding accepted + pending pledges, including a *refused*
  `withdraw` (identity with no accepted inversion — see the known-wrong
  claim below).

No seed contains a `backdonate` event; backdonation is not evaluated here.

**Integrated corpus** (`integrated.json`: `{initial, auth, steps}`) — 7
base-channel steps from `corpusInitial` (members alice/bob/dora/eve plus the
s62b fixture payload) with the refusing probe `probeAuth`: admin admit
(carol, accepted), non-admin admit (zed, rejected), member departure
propose/approve (bob), role-change admin loss with V-3 close (eve),
departure propose (dora), admin-departure cleanup. Each step stores the
signer, the integrated event, the `accepted` verdict, and the complete
post-`GroupState`.

## What neither covers — votes

Neither corpus exercises a vote event. `step` returns `none` for
`openQuestion`/`cast`/`renounce` (`lean/Reactivegas/Step.lean:140-142`,
verified against the candidate tree — the brief's `141-143` is off by one);
they
run inside `appFold` via `voteApply`, which the economic corpus never reaches
and the integrated corpus never emits (its 7 events are
admit/direct/propose/approve only).

Consequence, stated plainly: **assenso is named in the milestone's outcome
test and has no oracle behind it.** Extending coverage to votes is separate
work, now owned elsewhere: **`#75`** (integrated vote corpus through the
production root — reuses this slice's exporter, wrapper discipline, manifest
and negative-control pattern) and **`#76`** (runtime composition: vote
closures must authorize grant, deny and backdonate). Both filed under
milestone 2, parent `#72`, reporting to the desk — not children of `#74`.
A green gate must not be read as implying vote coverage.

## Known-wrong claim in the frozen bytes — `UNPROVED` mislabelling

Measured on the frozen artifact itself:

```
$ grep -o '"declaration":"[^"]*"' lean/corpus/economic.json | sort | uniq -c
      1 "declaration":"step_close_inv"
      1 "declaration":"UNPROVED"
$ grep -o '"id":"[^"]*"' lean/corpus/economic.json | sort | uniq -c
      1 "id":"closePurchase"
      1 "id":"withdraw"
```

The `withdraw` refusal row claims `"declaration":"UNPROVED"` for a guard the
repository proves. Cause (from `#66` S1 repair lane,
`/tmp/reactivegas/ms2/e-lean-compliance/handoffs/RECONCILIATION-001.md`):
the `Trace.lean` manifest resolves inversion candidates with
`Name.mkSimple`, so the six inversions declared inside
`namespace Reactivegas` are never found. A consumer who reads
`"declaration":"UNPROVED"` and believes it is worse off than one who knows
the corpus is provisional: the guard **is** proved, the label is wrong.

## Provisional content — one landed, the rest pending

The checked-in corpus content is provisional and must be re-frozen before D3
consumes the files as an oracle:

- `#66` S1 — LANDED at `4a6cd87` (PR #79, MERGED 2026-09-05): the manifest
  `mkSimple` repair. The re-emission against this base is the slice's
  submission 2 (own fresh audit). The `UNPROVED` bytes above move there.
- `#68` (proposer is not an assent — changes emitted verdicts) — pending;
- `#69` (pledge sovereignty — changes what the corpora emit) — pending;
- `#76` (runtime composition) and `#81` (V-5 vote lifecycle) semantics —
  pending, tracked for what they imply for corpus meaning;
- `#75` replay context — the threshold stays resolved out-of-band (see the
  replayer table); the outer replay-context contract is under desk review.

Nothing in this section reads as final: S1 leaving the list does not make
the bytes final. The exporter format (`reactivegas.trace/v1` plus the
`GroupView`+auth wrapper) does not depend on any of them and is final as
shipped.

## Replayer field table

Economic file, per field:

- `view`: load the member/role relation; resolve every membership/admin
  read (`isMember`/`isAdmin`) against it. Today: two admins + one member.
- `auth`: exact-match this string; evaluate with a refusing
  authorization (`fun _ _ => false`). Any other value means the file is not
  this corpus — refuse it.
- `traces[i]`: require `schema == "reactivegas.trace"` and `version == 1`;
  require `initial` to be the empty aggregate; for each step, require the
  step's `input` to chain continuously and recompute the outcome with
  `stepDetailed view input event auth`, comparing the full `result`
  (`applied` state or `refused` guard claim). Never trust `input` as
  authority — recompute it.

Integrated file, per field:

- `initial`: exactly `corpusInitial`; start the replay here, not from any
  locally constructed state.
- `auth`: exact-match this string; evaluate with refusing `probeAuth`.
- `steps[i]`: recompute `snapshotStep gs signer event`, i.e.
  `Reactivegas.apply s62bThreshold probeAuth gs signer event`
  (`s62bThreshold = KelGroups.Vote.legacyThreshold`,
  `lean/Reactivegas/Invariants.lean:1302`; `apply`,
  `lean/Reactivegas/Step.lean:377`); require the recomputed record to equal
  the stored one (`==`, includes `accepted`, full post-state, and change);
  advance with `nextState`. The repository's own `replayFrom`/
  `replayIntegratedCorpus` is this procedure.

Deliberately not carried (NOTE-001 wrapper bound): the vote threshold.
The replayer resolves `Reactivegas.s62bThreshold` from source. Changing the
threshold changes verdicts, and the stored steps would stop matching — that
mismatch is the signal, not a silent pass.
