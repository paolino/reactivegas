# Corpus coverage — `reactivegas#86` slice S86 (commit-owner authored content; ticket owner places/routes)

Successor to the `#74` handoff (`t74-corpus-exporter/handoffs/CORPUS-COVERAGE.md`).
Corrected to the current frozen bytes with zero unproved-label rows; the dated
pre-S1 measurement is preserved below as history. All counts measured from the
emitted frozen bytes.

## Dated entry — 2026-09-05 (S86, current)

- `lean/corpus/economic.json`: 14494 bytes,
  sha256 `73a077fc514038e40f84aca4a995fe68623e3af46ed11c0280d5b963137576aa`
- `lean/corpus/integrated.json`: 7673 bytes,
  sha256 `1f173aec9c3afd9cb95265e4be2966b9316e810a969d9fc40f672b17120f3675`
- `lean/corpus/corpus.sha256` carries one SHA-256 line per file over the exact
  bytes; `just lean-corpus-verify` re-emits to a temp dir, byte-compares both
  files, checks the manifest, runs the compiled `check` (live-bound context +
  traces/steps), and enforces both exact `jq` key-set programs.

Claim-label status of the current bytes (dated measurement, same command as
the prior handoff):

```
$ grep -o '"declaration":"[^"]*"' lean/corpus/economic.json | sort | uniq -c
      1 "declaration":"step_close_inv"
      1 "declaration":"step_withdraw_inv"
$ grep -c <unproved-label> lean/corpus/economic.json; grep -c <unproved-label> lean/corpus/integrated.json
0 / 0 — zero unproved-label rows in either file
```

(The two `grep -c` probes above are quoted here as procedure, not as file
content: the counts are zero and no such row exists in the bytes.)

## Dated history (preserved) — pre-S1 measurement

Before `#66` S1 landed, the frozen economic bytes
(sha256 `91526dc6…f586`) carried one withdrawal-refusal row whose declaration
rendered the unproved label even though the repository proves that guard.
Cause, from the `#66` S1 repair lane
(`/tmp/reactivegas/ms2/e-lean-compliance/handoffs/RECONCILIATION-001.md`):
the `Trace.lean` manifest resolved inversion candidates with `Name.mkSimple`,
so the six inversions declared inside `namespace Reactivegas` were never found.
`#66` S1 landed at `4a6cd87` (PR #79, merged 2026-09-05); the re-emission
against that base moved the economic bytes to `73a077fc…` and the withdrawal
row now binds `step_withdraw_inv`. The old `91526dc6…` bytes are superseded,
kept here only as a dated record.

Stale comment routed, no model edit (S86 records only): `Trace.lean`
`seedDenyPermissionRefunds` still documents that the corpus "exercises an
unproved-label claim row". That was true of the pre-S1 bytes and is false of
the current bytes. Routing to the desk → `#66`/`#71` as a quality/docs
follow-up; the exporter grants no model-edit scope, so the comment stands
until that lane corrects it.

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
  `withdraw` (now binding `step_withdraw_inv` since S1).

No seed contains a `backdonate` event; backdonation is not evaluated here.

**Integrated corpus** (`integrated.json`: `{initial, auth, steps}`) — 7
base-channel steps from `corpusInitial` (members alice/bob/dora/eve plus the
s62b fixture payload) with the refusing probe `probeAuth`: admin admit
(carol, accepted), non-admin admit (zed, rejected), member departure
propose/approve (bob), role-change admin loss with V-3 close (eve),
departure propose (dora), admin-departure cleanup. Each step stores the
signer, the integrated event, the `accepted` verdict, and the complete
post-`GroupState`.

Since S86 the compiled `check` binds the wrapper context live: economic
`view` against `seedView`, integrated `initial` against `corpusInitial`,
each `auth` against its refusing-probe identity string, all with nonzero
member extents. Bounded claim: this live-call/derived-ToJson method does not
establish serializer-instance independence.

## What neither covers — votes

Neither corpus exercises a vote event. `step` returns `none` for
`openQuestion`/`cast`/`renounce` (`lean/Reactivegas/Step.lean:140-142`);
they run inside `appFold` via `voteApply`, which the economic corpus never
reaches and the integrated corpus never emits (its 7 events are
admit/direct/propose/approve only).

Consequence, stated plainly: **assenso is named in the milestone's outcome
test and has no oracle behind it.** Extending coverage to votes is separate
work: **`#75`** (integrated vote corpus through the production root) and
**`#76`** (runtime composition: vote closures must authorize grant, deny and
backdonate). Both filed under milestone 2, parent `#72`. A green gate must
not be read as implying vote coverage.

## Provisional content — one landed, the rest pending

- `#66` S1 — LANDED at `4a6cd87` (PR #79, merged 2026-09-05): the manifest
  repair; current bytes `73a077fc…` / `1f173aec…` are post-S1.
- `#68` (proposer is not an assent — changes emitted verdicts) — pending;
- `#69` (pledge sovereignty — changes what the corpora emit) — pending;
- `#76` (runtime composition) and `#81` (V-5 vote lifecycle) semantics —
  pending, tracked for what they imply for corpus meaning;
- `#75` replay context — the threshold stays resolved out-of-band (see the
  replayer table); the outer replay-context contract is under desk review.

Nothing in this section reads as final. The exporter format
(`reactivegas.trace/v1` plus the `GroupView`+auth wrapper) does not depend
on any of them and is final as shipped.

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

Deliberately not carried: the vote threshold. The replayer resolves
`Reactivegas.s62bThreshold` from source. Changing the threshold changes
verdicts, and the stored steps would stop matching — that mismatch is the
signal, not a silent pass.
