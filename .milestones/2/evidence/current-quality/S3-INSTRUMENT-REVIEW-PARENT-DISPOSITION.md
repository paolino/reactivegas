# S3 successor instruments — parent disposition of the independent review

Owner `%503`. Reviewer `%581` (`gpt-6-astra`/`high`), START 2026-09-06T07:47:06Z,
**TERMINAL 07:59:41Z — AUDIT-FINDINGS**. Report
`7c8ac583b5c420893a1ad96abf4c90a092db9e9401e84ae68ffd373279881f3f`, manifest
`4afe1d00b634304478b3710647aae8d3d8fe4853ba6e3d3b0cd83798dca8e9d6`.
**I verified 79/79 entries, no self-entry.** Project executions: **0**.

**Verdict: the finite executable operation list is INCOMPLETE. The S3 successor
packet is NOT accepted, and no numeric grant can rest on it.**

## 1. What I confirmed myself, rather than relaying

**I-01.1 — all eight mutation patches are malformed, and I verified every one.**
Each hunk header declares `-N,7 +N,7`; the bodies supply:

| diff | body old/new | | diff | body old/new |
|---|---|---|---|---|
| `C-FOLD` | 6/6 | | `C-VALIDATE` | 6/6 |
| `C-INTEGRATION` | 5/5 | | `C-VOTEFOLD` | 4/4 |
| `C-KSTATE` | 3/3 (+2 bare blanks ⇒ 5 at best) | | `C-VOTESTATE` | 6/6 |
| `C-RSTATE` | 3/3 | | `C-VOTEVAL` | 5/5 |

The drivers call ordinary `git apply` with no `--recount`. **The mutation inputs
cannot apply.** Every mutation-bearing operation in the plan is unreachable before
any theorem is tested. I had not opened these files.

**I-01.3 — the wrappers invert their own exit status, and I proved it with an
executed control.** `cold.sh:35`, `replay-build.sh:25` and `replay-run-green.sh:35`
end with `[ "$C" -ne 0 ] && { … }`. Control:

```
$ bash -c 'C=0; [ "$C" -ne 0 ] && { echo x; }'; echo "exit=$?"   → exit=1
$ bash -c 'C=1; [ "$C" -ne 0 ] && { echo x; }'; echo "exit=$?"   → exit=0
```

**A successful command yields wrapper exit 1; a failed one yields 0.** The `.exit`
receipt can read 0 while the wrapper reports failure, and no frozen scheduler
resolves that. `check.sh:28` has the same defect on its promised-silent path.

**I-01.2** — `check.sh:14` rejects any `#eval` substring, and `Check.lean:5,8`
contain it **in comments**, so M09 rejects its own input before Lean runs.

## 2. My own claim that the review falsified

In `S3-SUCCESSOR-PACKET-PARENT-ASSESSMENT.md` §2 I wrote that the
`UNSUPPORTED-OBSERVED` control *"encodes the SS-0 correction structurally — only
Row1 EXECUTED-OBSERVED bound to SS0 is allowed — so the prediction/observation
boundary is enforced by the instrument, not merely asserted in prose."*

**That is false, and I have verified it at source.** `validate-packet.cjs:104–109`
is exactly:

```js
if (k.includes('OBSERVED') && obs!=='EXECUTED'){ fail('UNSUPPORTED-OBSERVED', …) }
```

There is **no Row1 identity check and no receipt lookup**. The words "only Row1
EXECUTED-OBSERVED bound to SS0" live in the **failure message string** — they are
documentation, not the predicate. Any row with both labels set to
`EXECUTED`/`OBSERVED` passes.

I read the fixture's *output message* and reported it as a structural guarantee.
That is the fourth time this session I have asserted content from a label instead
of the bytes behind it, and the worst of the four: I credited the instrument with
precisely the discipline the commission existed to establish.

## 3. My three findings, as the review resolved them

| | outcome |
|---|---|
| **A-01** `FALSE-AT-WITNESS` | **Qualified disagreement, and I accept it.** The field is explicitly a semantic axis, and the separate `observationKind` plus prediction notes can distinguish a static counterinstance from an executed witness; 66 fabricated executions do not follow from the spelling. The vocabulary does lack a schema — but I-04's actual binding inconsistencies and I-06's missing receipt-bound enforcement are **stronger than my naming objection**. |
| **A-02** `firstFailureIsolation` | **The reviewer disagrees with my "sound when read whole", and it is right.** I checked one file. `MEASUREMENT-REQUEST.md:7` and `REMAINING-REQUIRED-WORK.md` **repeat the unconditional first-failure claim without the correcting parenthesis**. The raw SS0 log has the error at physical line 126 / source `:209:4` followed by info at 1639–1641 and 2351–2353 — refuting universal termination, though not proving every later declaration was soundly checked. Not legibility: an unqualified false claim in two files. |
| **A-03** ownership | **Resolved as I-05; my arithmetic was wrong.** `relations` holds **561** records (558 PRESERVED, 3 CORRECTED); `openUnresolved` is a **separate** array of 2. Not "563 uniformly typed". The concern stands on better evidence: **51 WITNESS/SHARED relations remain PRESERVED with no atom allocation**, making the packet's own `witnessSharedNote` claim that unallocated relations are marked OPEN **false** for those entries. |

**D-01, D-02, D-03 confirmed as I-01, I-02, I-03. D-04 confirmed**, with its
explicit once-per-script / two-runtime-calls qualification preserved.

## 4. The findings I had no part in

- **I-02** — M03's mutation cannot falsify its named theorem: changing `verdictOf`
  `>=`→`>` changes **both sides** of the filter equivalence in
  `sweepClosures_open_mem`, which stays true. The replay RED guard requires
  substring `FAIL economic`, but `CorpusExport.lean:171–173` emits
  `corpus-check FAIL economic parse:` on malformed JSON — **the guard cannot
  distinguish the promised view mismatch from a parse failure.** The mutant driver
  counts `^ℹ.*Built` and misses the `✔ … Built` form present in the SS-0 log.
- **I-03** — eight restoration `argv` arrays contain a literal `&&`, which as an
  argv array passes `&&` to git; timer setup is claimed separately counted but no
  such layer or numbered operation exists; the preflight prints rather than
  enforces source SHA, coldness and toolchain equality.
- **I-04** — **116 atoms** carry the literal template
  `[single-atom negation/removal; exact literal frozen in operations.json per-row]`;
  **204 of 207 row spans are header-plus-estimate** (row 2's `430–442` covers only
  the opening arm), so **row 1's genuine `197–209` binding was not generalized**;
  **76 private compiled names are mangled** against the retained map
  (`_private.KelGroups.Invariants.assocAdjust_keys` vs
  `_private.KelGroups.Invariants.0.KelGroups.assocAdjust_keys`); and the packet's
  own SS-3 claim of 72 NO-EXECUTION operations parses as **156**.
- **I-06** — seven further validator blind spots, deduced from complete source:
  unresolved-reference whitelisting, no argv semantics in cost matching,
  hardcoded 239/81/158/131 quotas instead of discovery, `shaFile` **never called**,
  no argv-level prohibition on `#eval`-as-runtime, and single-atom credit that
  cannot establish one source edit. **The validator's PASS on this malformed
  packet is itself the evidence that these are not rejected.**
- **I-07** — of 66 receipt records the first **43 are ROSTER-ONLY table headers**;
  16 later records carry empty hash objects; the six purported recovered t57
  instrument records supply `hashes: {}`.

**Preserved as genuine:** the independent parser found **239** declaration sites
(163 public / 76 private) matching the packet with zero missing or extra, **81**
unique helper records, **all 207** original row IDs with successor references and
no duplicates or drops, **131** old atom IDs present in a 151-ID successor
vocabulary, and all eight supplied fixtures failing for their own intended reasons
on an independent rerun. Static identity reconciliation holds. Executable closure
does not.

## 5. Disposition

**S3 successor packet: NOT ACCEPTED.** The commission asked for an executable
instrument and a precise operation registry. The registry's identity layer is
sound; its executable layer is not — the patches cannot apply, the wrappers invert
their exit status, one driver rejects its own input, the batch operations have no
implementation, two guards accept the wrong reason, and the validator does not
enforce most of the contract it was written for.

**No execution commission and no numeric grant follow.** Enumeration must precede
any grant, and NOTE-071 already forbids inferring a replacement total; I infer
none.

Per NOTE-071 there is **no automatic second author submission**. `%580` is
terminal and is not returned to. The next S3 step needs a fresh desk decision on
scope, and I make no claim about what it should be.

**S3 is not closed. F-01, F-02, F-03, F-06, F-07 remain PARTLY — blocking. All
original 207-row and semantic-ownership obligations stand. SS-0 remains one bound
historical operation, not coverage.**
