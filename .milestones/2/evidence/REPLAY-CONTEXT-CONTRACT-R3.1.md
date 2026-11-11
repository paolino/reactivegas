# Replay-context contract — R3.1, planning contract

Epic owner `claude-opus-5[1m]`, for desk `%510`. R3.1, 2026-09-05, applying
NOTE-011 to R3. **Planning contract. No code grant. `#74` untouched.** Planning
direction for `#75`.

Accepted forward from r2 and unchanged: table authority, the refusal/mismatch
distinction, source-side independent re-derivation, and the label-only control.

---

## What R2 and R3 got wrong

| r2 | correction | source |
|---|---|---|
| `sourceTree` = Git tree of `lean/` | **still circular** — that tree contains the generated corpus and sidecar under `lean/corpus`. It also does not capture untracked or unstaged bytes, so "reproducible" was not established. | §6 |
| Lean-side re-derivation listed under "open for the desk" | **already authorized and required for `#75`.** Not a scope question. Moved into the contract. | §2 |
| "replay never starts" for every refusal | **false** for a domain refusal only detectable at a later query. Two different moments. | §1 |
| "a corpus containing any question that stays `.open`, or any closing `.negative`, satisfies the precondition" | **wrong.** `zeroThreshold` does not decide permission questions at all — `verdictOf`'s `.permission` arm never calls the threshold. Only a *collective* question can distinguish the two policies. | §7 |
| three explanations of unseen franchise churn | **not established behaviour.** Verified: `sweepClosures` does query the threshold repeatedly, but always at the post-transition view, whose franchise is visible in the state. The examples are withdrawn; the rule keeps a justification that does not need them. | §4 |
| "cleanliness friction" offered as an option | **no loophole.** Reproducibility from committed inputs is required. | §6 |
| control row 4 deleted an interior entry | **would refuse pre-replay, not abort at runtime.** Breaking contiguity trips §3 rule 3 before replay starts, so the control passed without ever testing the property it names. Truncate to a valid prefix instead. | §7 |
| "steps already checked are not evidence of anything" | **overreached.** The abort forbids a successful *full-conformance* claim and committed modeled/domain effects, not diagnostic files. Logs and a scoped prefix report are allowed. | §1 |
| monotonicity "not required so `zeroThreshold` stays expressible" | **the rationale was false** — a constant-zero function is non-decreasing, so `zeroThreshold` was never excluded by monotonicity. No monotonicity assumption is made; no reason is needed. | §3 |
| §10 reopened per-corpus vs shared | already selected in NOTE-007. Removed. | §10 |

---

## 1. Failure taxonomy — two moments, three outcomes

**PRE-REPLAY CONTEXT REFUSAL.** Detected before any step is replayed; replay
never starts.

- `schema` or `version` unrecognised;
- `corpus.contentSha256` ≠ the corpus file's digest;
- the table fails canonical validation (§3);
- an auth semantics token the replayer does not implement.

**RUNTIME CONTEXT ABORT.** A threshold is queried at an `n` outside the table's
domain. This is detectable only when it happens, so replay has started.

On detection the replayer **aborts validation**. It makes **no successful
full-conformance claim** and commits **no modeled or domain effect** — nothing
that would stand as a pass. It reports the queried `n` and the domain.

It **may** retain diagnostic evidence: logs, and a report of the prefix it
checked, explicitly scoped as a prefix. Those checked steps **do not establish
full-corpus conformance**. They are diagnostics, not a verdict — useful for
finding out where the domain ran short, and not usable as partial acceptance.

**CONFORMANCE MISMATCH.** The context is structurally valid and its domain
sufficed; replay ran and a step disagreed. This is the *only* outcome that is a
statement about the corpus.

A mismatch **may be reported at the first differing step**. Once a disagreement
is known the verdict is settled, and completing the remaining steps establishes
nothing further; continuing is a diagnostic choice, not an obligation.

The three must be distinguishable in the replayer's output. Collapsing an abort
into a mismatch reports a corpus defect that was never demonstrated; collapsing
it into a pre-replay refusal misstates when it was found.


## 2. Trust boundary, and the required source-side check

| bytes | status |
|---|---|
| the sidecar's own bytes | cannot authenticate themselves; integrity comes from the repository carrying them and from the gate re-emitting and byte-comparing them |
| `corpus.contentSha256` | operative — binds this sidecar to those corpus bytes; detects pairing errors, not forgery |
| `policies.*.table` | **the sole authority for replay** |
| `policies.*.source.*` | provenance only; **a replayer must never resolve it**, and it authenticates nothing |
| `generatedFrom` | provenance only (§6) |

### The Lean-side re-derivation is part of this contract

Not an option and not a scope question — **required for `#75`**.

The exporter gate independently evaluates the policy at every point in the
declared domain, straight from the named live declaration, and **compares those
values against the table in the emitted sidecar bytes**. Any difference fails
the gate.

Two things it must not do:

- it must not read `source.gitBlobOid` or any other provenance field and treat
  agreement there as evidence — provenance is not a value check;
- it must not obtain the "expected" values from the same call path that wrote
  the sidecar, which would co-generate both sides.

This check is what makes the table trustworthy. The replayer inherits that
trust; it performs no Lean evaluation of its own and could not.

---

## 3. Canonical table validation

A `table` is a list of `[key, value]` pairs, valid iff all hold; any failure is
a pre-replay refusal naming the rule:

1. every key and value is a non-negative integer, encoded exactly — no floats;
2. keys strictly increasing;
3. keys contiguous from `0`, i.e. exactly `{0 … N}`. **`N` is the domain**;
   there is no separate bound field able to disagree with it;
4. no duplicate key;
5. non-empty.

**No monotonicity assumption is made.** Values need not be non-decreasing.

---

## 4. Domain is checked at the point of query

**Every threshold query the replay performs must fall inside the domain**, and
the check happens **when the query is made**, not by pre-scanning states.

The justification is not that a queried franchise is invisible — it generally is
visible, and r2's three examples claiming otherwise are withdrawn as
unestablished. The justification is simpler and does not depend on them:

> **Pre-scanning requires the replayer to predict which sizes will be queried,
> which means reimplementing the sweep's query pattern. That is a second
> implementation of the thing under test, and it can diverge.** Checking at the
> point of query predicts nothing.

One fact was verified rather than assumed and is worth recording, because it
sets the generating side's obligation: `sweepClosures` evaluates `verdictOf` for
**every open question, twice** — once in its `filter`, once via `sweepStep` in
its `filterMap` — so one transition performs many threshold queries. All at the
same view in current behaviour.

The generating side's obligation follows: the exporter emits a table whose
domain covers **every query generation actually performed**, and its gate fails
if it does not. `#75` inherits that.

---

## 5. Proposed bytes

One sidecar per corpus, beside it, `<corpus>.context.json`. Never inside the
corpus file.

```json
{
  "schema": "reactivegas.replay-context",
  "version": 1,
  "corpus": {
    "path": "corpus/integrated.json",
    "contentSha256": "1f173aec…0367"
  },
  "policies": {
    "threshold": {
      "table": [[0,0],[1,1],[2,1],[3,2],[4,2],[5,3]],
      "label": "legacyThreshold",
      "source": { "declaration": "KelGroups.Vote.legacyThreshold",
                  "path": "lean/KelGroups/Vote/Types.lean",
                  "gitBlobOid": "<git object id>" }
    },
    "backdonateAuth": {
      "semantics": "always-refuse",
      "label": "probeAuth",
      "source": { "declaration": "Reactivegas.probeAuth",
                  "path": "lean/Reactivegas/Step.lean",
                  "gitBlobOid": "<git object id>" }
    }
  },
  "generatedFrom": {
    "sourceInputDigest": "<see §6>",
    "exporter": "Reactivegas.CorpusExport"
  }
}
```

- **`contentSha256`** — SHA-256 over the corpus file's **raw bytes**. Not a Git
  object id: Git hashes `"blob <len>\0" ++ content`, a different value over
  different input. The field names keep the two apart.
- **`gitBlobOid`** — a Git object id, provenance only.
- **`label`** — human-facing, **no operative force**; control row 8 proves it.
- **`semantics`** — enumerated tokens, only for `backdonateAuth`, which is not a
  numeric function and so has no table. An unrecognised token is a refusal,
  never a guess. §8 governs its retirement.

---

## 6. `sourceInputDigest` — non-circular, dirty-aware, no loophole

Replaces r2's `sourceTree`, which was circular (the tree it named contains the
generated outputs) and blind to uncommitted bytes.

**The source-input projection** is the set of paths that are genuinely inputs to
generation:

- `lean/lakefile.lean`, `lean/lean-toolchain`, `lean/lake-manifest.json` —
  build and dependency pins;
- the exporter module, and **every `.lean` file in its transitive import
  closure**, discovered from the import graph rather than hand-listed.

**Excluded, explicitly:** `lean/corpus/**` — every generated corpus and every
sidecar, which is what removes the circularity — and `lean/.lake/**`.

**The digest** is computed over the **working-tree bytes actually used**: for
each projected path in sorted order, `path` and the SHA-256 of its content;
those pairs concatenated and hashed once. Because it hashes the working tree, it
captures untracked and unstaged bytes, which `HEAD:lean` cannot. The proposal no
longer claims otherwise.

**Reproducibility requirement, mandatory.** Final candidate generation must be
reproducible from committed inputs: for every path in the projection, the
working-tree content must equal the committed content, and the gate must fail
otherwise. There is no optional stronger mode and no friction exemption.

**Output paths are exempt, and only they.** Regenerating the corpus modifies
`lean/corpus/**` by design, so those paths are outside the projection and their
modification is not a violation. Nothing else is exempt.

---

## 7. Controls, and a precondition that is actually sound

### The precondition — corrected

r2 claimed any question staying `.open` or closing `.negative` would
distinguish the policies. **That is wrong.** `verdictOf`'s `.permission` arm
never consults the threshold:

```lean
| .permission designee =>
    if question.assents.contains designee then .positive
    else if question.dissents.contains designee then .negative
    else .open
```

A permission question behaves identically under every policy, so a corpus of
permission questions distinguishes nothing.

**Corrected precondition, proved by the gate before any control runs:** there
exists at least one **collective-question threshold query**, actually reached
during replay, at which `legacyThreshold` and `zeroThreshold` yield **different
step outcomes**. The gate demonstrates it by executing both policies over the
corpus and exhibiting the first step whose outcome differs — it does not infer
it from the corpus's shape. If no such query exists, the corpus is **inadequate
as a policy witness** and the gate fails on that ground, named as such.

### The controls

| # | mutation | required outcome |
|---|---|---|
| 1 | none | exit 0 |
| 2 | replace `table` with `zeroThreshold` over the same domain | non-zero, **CONFORMANCE MISMATCH**, naming the discriminating collective query the precondition exhibited |
| 3 | corrupt one character of `corpus.contentSha256` | non-zero, **PRE-REPLAY REFUSAL** (digest) |
| 4 | **truncate the table to a valid nonempty prefix** `{0 … n-1}`, strictly below a witnessed queried `n >= 1` from the precondition | first **no pre-replay validation failure** (the prefix is still contiguous-from-zero and valid per §3), then non-zero as a **RUNTIME ABORT** (domain) at exactly that `n`, with no full-conformance claim |
| 5 | duplicate one table key | non-zero, **PRE-REPLAY REFUSAL** (rule 4) |
| 6 | make one table value a float | non-zero, **PRE-REPLAY REFUSAL** (rule 1) |
| 7 | unknown `backdonateAuth.semantics` token | non-zero, **PRE-REPLAY REFUSAL** (semantics) |
| 8 | change `label` only | **exit 0** — proves `label` is not operative |
| 9 | restore byte-identical | exit 0 |

Row 4 must **truncate to a prefix**, never delete an interior entry. Deleting an
interior entry breaks contiguity, so §3 rule 3 would refuse it **before replay**
and the control would never reach the runtime abort it exists to test — passing
for the wrong reason. Truncation keeps the table valid, so validation succeeds and
the abort fires at query time, which is the property under test.

The truncation point is bound to the precondition's evidence: it must sit strictly
below an `n >= 1` the precondition proved is actually queried. Truncating below an
`n` nothing queries would be vacuous. **If the corpus exhibits no collective query
at `n >= 1`, report the corpus inadequate** for this control rather than
substituting a weaker one.

Rows 2-7 must each be demonstrated failing before they pass. Row 9 exists because a
mutation that fails to restore leaves the tree dirty and every later row judges the
wrong artifact.

---

## 8. `backdonateAuth` — conditional, anticipating nothing

Retained as a finite explicit encoding: an enumerated `semantics` token, today
`always-refuse`, refused if unrecognised.

**It is retired from this contract if and only if the accepted `#76`
implementation retires `BackdonateAuth` as an input.** Not before, not on
expectation. If `#76` lands keeping the input in any form, this field stays and
encodes whatever that form is, finitely and explicitly. No future semantics are
anticipated here.

---

## 9. Scope

- Does not touch `reactivegas.trace/v1`; does not widen `#74`'s wrapper; `#74`
  may ship without it.
- Ships with **`#75`**.
- Records what a corpus was generated under. **Selects nothing for the shipped
  coordinator**; the product default remains unruled. A finite table is test
  input.

## 10. What the `#75` implementation mandate must add

This contract fixes semantics. Two things it cannot fix, which the mandate must:

1. **Concretize the projection encoding.** §6 defines the digest as sorted
   `path` plus content SHA-256, concatenated and hashed once. The mandate fixes
   the exact byte encoding — separator, path normalization, hash-of-hashes
   framing — so two implementations cannot disagree while both believing they
   follow this document.

2. **Verify the projection contains every actual generator input.** §6's list —
   pins, the exporter, its transitive import closure — is an **intended**
   projection. It is not proof that generation reads nothing else: an external
   configuration file, an environment-dependent path, a script invoked by the
   build, or a data file opened at elaboration time would all be inputs and
   none is in that list. The mandate must establish the real input set, and
   **an added input that the projection omits must fail** — a control that
   introduces a new generation input and requires the gate to reject it while
   the projection is unchanged.

The independent table evaluation of §2 is preserved unchanged by both.

## Status

**R3.1.** The per-corpus question is settled (NOTE-007) and is not reopened
here. No product default is selected. No scope question about this artifact
remains open.
