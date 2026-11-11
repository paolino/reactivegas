# Spec — #71 design record rewritten to the merged model, with a citation checker that fails closed

Version 2 (2026-11-11). Supersedes version 1 (base e6c5924), whose model facts
predate #68, #76, #81, #66, #69, #70 and #75. Version 1 stays in git history as
evidence only.

Base: origin/master ddb1703, merged into `docs/71-design-record` at 63e686c.
Class: documentation plus a new hosted check. No Lean model, proof, simulator or
Haskell change.

## Outcome

`docs/en/design/` describes the model on master as it is, every claim that names
a Lean declaration is a citation the hosted CI resolves against the built Lean
library, and that check goes red when a citation is stale, when a declaration is
named without a citation, or when the Lean library cannot be imported.

## Requirements — the record

Each requirement is satisfied by prose that cites the declarations it rests on.
Where master and an issue text disagree, master wins; the record never reports an
issue's wording as the model.

- **record-describes-state**: the economic state and where membership lives —
  a single writable membership store in the KelGroups group state, read by the
  application only through the read-only group view; no second membership copy.
- **record-describes-events**: the economic event constructors and the
  application event constructors exactly as on master, counted from the source;
  the retired constructors (`addUser`, `electResponsabile`, `removeResponsabile`,
  the economic `removeMember`) named as retired, distinguished from the KelGroups
  membership mutations that remain.
- **record-describes-routes**: the route classifier inventory and the sealed base
  transition hook with each base change it handles.
- **record-describes-authority**: who may sign each event and under which guard,
  as on master, including that a pledge is signed by its owner, that the owner of
  a pending pledge corrects or withdraws it, and that the referente corrects an
  accepted one.
- **record-describes-proposer-not-assent**: a question opens with zero approvals;
  the proposer's opening is not an assent.
- **record-describes-vote-closure-effects**: grant, deny and backdonate take their
  economic effect only from a closed vote, and what each closure binds
  (collection identity for permission, the per-member share for backdonation).
- **record-describes-vote-lifecycle**: renounce or departure of the proposer
  closes the question negatively and the escrow refunds on that negative closure;
  a renounce by anyone other than the proposer and a ballot on a permission
  question by anyone other than the named designee are refused and change no
  state.
- **record-describes-threshold**: the threshold is a parameter of the model; the
  group sets an explicit policy at setup; there is no shipped default.
- **record-describes-corpora**: the frozen corpora on master (economic,
  integrated, vote) and the finite-history validation, what each checks and how
  CI re-verifies them.
- **record-separates-law-from-witness**: every claim people read says whether it
  rests on a universally quantified theorem or on a finite check evaluated to
  `true`; no claim of model completeness the source does not prove.
- **record-drops-false-closure-claim**: the claim that zero member balances plus
  no open collections make the cash boxes sum to zero is absent; group closure is
  described only by what master defines and proves about it.
- **record-carries-voci-non-goal**: a section stating the `Voci/` catalogue as an
  explicit non-goal: the fact (21 modules under `Voci/` plus
  `ImpegnoVincolato` / `CorrezioneImpegnoVincolato`, order-bound pledges; the
  Lean models none of it), the ruling (out of scope for milestone 2, deferred to
  milestone 3, issue #91), the reason it is written down rather than omitted,
  the cost curve (cheapest now; after the conformance corpus freezes it costs a
  corpus re-freeze plus an application-fold rewrite), and the open question
  carried open, not resolved: does a group reach the outcome test (election →
  collection → pledge → assent → purchase → refund) without a catalogue, or is
  the catalogue absent from the Lean because nobody asked?
- **record-states-no-pending-as-pending**: nothing that has landed on master is
  described as pending or ruled-but-unimplemented; anything still unimplemented
  is named as such with the declaration or its absence.
- **record-is-plain-published-prose**: no internal process vocabulary (desk,
  lane, owner, ruling identifiers such as Q-NNN or NOTE-NNN), no `.lean:<line>`
  pins; declarations are cited by name.

## Requirements — the citation checker

- **citations-resolve**: every citation in every Markdown file under
  `docs/en/design/` resolves to a declaration of the built Lean library on the
  checked-out tree. An unresolved citation exits non-zero and names file, line
  and citation.
- **uncited-declaration-is-red**: a code span in the record whose text names a
  declaration of the built Lean library without the citation marker exits
  non-zero and names it. The rule deciding when a code span names a declaration
  is stated in the record itself and decided by Lean, not by a list.
- **discovered-extent-only**: the checker discovers the record files and the
  citations by scanning; the set of known declarations comes from the Lean
  environment. No hand-kept list of files, citations or declarations exists.
- **fails-closed-on-cold-tree**: when the Lean import prerequisite fails (library
  not built, module missing), the checker exits non-zero with a message naming
  the import failure. It never exits zero without having resolved every
  discovered citation, and it exits non-zero when it discovers zero citations.
- **controls-are-hosted**: a hosted CI step shows the checker red on (a) a cold
  tree, through the real import failure path, and (b) a stale citation and (c) an
  uncited declaration, each through the real checker on a fixture copy of the
  record; and green on the real record.
- **ci-placement**: the checker runs in the "Build and check" job (after the Lean
  build) or in a job of its own; never in the "Lean mutant ledger" job; no mutant
  ledger rows are added. The PR body states its measured hosted cost.

## Non-goals

No Lean model, proof or mutant change. No simulator or Haskell change. No
threshold default. No `Voci/` modelling. No change outside `docs/en/design/`,
the checker and its fixtures, the justfile recipe, the CI workflow and this
spec directory.
