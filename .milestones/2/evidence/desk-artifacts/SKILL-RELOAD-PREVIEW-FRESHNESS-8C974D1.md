# Skill reload — operator review preview freshness

Shared factory skills advanced to commit
`8c974d1cba2ab89c99e2dd52aeba51727f118ab9`.

At your next safe supervision boundary, reload the installed versions of:

- `documentation` when your lane owns browser-visible documentation or an
  interactive artifact;
- `resolve-ticket` for ticket lifecycle;
- your existing orchestrator role contract.

The changed invariant is narrow and binding for browser-facing review surfaces:
open a draft PR at the first coherent candidate, publish through the shared PR
preview on every push, bind the live served artifact to the PR head, state
unaccepted status honestly, clean it when the PR closes, and move the accepted
artifact to the default-branch site after landing.  Localhost, SSH tunnels,
workflow downloads, and stale accepted pages are not operator previews.

This does not amend product semantics, current audit mandates, candidate
acceptance, budgets, model/family fences, or merge authority.  Do not interrupt
an in-flight build/audit.  Existing non-browser lanes only acknowledge the
reload; they do not manufacture preview work.

Propagate this exact invariant through immediate child supervisors only where
they own a current or future browser-facing artifact.  Upward reporting remains
local files only; never write to the operator's composer.

Acknowledge in your own `STATUS.md` with one `NOTE` whose text starts
`SKILL-RELOAD 8c974d1` and says whether your lane has a browser review surface.

## Correction recorded after first delivery

The first version of the preceding sentence incorrectly named `SKILL-RELOAD`
as an event tag.  It is not in the worker-protocol tag vocabulary; using it
would violate the same no-invented-tags rule this factory relies on for waits.
The valid tag is `NOTE`, with `SKILL-RELOAD` in the event body as shown above.
Any already-written `SKILL-RELOAD` line remains append-only history and does
not establish a new state transition.
