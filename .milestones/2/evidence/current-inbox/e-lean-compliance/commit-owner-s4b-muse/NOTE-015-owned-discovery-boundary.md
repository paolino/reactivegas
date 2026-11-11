# NOTE-015 — two discovery boundaries, neither equal to "owned project sources"

Same pending binding review. **No new execution or budget grant.** Not a
candidate acceptance and not an executed control result.

R7 requires source → compiled → counterpart reconciliation; R8 requires a newly
introduced **owned** predicate to be detected **through the mandatory path**. The
checker enforces **two different things**, and neither is "owned project source":

```
scripts/check-lean-mirrors:48-51   import KelGroups
                                   import Reactivegas
                                   import Reactivegas.Mirrors
                                   import KelGroups.Mirrors
scripts/check-lean-mirrors:204     if !(home.startsWith "KelGroups" || home.startsWith "Reactivegas") then
```

- **Boundary 1 — import reach.** An owned module outside those closures never
  enters `env.constants`, so it is **invisible before the kind classifier ever
  runs**. The repaired opaque-kind arm cannot help: there is nothing to classify.
- **Boundary 2 — name spelling.** An imported owned module whose home string
  starts differently is **filtered out afterwards**, at line 204.

**The accepted S2R source inventory imposes neither prefix.**

## Correcting the example, and sharpening it

The cited path `lean/CorpusExport.lean` **does not exist**. The actual module is:

```
lean/Reactivegas/CorpusExport.lean          -- owned source
lakefile.lean:17-18   lean_exe corpusExport
                        root := `Reactivegas.CorpusExport
```

Verified consequence — and it is **boundary 1, not boundary 2**:

- it **is** owned and registered, and is inside the S2R tracked inventory;
- its home `Reactivegas.CorpusExport` **passes** the line-204 prefix test;
- but `lean/Reactivegas.lean` imports Types, State, Step, Predicates, Invariants,
  Trace and Composition — **not `CorpusExport`** — and no other checker-imported
  umbrella reaches it.

**So it is an owned, registered module outside the checker's import closure.** A
well-typed Prop-valued control added there would exercise a boundary that adding
to `Reactivegas.Predicates` **cannot** — because it is invisible before
classification, not misclassified after it.

**No claim is made that it currently contains an omitted Prop-valued
declaration.** I did not add one and did not run anything.

Boundary 2 has **no live example today** — with `lean_lib Reactivegas` and
`lean_lib KelGroups` both at `srcDir := "."`, every owned module currently sits
under those two namespaces. It remains a **latent hole**, and worth naming
plainly: **a fixed pair of name prefixes standing in for discovery is the F-001
defect class that S2R removed from the axiom gate.** Do not let those two
prefixes become another fixed list.

## What is required

**Adjudicate this as an original future-discovery requirement** — not a new
feature, and **not an invitation to narrow "owned"**. Verify **source ownership,
import reach and the mandatory-path consequence** independently of this note.

**Preserve exclusion of toolchain and dependency definitions through actual
provenance** — not by calling every loaded module owned, and not by reducing
ownership to the loaded subset.

**Do not use a narrower passing specimen to close the general row.** A specimen
in `Predicates` passing says nothing about a module the checker never imports.

Any executable control here needs **actual command-fit authorization before
running**. The repaired opaque-kind arm and the current inventory evidence remain
**credited at their scope**. **No new predicate, module or product semantics is
requested in shipped code.**

## If it does not fit

**Return ONE consolidated exact gap — covering both the production-isolation
problem from NOTE-014 and this discovery boundary — before any expensive work.**
Not two separate partial gaps, and not after the phase.
