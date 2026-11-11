# S4-B discovery boundary — original R7/R8, source finding before validation

To %503, for the same pending binding review through the owner. Own ACK.
No new execution or budget grant. This is not a candidate acceptance or an
executed control result.

I re-read R7/R8/R9 in the parent acceptance instrument and the actual checker:
R7 requires source -> compiled -> counterpart reconciliation; R8 requires a
newly introduced OWNED predicate to be detected through the mandatory path.
The checker currently:
- imports only KelGroups, Reactivegas, Reactivegas.Mirrors, KelGroups.Mirrors;
- treats home-module strings starting with KelGroups/Reactivegas as ownership.

Those are TWO boundaries, neither equivalent to owned project sources. An owned
module outside the imported closures is invisible before the kind classifier;
an imported owned module with another root spelling is filtered out afterwards.
The accepted S2R source inventory does not impose those two name prefixes.
Concrete existing example to inspect: lean/CorpusExport.lean is an owned
registered root outside those imports/prefixes. I make no claim that it currently
contains an omitted Prop-valued declaration. A well-typed control addition there
would exercise a boundary that adding only to Reactivegas.Predicates cannot.

Treat this as an original future-discovery requirement to adjudicate, not a new
unrelated feature or an invitation to narrow 'owned'. Verify source ownership,
import reach and the mandatory-path consequence independently; any executable
control needs the actual command-fit authorization before running. The two
prefixes must not become another fixed list standing in for discovery. Preserve
exclusion of toolchain/dependency definitions through actual provenance, not by
calling every loaded module owned or reducing ownership to the loaded subset.

This is relevant to the F01 discovery repair and final FULL audit. The already
repaired opaque-kind arm and current inventory evidence remain credited at their
scope. No new predicate, module or product semantics is requested in shipped
code. If the complete contract cannot fit the current repair plan, return one
consolidated exact gap including production isolation and this boundary before
expensive work; do not use a narrower passing specimen to close the general row.

## Desk correction after direct path and Lake readback — original example was wrong

I own an error in the concrete example above: there is NO lean/CorpusExport.lean.
The real file is lean/Reactivegas/CorpusExport.lean; lakefile.lean:17-18 registers
lean_exe corpusExport with root Reactivegas.CorpusExport. Thus it DOES satisfy
the current namespace-prefix test. My contrary claim inferred its path from the
executable name before reading Lake and is withdrawn, not a finding about code.

The example establishes only the FIRST boundary: Reactivegas.lean imports
Types/State/Step/Predicates/Invariants/Trace/Composition, and no project .lean
imports Reactivegas.CorpusExport. The exporter is therefore a registered owned
module outside the mirror driver's imported closure. This was checked by the
actual aggregator and full-project import search, not guessed from its name.
No omitted current Prop is asserted. The SECOND boundary remains a hypothetical
new owned module of a different root name, not an existing-module observation.
Do not relay the false path or treat the exporter as an executed outside-prefix
control. Reconcile the genuine import-closure boundary under original R7/R8;
there is no need to invent a new current omission or to narrow ownership.
