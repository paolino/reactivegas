# #90 module ownership

| ID | Package / path / module | Responsibility / dependencies |
|---|---|---|
| M90-CORE | economic-core/reactivegas-economic-core.cabal; economic-core/src/Reactivegas/Economic/Core.hs; Reactivegas.Economic.Core | Sole pure custody transition, closed event type, lossless identity, read-only query boundary and frame-carrying state. Depends only on base/text, optionally containers. Data D90 and signatures F90-CORE. |
| M90-ADAPTER | economic-kelgroups/reactivegas-economic-kelgroups.cabal; economic-kelgroups/src/Reactivegas/Economic/KelGroups.hs; Reactivegas.Economic.KelGroups | Production binding of core queries to the accepted KelGroups.Types API. No economic policy or membership store. Depends on core and pinned kelgroups. F90-ADAPTER. |
| M90-TEST | economic-kelgroups/test/Main.hs, MoneyCustodySpec.hs, CorpusSpec.hs, MutationSpec.hs | Permanent executable test component `money-custody-tests`, registering every module. Full direct behavior/frame/query cases, partial stored-input corpus replay, compiled query-guard controls. JSON dependencies stay here. |
| M90-DOC | docs/money-custody.md | User-facing API usage, exact supported operations, refusal and corpus boundary, discovered extent, direct coverage and residual limits. |
| M90-BUILD | cabal.project; flake.nix/lock; nix/project.nix; justfile; CI workflow | Declare/build the actual packages and run the tests additively in mandatory CI. |

Dependency direction: tests and adapter consume core; adapter consumes
KelGroups.Types from its accepted upstream pin. Pure core does not consume
adapter, JSON or the legacy server. Later economic integration adds behavior
to this core rather than introducing a parallel implementation. No upstream
change is needed: GroupView and its queries are already accepted.
