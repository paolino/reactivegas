# Epic #43 requirement → ruling/issue map

Epic owner `claude-opus-5[1m]`, for desk `%510`, per NOTE-012. 2026-09-05.
Source: open issue #43 body, read in full. **Administrative reconciliation. No
implementation dispatched. #43 remains unrespawned.**

Rule applied throughout, from NOTE-012: **a requirement is superseded only by an
exact later ruling.** Absence from a newer summary is not cancellation, and
neither is a coordinator-only test passing.

## The finding that frames the rest

The milestone's own description — not #43's — sets the completion boundary:

> *"One pure Haskell economic core runs natively in the coordinator and as
> wasm32-wasi in the browser client; a CLI talks to the same server. Multi-gruppo
> = multiple instances (MVP). **Done when a released coordinator + browser client
> + CLI implement the laws end-to-end for one gruppo.** Live-gruppi migration is
> a follow-on milestone."*

So **the six-step stranger test through the coordinator is necessary and not
sufficient** for this milestone as written. Browser client and CLI are inside the
boundary. Until today neither had a ticket. That is the planning gap NOTE-012
identified, and it is real.

## The map

| # | #43 requirement | disposition | authority / owner |
|---|---|---|---|
| R1 | *"The legacy design is the specification"* | **SUPERSEDED** | #45 landed the Lean machine as the semantic source of truth, and the standing rule is "the Lean is the specification; the legacy server is evidence about what the group actually did". #62 then changed the substrate mapping, superseding #47. |
| R2 | sovereign members; self-service pledge, referente acceptance unchanged | **STANDS** | #69 |
| R3 | responsabili may declare for a keyless member | **STANDS** | inside #69's scope — the Lean's `pledge` already separates `author` from `user` |
| R4 | deposits stay responsabile-authored | **STANDS, implemented** | Lean `deposit` demands `isResponsabile signer` and `signer ≠ user`; D3 conformance covers it |
| R5 | multi-gruppo = multiple server instances (MVP) | **STANDS** | a boundary, not a ticket. D4 exposes one gruppo per instance and must not grow a multi-tenant surface. |
| R6 | Lean is source of truth; the Haskell core is its **one** implementation, QuickCheck mirroring the theorems | **STANDS** | #67 **D2/D3**, qualified by the standing ruling against a blanket mirror-count quota and against manufacturing decidability for arbitrary reachability |
| R7 | substrate contract kelgroups `#28`/`#30` | **STANDS** | #73 |
| R8 | responsabili = kelgroups admins; referente is per-collection app state; no app roles | **STANDS, implemented** | in the Lean; D2 must not introduce a second role store |
| R9a | core compiled **native in the coordinator** | **STANDS** | #67 D2/D3, exposed by D4 |
| R9b | core compiled **wasm32-wasi** | **STANDS — was uncovered** | **#82** (new) |
| R9c | **browser client**, PureScript shell over `core.wasm` | **STANDS — was uncovered.** The shell exists as a package inside `paolino/kelgroups`; the Reactivegas UI and the `core.wasm` boundary do not. | **#84** — corrected section below |
| R9d | **CLI**, native, HTTP to the same server | **STANDS — was uncovered** | **#83** (new) |
| R10 | migration of the live 2018 gruppi | **STANDS as out of scope** | follow-on milestone |
| R11 | runnable artifact is *"`kelgroups-server` with the reactivegas app plugged in"*; browser and CLI ride the same release | **STANDS.** Architecture was ruled; only a delivery/packaging choice remains | corrected section below |

### #43's ordered children, reconciled

| #43 child | now |
|---|---|
| #45 Lean extraction | **merged** |
| substrate-mapping design doc | #47, **closed**, superseded by #62's mapping change; the record rewrite is #71 |
| Haskell economic core | #67 D2/D3 |
| wasm32-wasi build | **#82** |
| Coordinator (epic artifact) | #67 D4 |
| Browser client | **#84** |
| CLI client | **#83** |

## R9c — CORRECTED 2026-09-05: no third repository. The original claim was wrong.

**What this document said, and it was false:** that `paolino/kelgroups-client`
does not exist and R9c therefore entails a third repository requiring a
cross-repository ruling.

**The error, named:** absence of a standalone *repository* was treated as
absence of the *component*. The checks run were `gh repo view
paolino/kelgroups-client` and `gh repo list paolino`. The kelgroups tree itself
was never opened. That is the same inference shape this milestone has been
correcting elsewhere, made here by me.

**Measured** at `paolino/kelgroups` HEAD `368b596fef0b6d393c2ac7afc631d236c55d86d1`,
clean working tree:

```
client/kelgroups-client/spago.yaml                   package name: kelgroups-client
client/kelgroups-client/src/KelGroups/Client/Api.purs        postEvent, getEvents, getInfo
client/kelgroups-client/src/KelGroups/Client/{Codec,Event,Fold,Jwk,Message,State,Types}.purs
client/kelgroups-client/src/FFI/{Fetch,SSE,Storage,KeyBytes}.{purs,js}
client/kelgroups-client/test/{FoldSpec,Generators,InvariantsSpec,JwkSpec,TransitionInvariantsSpec}.purs
client/kelgroups-trivial/{spago.yaml,src/Main.purs,src/View/{App,Bootstrap,Members}.purs}
nix/client-bundle.nix
```

`kelgroups-client` is a **package inside `paolino/kelgroups`**. No third
repository is entailed; none needs creating; the escalation is withdrawn.

### Reconciled against #43's shell requirement — bounded evidence

| #43 element | present | evidence |
|---|---|---|
| keys | **yes** | `Client/Jwk.purs`, `FFI/KeyBytes` |
| transport | **yes** | `Client/Api.purs`, `FFI/{Fetch,SSE,Storage}` |
| a UI shell | **yes, for the base group** | `kelgroups-trivial` views: App, Bootstrap, Members |
| packaging | **yes** | `nix/client-bundle.nix` from `client/spago.lock` |
| **the Reactivegas UI** | **no** | no conti, casse, collections, pledges or assenso anywhere in the views |
| **`core.wasm` integration** | **no** | no loader, no FFI; the only occurrence of the string in the repository is inside `client/spago.lock` |

**The package existing does not supply the Reactivegas UI or wasm integration**,
and nothing here claims it does.

### The residual that matters

`Client/Fold.purs` is a PureScript reimplementation of the base fold —
`foldGroup`, `applyEvent`, `tryEnact`, `enact`, `proposalDigest` — over a
`GroupState a` with `PendingProposal`, the historical store shape.

So a **second implementation of base semantics already exists, in a second
language**, and #43 requires semantics to come from `core.wasm`. Adding wasm
beside a fold that still decides is the semantic fork #82's gate exists to
prevent, arriving through the client rather than the build. Resolving that —
retire the PureScript fold, or scope it explicitly and gate the scope — is the
substance of #84.

### Where the additional work belongs

| change | repository | authorized |
|---|---|---|
| wasm FFI; retire or scope `Client/Fold.purs`; extend `Api.purs` for app events | `paolino/kelgroups` | **no.** #73's proposed ownership is limited to `#28`/`#30`; client work is not dispatched and NOTE-014 grants none |
| the Reactivegas UI | undecided — a Reactivegas-side view package, or an app view layer in the kelgroups client tree | undecided, not dispatched |
| `core.wasm` | `paolino/reactivegas` | #82 |

## R11 — CORRECTED: architecture was ruled; only a delivery choice remains

**What this document said, and it overreached:** that #43's *"`kelgroups-server`
with the reactivegas app plugged in"* versus the existing reactivegas `server`
bundle is an unresolved architecture question.

**It is not.** The operator ruled the Haskell implementation on kelgroups as the
backend. A working legacy release does not reopen that, and a
package-versus-repository naming difference is not a product ruling.

**The extension surface, measured** at kelgroups `368b596`:

- the library is polymorphic — `GroupConfig a`, `AppFold a`, `GroupState a`;
- `app/Main.hs` of the `kelgroups-server` executable imports
  `KelGroups.Trivial (trivialConfig, trivialFold, trivialInitial)`.

So an application is supplied **at link time in an executable's `Main`**, not
through configuration or a plugin. "kelgroups-server with the reactivegas app
plugged in" concretely means *an executable that links the kelgroups library
with the Reactivegas app instead of the trivial one*.

**The one concrete choice that remains is delivery, not architecture:**

1. **Which package produces that executable** — a reactivegas cabal package
   depending on the kelgroups library, or an executable in kelgroups. The first
   is the natural reading given #73's limited scope, but it is a choice with
   cross-repo consequences and it has not been made.
2. **What the released artifact is called and where its entrypoint lives.**
   Reactivegas today releases `reactivegas-server-<tag>-linux-x86_64.tar.gz`
   with `bin/server`. When D4 lands, that entrypoint becomes the
   kelgroups-backed coordinator. Keeping the name and path leaves #51's
   release-identity work and the D5 stranger fetch-and-run steps unchanged;
   changing them touches both.

Both are packaging and release-identity questions. Recorded as measured
residuals for whoever owns D4 and #51 — **not escalated as a product ruling.**

## Boundaries preserved, as NOTE-012 required

- **Multi-gruppo MVP is multiple instances.** No multi-tenant surface in D4.
- **Live 2018 gruppi migration is out of scope**, a follow-on milestone.
- **#70's current JS acceptance is unchanged.** The historical desk record keeps
  `core.wasm` as the *planned* simulator backend replacement while accepting the
  current JS transcription with conformance as the interim review surface. #82
  builds the wasm core; it does **not** replace the simulator, and nothing here
  authorizes that.
- **#43 remains unrespawned.**

## Incidental observation

`/code/kelgroups-issue-28` exists as a local worktree on branch
`feat/28-generalize-app-api`, but its HEAD is `6ec0248` — a merge of kelgroups
PR #27, behind that repository's current `368b596`, with no commits of its own.
Consistent with the D1 finding that `#28` is unstarted. Recorded so nobody
mistakes the branch's existence for work in progress.
