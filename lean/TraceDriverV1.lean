import Lean
import Reactivegas.Step
import KelGroups.Integration

/-!
# Integrated trace producer (production root)

Emits the frozen `reactivegas-integrated.trace` version-1 seed envelopes
embedded by `economics-simulator.html` as `LEAN_TRACES_V1`. Every step is a
SIGNED INTEGRATED EVENT through the sole production root
`Reactivegas.apply` — the same boundary the running system uses — so each
recorded state is the whole canonical aggregate (members, pending base,
payload) after a transition in which the sealed base hook ran exactly once:
economic cleanup and the vote recompute, hook-driven closures without any
ballot included. Nothing is derived from supplied views and nothing is
hand-written; post-states come from the production root only.

Reproduce from a clean checkout with:

```sh
cd lean && lake env lean TraceDriverV1.lean
```

(`economics-simulator-trace-gate.mjs` at the repository root runs exactly
that, compares the fresh output against the embedded fixture, and replays it
through the page's production JavaScript.)

Traces A and B contain applied steps only: if any of their seeded steps is
refused, the driver throws instead of emitting a usable-looking corpus. Trace C
(the #81 V-5 closures and S-12 refusals) seeds each step with its expected
outcome and records a refused step with the production error and an unchanged
aggregate; an outcome other than the seeded one throws in the same way.
-/

open Lean (ToJson toJson Json)
open KelGroups (GroupState Key Key IntegratedEvent BaseChange BaseMutation Member Role Admin)
open Reactivegas (ProductionError)

namespace TraceDriverV1

deriving instance Lean.ToJson for KelGroups.Admin
deriving instance Lean.ToJson for KelGroups.Role
deriving instance Lean.ToJson for KelGroups.Member
deriving instance Lean.ToJson for KelGroups.BaseChange
deriving instance Lean.ToJson for KelGroups.BaseMutation
deriving instance Lean.ToJson for KelGroups.Vote.Verdict
deriving instance Lean.ToJson for KelGroups.Vote.Ballot
deriving instance Lean.ToJson for KelGroups.Vote.QuestionKind
deriving instance Lean.ToJson for KelGroups.Vote.ClosureCause
deriving instance Lean.ToJson for KelGroups.Vote.Question
deriving instance Lean.ToJson for KelGroups.Vote.ClosureRecord
deriving instance Lean.ToJson for KelGroups.Vote.VoteState
deriving instance Lean.ToJson for Pledge
deriving instance Lean.ToJson for Collection
deriving instance Lean.ToJson for State
deriving instance Lean.ToJson for StepError
deriving instance Lean.ToJson for KelGroups.ValidationError
deriving instance Lean.ToJson for KelGroups.IntegratedError

/-- The restricted Reactivegas proposal in the consumer's shape. -/
def jsonProposal : Proposal → Json
  | .departure key => Json.mkObj [("departure", toJson key)]
  | .changeRoles key roles =>
      Json.mkObj [("changeRoles", Json.mkObj [("key", toJson key), ("roles", toJson roles)])]

/-- App events in the consumer's shape: the vote constructors ride the same
app route the Lean appFold uses. -/
def jsonAppEvent : AppEvent → Json
  | .openPurchase c => Json.mkObj [("openPurchase", Json.mkObj [("c", toJson c)])]
  | .grantPermission c => Json.mkObj [("grantPermission", Json.mkObj [("c", toJson c)])]
  | .denyPermission c => Json.mkObj [("denyPermission", Json.mkObj [("c", toJson c)])]
  | .deposit u v => Json.mkObj [("deposit", Json.mkObj [("user", toJson u), ("v", toJson v)])]
  | .withdraw u v => Json.mkObj [("withdraw", Json.mkObj [("user", toJson u), ("v", toJson v)])]
  | .transferCassa f v => Json.mkObj [("transferCassa", Json.mkObj [("from_", toJson f), ("v", toJson v)])]
  | .donate v => Json.mkObj [("donate", Json.mkObj [("v", toJson v)])]
  | .backdonate w => Json.mkObj [("backdonate", Json.mkObj [("w", toJson w)])]
  | .pledge u c v => Json.mkObj [("pledge", Json.mkObj [("user", toJson u), ("c", toJson c), ("v", toJson v)])]
  | .acceptPledge u c => Json.mkObj [("acceptPledge", Json.mkObj [("user", toJson u), ("c", toJson c)])]
  | .refusePledge u c => Json.mkObj [("refusePledge", Json.mkObj [("user", toJson u), ("c", toJson c)])]
  | .correctPledge u c v => Json.mkObj [("correctPledge", Json.mkObj [("user", toJson u), ("c", toJson c), ("v", toJson v)])]
  | .closePurchase c => Json.mkObj [("closePurchase", Json.mkObj [("c", toJson c)])]
  | .failPurchase c => Json.mkObj [("failPurchase", Json.mkObj [("c", toJson c)])]
  | .openQuestion qid kind =>
      Json.mkObj [("openQuestion", Json.mkObj [("questionId", toJson qid), ("kind", toJson kind)])]
  | .cast qid ballot =>
      Json.mkObj [("cast", Json.mkObj [("questionId", toJson qid), ("ballot", toJson ballot)])]
  | .renounce qid => Json.mkObj [("renounce", Json.mkObj [("questionId", toJson qid)])]

/-- The integrated event in the consumer's shape. -/
def jsonIntegratedEvent : KelGroups.IntegratedEvent Proposal AppEvent → Json
  | .direct (.admitMember key email roles) =>
      Json.mkObj [("direct", Json.mkObj [("admitMember",
        Json.mkObj [("key", toJson key), ("email", toJson email), ("roles", toJson roles)])])]
  | .propose p => Json.mkObj [("propose", Json.mkObj [("proposal", jsonProposal p)])]
  | .approve pid => Json.mkObj [("approve", Json.mkObj [("proposalId", toJson pid)])]
  | .app ae => Json.mkObj [("app", jsonAppEvent ae)]

/-- The toy aggregate, serialized in the consumer's shape: members, pending
base (id + proposal + proposer + approvals) and the app payload. -/
def aggJson (gs : GroupState State) : Json :=
  Json.mkObj
    [ ("members", toJson gs.members)
    , ("pendingBase", Json.arr (gs.pendingBase.map fun e =>
        Json.arr #[toJson e.1,
          Json.mkObj [("mutation", toJson e.2.mutation),
            ("proposer", toJson e.2.proposer),
            ("approvals", toJson e.2.approvals)]]).toArray)
    , ("payload", toJson gs.appFold) ]

/-- One seeded signed integrated event. -/
abbrev Seed := Key × KelGroups.IntegratedEvent Proposal AppEvent

/-- Fold seeds through the production root; the first refusal aborts with
its index and the production error, so a broken seed never emits a partial
corpus and the operator sees exactly which signed step failed. -/
def runSeeds? (gs : GroupState State) (i : Nat) (seeds : List Seed) :
    Except String (List Json) :=
  match seeds with
  | [] => .ok []
  | (signer, ev) :: rest =>
      match Reactivegas.apply KelGroups.Vote.legacyThreshold
          Reactivegas.probeAuth gs signer ev with
      | .ok res =>
          match runSeeds? res.state (i + 1) rest with
          | .ok tail =>
              .ok (Json.mkObj
                [("input", aggJson gs), ("signer", Json.str signer),
                 ("event", jsonIntegratedEvent ev),
                 ("result", Json.mkObj [("tag", "applied"),
                   ("aggregate", aggJson res.state),
                   ("change", match res.change with
                     | some (.memberAdmitted k) => Json.mkObj [("memberAdmitted", toJson k)]
                     | some (.memberRemoved k) => Json.mkObj [("memberRemoved", toJson k)]
                     | some (.rolesChanged k) => Json.mkObj [("rolesChanged", toJson k)]
                     | none => Json.null)])] :: tail)
          | .error e => .error e
      | .error e =>
          .error s!"passo {i} ({signer}): il production root ha rifiutato — {repr e}"
termination_by seeds.length

def adminRoles : List KelGroups.Role := [.adminRole .publicAdmin]
def socioRoles : List KelGroups.Role := [.appRole "socio"]

def foundedMember : KelGroups.Member :=
  { key := "anna", email := "anna@toy.example", roles := adminRoles }

def admit (key : Key) : KelGroups.IntegratedEvent Proposal AppEvent :=
  .direct (.admitMember key (key ++ "@toy.example") socioRoles)
def elect (key : Key) : KelGroups.IntegratedEvent Proposal AppEvent :=
  .propose (.changeRoles key adminRoles)
def appE (ae : AppEvent) : KelGroups.IntegratedEvent Proposal AppEvent := .app ae
def propose (p : Proposal) : KelGroups.IntegratedEvent Proposal AppEvent := .propose p
def approve (pid : KelGroups.ProposalId) : KelGroups.IntegratedEvent Proposal AppEvent := .approve pid

/-- Trace A: the full one-membership journey through the production root —
direct admission, an election the founder proposes and then approves (a
proposal opens with no assent, the sole admin's separate approval enacts),
double-entry deposit, a purchase with pledges in flight, a third admin so the
threshold is 2, an open question at 1/2, and the departure of the admin
referente PENDING at 1/2 after the first assent until the approving vote:
wind-up of his open collections, refund of every pledge, absorption of his
conto into the comune, and the hook's vote sweep closing the open question at
the new threshold WITHOUT any further ballot — all inside the transition of
the approving vote. -/
def traceA : List Seed := [
  ("anna", admit "bruno"),
  ("anna", elect "bruno"),
  ("anna", approve "roles:bruno"),
  ("anna", appE (.deposit "bruno" 100)),
  ("bruno", appE (.openPurchase 10)),
  ("anna", appE (.pledge "bruno" 10 30)),
  ("bruno", appE (.acceptPledge "bruno" 10)),
  ("bruno", appE (.openPurchase 11)),
  ("anna", appE (.pledge "bruno" 11 20)),
  ("anna", admit "elena"),
  ("anna", elect "elena"),
  ("bruno", approve "roles:elena"),
  ("anna", appE (.openQuestion "q:sconto" .collective)),
  ("anna", appE (.cast "q:sconto" .assent)),
  ("anna", propose (.departure "bruno")),
  ("bruno", approve "depart:bruno"),
  ("elena", approve "depart:bruno")
]

/-- Trace B ... -/
def traceB : List Seed := [
  ("anna", admit "bruno"),
  ("anna", elect "bruno"),
  ("anna", approve "roles:bruno"),
  ("anna", appE (.deposit "bruno" 50)),
  ("bruno", appE (.deposit "anna" 25)),
  ("bruno", appE (.openPurchase 7)),
  ("anna", appE (.pledge "bruno" 7 20)),
  ("bruno", appE (.acceptPledge "bruno" 7)),
  ("bruno", appE (.correctPledge "bruno" 7 35)),
  ("bruno", appE (.correctPledge "bruno" 7 5)),
  ("anna", appE (.openQuestion "q:permesso:7" .collective)),
  ("anna", appE (.cast "q:permesso:7" .assent)),
  ("anna", appE (.grantPermission 7)),
  ("bruno", appE (.closePurchase 7)),
  ("bruno", appE (.openPurchase 8)),
  ("bruno", appE (.pledge "bruno" 8 10)),
  ("bruno", appE (.acceptPledge "bruno" 8)),
  ("anna", appE (.pledge "anna" 8 15)),
  ("anna", appE (.denyPermission 8)),
  ("bruno", appE (.openPurchase 9)),
  ("anna", appE (.pledge "bruno" 9 10)),
  ("anna", propose (.changeRoles "bruno" socioRoles))
]

/-- One seeded signed integrated event with its expected outcome (trace C):
`true` = the production root applies it, `false` = it refuses it. -/
abbrev CheckedSeed := Key × KelGroups.IntegratedEvent Proposal AppEvent × Bool

/-- The production refusal in the consumer's shape (derived `ToJson`). -/
def errorJson : ProductionError → Json
  | .comuneReserved => Json.str "comuneReserved"
  | .integrated err => Json.mkObj [("integrated", toJson err)]

/-- Fold checked seeds through the production root. An applied step records
the post-aggregate exactly as `runSeeds?`; a refused step records the
production error and leaves the aggregate as it was (the next step's input
is this step's input). An outcome other than the seeded one aborts with its
index, so a broken seed never emits a partial corpus. -/
def runChecked? (gs : GroupState State) (i : Nat) (seeds : List CheckedSeed) :
    Except String (List Json) :=
  match seeds with
  | [] => .ok []
  | (signer, ev, expectApplied) :: rest =>
      match Reactivegas.apply KelGroups.Vote.legacyThreshold
          Reactivegas.probeAuth gs signer ev with
      | .ok res =>
          if !expectApplied then
            .error s!"passo {i} ({signer}): applicato dove era atteso un rifiuto"
          else
            match runChecked? res.state (i + 1) rest with
            | .ok tail =>
                .ok (Json.mkObj
                  [("input", aggJson gs), ("signer", Json.str signer),
                   ("event", jsonIntegratedEvent ev),
                   ("result", Json.mkObj [("tag", "applied"),
                     ("aggregate", aggJson res.state),
                     ("change", match res.change with
                       | some (.memberAdmitted k) => Json.mkObj [("memberAdmitted", toJson k)]
                       | some (.memberRemoved k) => Json.mkObj [("memberRemoved", toJson k)]
                       | some (.rolesChanged k) => Json.mkObj [("rolesChanged", toJson k)]
                       | none => Json.null)])] :: tail)
            | .error e => .error e
      | .error err =>
          if expectApplied then
            .error s!"passo {i} ({signer}): il production root ha rifiutato — {repr err}"
          else
            match runChecked? gs (i + 1) rest with
            | .ok tail =>
                .ok (Json.mkObj
                  [("input", aggJson gs), ("signer", Json.str signer),
                   ("event", jsonIntegratedEvent ev),
                   ("result", Json.mkObj [("tag", "refused"),
                     ("error", errorJson err)])] :: tail)
            | .error e => .error e
termination_by seeds.length

/-- Trace C: the V-5 lifecycle and S-12 refusals (#81) through the production
root, on the departure fixture of `Reactivegas.Lifecycle` (five
responsabili, `legacyThreshold 5 = 3`, `legacyThreshold 4 = 2`) reached from
the founded aggregate by signed admissions and elections. `qc` closes by
tally in the setup, so the closure log is non-empty before any V-5 closure.
`carlo`'s ballot on `dora`'s permission question addressed to `bruno` is
refused (`notDesignee`), `anna`'s renounce of `dora`'s `qd1` is refused
(`notProposer`); both leave the aggregate unchanged. Elections follow the
proposer-is-not-an-assent rule: the founder alone approves her own first
proposal; above one admin the assents come from the others, and `anna`'s
approval of her own proposal for `dora`'s departure is refused
(`proposerSelfApproval`) with the aggregate unchanged. `carlo` renounces his own
`qz`: it closes `.negative`/`.renounced` and every other open question stays
as it stood. Then `dora` leaves: in the transition of the enacting approval
her `qd1` and `qd2` close `.negative`/`.proposerDeparted`, and `anna`'s `qx`,
whose stale tally (`anna`, `dora`) crosses the four-responsabile threshold,
closes `.positive`/`.franchiseChange`; `bruno`'s `qy` stays open. -/
def traceC : List CheckedSeed := [
  ("anna", admit "bruno", true), ("anna", elect "bruno", true),
  ("anna", approve "roles:bruno", true),
  ("anna", admit "carlo", true), ("anna", elect "carlo", true),
  ("bruno", approve "roles:carlo", true),
  ("anna", admit "dora", true), ("anna", elect "dora", true),
  ("bruno", approve "roles:dora", true), ("carlo", approve "roles:dora", true),
  ("anna", admit "elena", true), ("anna", elect "elena", true),
  ("bruno", approve "roles:elena", true), ("carlo", approve "roles:elena", true),
  ("elena", appE (.openQuestion "qc" .collective), true),
  ("bruno", appE (.cast "qc" .dissent), true),
  ("carlo", appE (.cast "qc" .dissent), true),
  ("elena", appE (.cast "qc" .dissent), true),
  ("dora", appE (.openQuestion "qd1" .collective), true),
  ("dora", appE (.cast "qd1" .assent), true),
  ("anna", appE (.cast "qd1" .assent), true),
  ("dora", appE (.openQuestion "qd2" (.permission "bruno")), true),
  ("anna", appE (.openQuestion "qx" .collective), true),
  ("anna", appE (.cast "qx" .assent), true),
  ("dora", appE (.cast "qx" .assent), true),
  ("bruno", appE (.openQuestion "qy" .collective), true),
  ("carlo", appE (.cast "qy" .dissent), true),
  ("carlo", appE (.cast "qd2" .assent), false),
  ("anna", appE (.renounce "qd1"), false),
  ("carlo", appE (.openQuestion "qz" .collective), true),
  ("carlo", appE (.renounce "qz"), true),
  ("anna", propose (.departure "dora"), true),
  ("anna", approve "depart:dora", false),
  ("bruno", approve "depart:dora", true),
  ("carlo", approve "depart:dora", true),
  ("elena", approve "depart:dora", true)
]

/-- The guarded founding aggregate: the founding admin arrives through the
initial aggregate, never by a self-admitting event. -/
def foundedAggregate : GroupState State :=
  { members := [ ("anna", foundedMember) ]
    pendingProposals := []
    pendingBase := []
    appFold := State.empty }

#eval do
  match runSeeds? foundedAggregate 0 traceA, runSeeds? foundedAggregate 0 traceB,
      runChecked? foundedAggregate 0 traceC with
  | .ok a, .ok b, .ok c =>
      let env (steps : List Json) :=
        Json.mkObj [("schema", "reactivegas-integrated.trace"), ("version", (1 : Nat)),
          ("initial", aggJson foundedAggregate), ("steps", Json.arr steps.toArray)]
      IO.println (Json.mkObj [("A", env a), ("B", env b), ("C", env c)]).compress
  | .error dbg, _, _ =>
    throw (IO.userError s!"SEED-TRACE-EVENT-REFUSED (traceA): {dbg}")
  | _, .error dbg, _ =>
    throw (IO.userError s!"SEED-TRACE-EVENT-REFUSED (traceB): {dbg}")
  | _, _, .error dbg =>
    throw (IO.userError s!"SEED-TRACE-OUTCOME-MISMATCH (traceC): {dbg}")

end TraceDriverV1
