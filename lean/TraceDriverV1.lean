import Lean
import Reactivegas.Step

/-!
# Trace interchange v1 producer

Emits the frozen `reactivegas.trace` version-1 seed envelopes embedded by
`economics-simulator.html` as `LEAN_TRACES_V1`: `State` and `Event` are
serialized by Lean `ToJson` instances — never `Repr` parsing, never
JavaScript — and every step carries its explicit input state plus the
`applied` post-state computed by the authoritative `step` of
`Reactivegas.Step`. The driver is the durable artifact; its JSON output is
disposable and reproducible from a clean checkout with:

```sh
cd lean && lake env lean TraceDriverV1.lean
```

(`economics-simulator-trace-gate.mjs` at the repository root runs exactly
that, compares the fresh output against the embedded fixture, and replays it
through the page's production JavaScript.)

If any seeded event is refused by `step`, the driver throws instead of
emitting a usable-looking corpus. The seed contains applied steps only: the
typed diagnostic evaluator that would emit refusal guard ids rides in #48
and is not fabricated here.
-/

open Lean (ToJson toJson Json)

deriving instance Lean.ToJson for Pledge
deriving instance Lean.ToJson for Collection
deriving instance Lean.ToJson for State
deriving instance Lean.ToJson for Event

/-- Fold events through `step`; `none` as soon as any seeded event is
refused, so a broken seed can never emit a partial corpus. -/
def stepsJson? : State → List Event → Option (List Json)
  | _, [] => some []
  | s, e :: es =>
    match step s e with
    | some s' =>
      (stepsJson? s' es).map fun rest =>
        Json.mkObj [("input", toJson s), ("event", toJson e),
          ("result", Json.mkObj [("tag", "applied"), ("state", toJson s')])]
          :: rest
    | none => none

def envelope? (es : List Event) : Option Json :=
  (stepsJson? (State.init 0) es).map fun steps =>
    Json.mkObj [
      ("schema", "reactivegas.trace"),
      ("version", (1 : Nat)),
      ("initial", toJson (State.init 0)),
      ("steps", Json.arr steps.toArray)]

/-- Trace A: removeResponsabile with live collections in flight —
strip + refund of accepted and pending across two open collections. -/
def traceA : List Event := [
  .addUser 0 1,
  .addUser 0 2,
  .electResponsabile 0 1,
  .deposit 0 1 100,
  .deposit 1 2 80,
  .openPurchase 1 10,
  .pledge 0 2 10 30,
  .acceptPledge 1 2 10,
  .pledge 1 1 10 40,
  .openPurchase 1 11,
  .pledge 0 2 11 20,
  .removeResponsabile 0 1
]

/-- Trace B: correctPledge both directions, closePurchase driving a cassa
negative, denyPermission refunding both accepted and pending pledges. -/
def traceB : List Event := [
  .addUser 0 1,
  .electResponsabile 0 1,
  .addUser 0 2,
  .deposit 0 2 50,
  .openPurchase 1 7,
  .pledge 0 2 7 20,
  .acceptPledge 1 2 7,
  .correctPledge 1 2 7 35,
  .correctPledge 1 2 7 5,
  .grantPermission 0 7,
  .closePurchase 1 7,
  .openPurchase 1 8,
  .pledge 1 2 8 10,
  .acceptPledge 1 2 8,
  .deposit 1 0 25,
  .pledge 0 0 8 15,
  .denyPermission 0 8
]

#eval do
  match envelope? traceA, envelope? traceB with
  | some a, some b =>
    IO.println (Json.mkObj [("A", a), ("B", b)]).compress
  | _, _ =>
    throw (IO.userError
      "SEED-TRACE-EVENT-REFUSED: un evento seminato è stato rifiutato da step; nessun corpus emesso")
