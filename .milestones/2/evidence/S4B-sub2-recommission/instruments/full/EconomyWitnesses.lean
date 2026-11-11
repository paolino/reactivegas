import Reactivegas.Mirrors

namespace AuditEconomy

def check (rows : Array (String × Bool)) : IO Unit := do
  let mut failed := false
  for (name, ok) in rows do
    IO.println s!"WITNESS {name} {ok}"
    if !ok then failed := true
  if failed then throw (IO.userError "WITNESS-FAILED")

def member : KelGroups.Member := ⟨"u", "u@audit", [.appRole "buyer"]⟩
def view : KelGroups.GroupView := ⟨[("u", member), ("u", ⟨"u", "second@audit", []⟩)]⟩
def col : Collection := ⟨7, "a", true, [⟨"u", 3⟩], [⟨"v", 7⟩]⟩
def s : State := { State.empty with conti := [("u", 10)], casse := [("a", 20)], collections := [col] }
def dup : State := { State.empty with conti := [("u", 1), ("u", -999)], collections := [col] }
def neg : State := { dup with conti := [("u", -1), ("u", 999)] }
def dupCol : Collection := { col with accepted := [⟨"u", 5⟩], pending := [⟨"u", 5⟩] }
def clashCol : Collection := { dupCol with pending := [⟨"u", 6⟩] }
def held : Collection := { col with pending := [⟨"u", 5⟩, ⟨"u", 9⟩] }
def before : State := { State.empty with conti := [("u", 5)], casse := [("a", 8)] }
def after : State := { before with conti := [("u", 12)], casse := [("a", 15)] }
def zero : State := { State.empty with conti := [("u", 0), ("u", 999), ("nonmember", 77)], casse := [("a", 0), ("a", 999)] }

def rows : Array (String × Bool) := #[
  ("sentinel", true),
  ("P02-nonzero-balanced", conservationB s),
  ("P02-distinguishes-one-unit", !conservationB { s with casse := [("a", 19)] }),
  ("P03-duplicate-first-balance", solventB view dup),
  ("P03-negative-first-balance", !solventB view neg),
  ("P03-accepted-negative", !solventB view { dup with collections := [{col with accepted := [⟨"u", -3⟩]}] }),
  ("P03-pending-negative", !solventB view { dup with collections := [{col with pending := [⟨"v", -7⟩]}] }),
  ("P03-absent-balance-default", solventB view {dup with conti := []}),
  ("P04-negative-present", insolventB view neg),
  ("P04-first-match-positive", !insolventB view dup),
  ("P04-absent-balance-default", !insolventB view {dup with conti := []}),
  ("P05-identical-duplicates-allowed", uniquePledgesB dupCol),
  ("P05-conflicting-duplicates-refused", !uniquePledgesB clashCol),
  ("P06-nonempty-all", allUniquePledgesB {s with collections := [col, dupCol]}),
  ("P06-nonempty-counterexample", !allUniquePledgesB {s with collections := [col, clashCol]}),
  ("P08-first-pending-amount", escrowHeldB held "u" 5),
  ("P08-later-duplicate-amount", !escrowHeldB held "u" 9),
  ("P08-absent-key", !escrowHeldB held "absent" 0),
  ("P09-nonempty-removed-other", governanceEnactsB "other" s),
  ("P09-live-referent", !governanceEnactsB "a" s),
  ("P10-nonzero-both-effects", doubleEntryB before after "a" "u" 7),
  ("P10-conto-alone-wrong", !doubleEntryB before {after with conti := [("u", 13)]} "a" "u" 7),
  ("P10-cassa-alone-wrong", !doubleEntryB before {after with casse := [("a", 16)]} "a" "u" 7),
  ("P10-negative-delta", doubleEntryB before {before with conti := [("u", 3)], casse := [("a", 6)]} "a" "u" (-2)),
  ("P12-first-match-duplicates-absent-defaults", canCloseGroupB view zero),
  ("P12-nonzero-member", !canCloseGroupB view {zero with conti := [("u", 1)]}),
  ("P12-open-collection", !canCloseGroupB view {zero with collections := [col]}),
  ("P12-nonzero-cassa", !canCloseGroupB view {zero with casse := [("a", 1)]})]
#eval check rows
end AuditEconomy
