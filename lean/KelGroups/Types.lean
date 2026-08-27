/-!
# KelGroups core types

The Haskell `Map` and `Set` surfaces are represented by association lists and
duplicate-free membership lists.  The application fold remains abstract.
-/

namespace KelGroups

abbrev Key := String
abbrev Email := String
abbrev RoleName := String
abbrev ProposalId := String

inductive Admin where
  | publicAdmin
  | privateAdmin
deriving DecidableEq, BEq, Repr

inductive Role where
  | adminRole (admin : Admin)
  | appRole (name : RoleName)
deriving DecidableEq, BEq, Repr

structure Member where
  key : Key
  email : Email
  roles : List Role
deriving DecidableEq, BEq, Repr

def isAdminRole : Role → Bool
  | .adminRole _ => true
  | .appRole _ => false

def hasAdmin (roles : List Role) : Bool := roles.any isAdminRole

structure RoleDef (α : Type) where
  canAdd : α → Bool
  canRemove : α → Bool

structure GroupConfig (α : Type) where
  roleDefs : List (RoleName × RoleDef α)

variable {α κ ν : Type}

def setInsert [BEq α] (value : α) (values : List α) : List α :=
  if values.contains value then values else value :: values

def assocLookup [BEq κ] (key : κ) : List (κ × ν) → Option ν
  | [] => none
  | (candidate, value) :: rest =>
      if candidate == key then some value else assocLookup key rest

def assocErase [BEq κ] (key : κ) : List (κ × ν) → List (κ × ν)
  | [] => []
  | (candidate, value) :: rest =>
      if candidate == key then rest else (candidate, value) :: assocErase key rest

def assocInsert [BEq κ] (key : κ) (value : ν)
    (entries : List (κ × ν)) : List (κ × ν) :=
  (key, value) :: assocErase key entries

def assocAdjust [BEq κ] (key : κ) (f : ν → ν) : List (κ × ν) → List (κ × ν)
  | [] => []
  | (candidate, value) :: rest =>
      if candidate == key then (candidate, f value) :: rest
      else (candidate, value) :: assocAdjust key f rest

theorem assocLookup_insert_self [BEq κ] [LawfulBEq κ]
    (key : κ) (value : ν) (entries : List (κ × ν)) :
    assocLookup key (assocInsert key value entries) = some value := by
  simp [assocInsert, assocLookup]

end KelGroups
