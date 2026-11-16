-- | QuickCheck generators for group state property tests.
module Test.Generators
  ( arbitraryKey
  , freshKey
  , adminKey
  , arbitraryRole
  , arbitraryAdminRoles
  , arbitraryNonAdminRoles
  , arbitraryGroupState
  , arbitraryWithAdmin
  , arbitraryWithTwoAdmins
  , gsWithAdminCount
  , arbitraryProposal
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import KelGroups.Client.Event (Proposal(..))
import KelGroups.Client.State (GroupState)
import KelGroups.Client.Types (Admin(..), Role(..))
import Data.Foldable (any)
import Test.QuickCheck.Gen (Gen, chooseInt, elements)

isAdminR :: Role -> Boolean
isAdminR (AdminRole _) = true
isAdminR _ = false

-- | Generate a random key ("key0" through "key99").
arbitraryKey :: Gen String
arbitraryKey = do
  i <- chooseInt 0 99
  pure $ "key" <> show i

-- | Generate a key guaranteed not in the state's members.
-- Members use keys 0-19 and "admin*"; fresh keys use 50-99.
freshKey :: forall a. GroupState a -> Gen String
freshKey _ = do
  i <- chooseInt 50 99
  pure $ "key" <> show i

-- | Pick an existing admin key from state.
adminKey :: forall a. GroupState a -> Gen String
adminKey gs =
  let
    adminMap = Map.filter
      (\m -> (Set.toUnfoldable m.roles :: Array Role) # any isAdminR)
      gs.members
    adminKeys = Array.fromFoldable $ Map.keys adminMap
  in
    case NEA.fromArray adminKeys of
      Nothing -> pure "admin0"
      Just ne -> elements ne

-- | Generate a random role.
arbitraryRole :: Gen Role
arbitraryRole = do
  n <- chooseInt 0 4
  pure $ case n of
    0 -> AdminRole PublicAdmin
    1 -> AdminRole PrivateAdmin
    2 -> AppRole "reader"
    3 -> AppRole "writer"
    _ -> AppRole "moderator"

-- | Generate a role set that always contains Admin.
arbitraryAdminRoles :: Gen (Set.Set Role)
arbitraryAdminRoles = do
  extra <- arbitraryRole
  pure $ Set.insert (AdminRole PublicAdmin) (Set.singleton extra)

-- | Generate a role set that never contains Admin.
arbitraryNonAdminRoles :: Gen (Set.Set Role)
arbitraryNonAdminRoles = do
  n <- chooseInt 0 2
  let
    name = case n of
      0 -> "reader"
      1 -> "writer"
      _ -> "moderator"
  pure $ Set.singleton (AppRole name)

-- | Construct a state with exactly n admins (deterministic).
gsWithAdminCount :: Int -> GroupState Unit
gsWithAdminCount n =
  { members: Map.fromFoldable
      $ map mkAdmin
          (if n <= 0 then [] else Array.range 0 (n - 1))
  , pendingProposals: Map.empty
  , appFold: unit
  }
  where
  mkAdmin i =
    let
      k = "admin" <> show i
    in
      Tuple k
        { key: k
        , email: k <> "@test.example"
        , roles: Set.singleton (AdminRole PublicAdmin)
        }

-- | Generate a random GroupState with 0-10 members.
arbitraryGroupState :: Gen (GroupState Unit)
arbitraryGroupState = do
  nMembers <- chooseInt 0 10
  let keys = Array.take nMembers allKeys
  members <- mkMembers keys
  pure
    { members
    , pendingProposals: Map.empty
    , appFold: unit
    }
  where
  allKeys =
    map (\i -> "key" <> show i) (Array.range 0 19)
  mkMembers keys = do
    pairs <- traverse mkMember keys
    pure $ Map.fromFoldable pairs
  mkMember k = do
    admin <- chooseInt 0 1
    roles <-
      if admin == 0 then arbitraryAdminRoles
      else arbitraryNonAdminRoles
    pure $ Tuple k { key: k, email: k <> "@test.example", roles }

-- | Generate a GroupState with at least one admin.
arbitraryWithAdmin :: Gen (GroupState Unit)
arbitraryWithAdmin = do
  gs <- arbitraryGroupState
  let
    adminMember =
      { key: "admin0"
      , email: "admin0@test.example"
      , roles: Set.singleton (AdminRole PublicAdmin)
      }
  pure $ gs
    { members =
        Map.insert "admin0" adminMember gs.members
    }

-- | Generate a GroupState with at least two admins.
arbitraryWithTwoAdmins :: Gen (GroupState Unit)
arbitraryWithTwoAdmins = do
  gs <- arbitraryGroupState
  let
    a1 =
      { key: "admin0"
      , email: "admin0@test.example"
      , roles: Set.singleton (AdminRole PublicAdmin)
      }
    a2 =
      { key: "admin1"
      , email: "admin1@test.example"
      , roles: Set.singleton (AdminRole PublicAdmin)
      }
  pure $ gs
    { members =
        Map.insert "admin0" a1
          (Map.insert "admin1" a2 gs.members)
    }

-- | Generate a random proposal.
arbitraryProposal :: Gen Proposal
arbitraryProposal = do
  k <- arbitraryKey
  n <- chooseInt 0 2
  case n of
    0 -> do
      roles <- arbitraryAdminRoles
      pure $ IntroduceMember k (k <> "@test.example") roles
    1 -> pure $ RemoveMember k
    _ -> do
      roles <- arbitraryAdminRoles
      pure $ ChangeRoles k roles
