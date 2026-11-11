-- | Core types mirroring Haskell KelGroups.Types.
module KelGroups.Client.Types
  ( Admin(..)
  , Role(..)
  , Member
  , isAdminRole
  , hasAdmin
  ) where

import Prelude

import Data.Foldable (any)
import Data.Set (Set)
import Data.Set as Set

-- | Admin visibility level.
data Admin
  = PublicAdmin
  | PrivateAdmin

derive instance eqAdmin :: Eq Admin
derive instance ordAdmin :: Ord Admin

instance showAdmin :: Show Admin where
  show PublicAdmin = "PublicAdmin"
  show PrivateAdmin = "PrivateAdmin"

-- | A role in the group.
data Role
  = AdminRole Admin
  | AppRole String

derive instance eqRole :: Eq Role
derive instance ordRole :: Ord Role

instance showRole :: Show Role where
  show (AdminRole adm) = "AdminRole " <> show adm
  show (AppRole name) = "AppRole " <> name

-- | Check if a role is any admin role.
isAdminRole :: Role -> Boolean
isAdminRole (AdminRole _) = true
isAdminRole _ = false

-- | Check if a set of roles contains any admin role.
hasAdmin :: Set Role -> Boolean
hasAdmin roles =
  any isAdminRole (Set.toUnfoldable roles :: Array Role)

-- | A group member identified by CESR public key.
type Member =
  { key :: String
  , email :: String
  , roles :: Set Role
  }
