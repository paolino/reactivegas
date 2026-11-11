{- |
Module      : KelGroups.Types
Description : Core types for KEL-based group management
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Base types for members, roles, and group configuration.
The library is polymorphic over an application event type
@a@ which carries domain-specific semantics.
-}
module KelGroups.Types
    ( Admin (..)
    , Role (..)
    , RoleName
    , Member (..)
    , GroupConfig (..)
    , RoleDef (..)
    , ProposalId
    , isAdminRole
    , hasAdmin
    , GroupView (..)
    , lookupMemberInView
    , isMemberInView
    , isAdminInView
    ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)

-- | A role name is an opaque text label.
type RoleName = Text

-- | A proposal identifier (digest of the proposal).
type ProposalId = Text

{- | Admin visibility. 'PublicAdmin' exposes their
email to non-members; 'PrivateAdmin' does not.
-}
data Admin
    = -- | Email visible to non-members
      PublicAdmin
    | -- | Email hidden from non-members
      PrivateAdmin
    deriving stock (Eq, Ord, Show)

{- | A role in the group. Admin roles (public or
private) carry base-system meaning: admins vote on
member and role changes. Application roles are
opaque labels interpreted by the application layer.
-}
data Role
    = -- | Admin role with visibility flag
      AdminRole Admin
    | -- | Application-defined role
      AppRole RoleName
    deriving stock (Eq, Ord, Show)

-- | Check if a role is any admin role.
isAdminRole :: Role -> Bool
isAdminRole (AdminRole _) = True
isAdminRole _ = False

-- | Check if a role set contains any admin role.
hasAdmin :: Set Role -> Bool
hasAdmin = any isAdminRole

{- | An immutable projection of the one writable member/role relation.
Carries no app payload and confers no capability to return or replace a
group aggregate: consumers read membership and roles and produce nothing
but app payload.
-}
newtype GroupView = GroupView
    { gvMembers :: Map Text Member
    -- ^ Sole canonical member relation
    }
    deriving stock (Eq, Show)

-- | The member registered under a key in the canonical view, if any.
lookupMemberInView :: Text -> GroupView -> Maybe Member
lookupMemberInView key view = Map.lookup key (gvMembers view)

-- | Is a key a current member of the canonical relation?
isMemberInView :: Text -> GroupView -> Bool
isMemberInView key view = Map.member key (gvMembers view)

{- | Is a key a current member holding an admin role? The one notion of
responsibility every consumer reads; there is no second list.
-}
isAdminInView :: Text -> GroupView -> Bool
isAdminInView key view =
    maybe False (hasAdmin . memberRoles) (lookupMemberInView key view)

-- | A group member with their public key and roles.
data Member = Member
    { memberKey :: Text
    -- ^ CESR-encoded public key
    , memberEmail :: Text
    -- ^ Contact email address
    , memberRoles :: Set Role
    -- ^ Current set of roles
    }
    deriving stock (Eq, Show)

{- | Definition of an application role with
preconditions for adding and removing the role.
The predicates receive a fold function that can
extract information from the KEL.
-}
data RoleDef a = RoleDef
    { canAdd :: a -> Bool
    -- ^ Can this role be added given current app fold?
    , canRemove :: a -> Bool
    -- ^ Can this role be removed given current app fold?
    }

{- | Group configuration, parameterized by the
application event type @a@. Supplied at server startup.
-}
newtype GroupConfig a = GroupConfig
    { roleDefs :: Map RoleName (RoleDef a)
    -- ^ Application role definitions with predicates
    }
