{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Reactivegas.Economic.Core
Description : Pure money custody economic core (selected Lean arms)
Copyright   : (c) 2026 Paolo Veronelli
License     : BSD-3-Clause

The first economic core of issue #90: the four custody arms selected from
the pinned Lean machine (deposit, withdraw, transferCassa, donate) as one
pure transition. Semantics mirror lean/Reactivegas/Step.lean and
lean/Reactivegas/State.lean at the pinned authority; nothing here invents
policy. The caller's frame is preserved parametrically: the transition
never reads or rebuilds it. Money is arbitrary-precision Integer, and
balances are ordered association lists with Lean first-match lookup and
update and append semantics, duplicates included.
-}
module Reactivegas.Economic.Core (
    Key,
    CustodyEvent (..),
    Queries (..),
    State (..),
    step,
) where

import Data.Text (Text)

{- | Lossless account identity, identical to the accepted Haskell substrate
key representation. No normalization, narrowing or nickname.
-}
type Key = Text

{- | The closed selected custody surface. The signer arrives separately, so
an unsupported operation is unrepresentable and no unsupported event can
be accepted as a successful no-op.
-}
data CustodyEvent
    = -- | Credit the member's @conti@ account from the signer's cash box.
      Deposit Key Integer
    | -- | Debit the member's @conti@ account into the signer's cash box.
      Withdraw Key Integer
    | -- | Move between two admin cash boxes: @from@ loses, the signer gains.
      TransferCassa Key Integer
    | -- | Credit the signer's cash box and the reserved comune account.
      Donate Integer
    deriving (Eq, Ord, Show)

{- | Read-only membership and admin queries. Production values derive from
one canonical GroupView via the adapter package; the core never stores
or reconstructs membership.
-}
data Queries = Queries
    { memberQuery :: Key -> Bool
    -- ^ Is the key a current member?
    , adminQuery :: Key -> Bool
    -- ^ Is the key a current member holding an admin role?
    }

{- | The custody state frame. @conti@ are user credit accounts and @casse@
are cash boxes; @untouched@ carries every other application field
(collections, votes, opaque future payload) and is preserved
structurally, for arbitrary values, without reconstruction.
-}
data State frame = State
    { conti :: [(Key, Integer)]
    -- ^ user credit accounts, keyed by substrate key
    , casse :: [(Key, Integer)]
    -- ^ cash boxes, keyed by substrate key
    , untouched :: frame
    -- ^ all application state this transition does not change
    }
    deriving (Eq, Show)

{- | One economic refusal: an event outside the pinned guards returns
'Nothing'. There are no invented per-guard reasons.
-}
step :: Queries -> State frame -> Key -> CustodyEvent -> Maybe (State frame)
step queries state signer event = case event of
    Deposit user amount
        | adminQuery queries signer
            && memberQuery queries user
            && signer /= user
            && amount >= 0 ->
            Just
                state
                    { conti = bump (conti state) user amount
                    , casse = bump (casse state) signer amount
                    }
        | otherwise -> Nothing
    Withdraw user amount
        | adminQuery queries signer
            && memberQuery queries user
            && signer /= user
            && balance (conti state) user >= amount
            && not (stalled state) ->
            Just
                state
                    { conti = bump (conti state) user (-amount)
                    , casse = bump (casse state) signer (-amount)
                    }
        | otherwise -> Nothing
    TransferCassa from amount
        | adminQuery queries signer
            && adminQuery queries from
            && signer /= from
            && amount > 0 ->
            Just
                state
                    { casse =
                        bump
                            (bump (casse state) from (-amount))
                            signer
                            amount
                    }
        | otherwise -> Nothing
    Donate amount
        | adminQuery queries signer && amount > 0 ->
            Just
                state
                    { conti = bump (conti state) "comune" amount
                    , casse = bump (casse state) signer amount
                    }
        | otherwise -> Nothing

-- | First-match balance lookup; an absent account has zero balance.
balance :: [(Key, Integer)] -> Key -> Integer
balance [] _ = 0
balance ((key, amount) : rest) wanted
    | key == wanted = amount
    | otherwise = balance rest wanted

{- | Add an amount to the first matching account, appending a new entry
when absent. Duplicate and zero entries are retained.
-}
bump :: [(Key, Integer)] -> Key -> Integer -> [(Key, Integer)]
bump [] key amount = [(key, amount)]
bump ((key, current) : rest) wanted amount
    | key == wanted = (key, current + amount) : rest
    | otherwise = (key, current) : bump rest wanted amount

-- | The machine is stalled exactly when the reserved comune account is negative.
stalled :: State frame -> Bool
stalled state = balance (conti state) "comune" < 0
