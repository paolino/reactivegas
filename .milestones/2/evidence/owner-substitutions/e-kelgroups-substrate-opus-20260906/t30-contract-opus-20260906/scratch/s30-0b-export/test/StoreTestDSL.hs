{- |
Module      : StoreTestDSL
Description : DSL combinators for store-through invariant testing
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Combinators that mirror Lean quantifier patterns for
testing invariants through the full store roundtrip
(KERI event construction → SQLite write → read → fold).
-}
module StoreTestDSL
    ( -- * Store helpers
      withStore
    , replayHistory
    , appendTestEvent

      -- * DSL combinators
    , onReachable
    , onReachableWhere
    , onReachableWith

      -- * History generators
    , arbitraryHistory
    ) where

import Control.Monad (forM_)
import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text, pack)
import KelGroups.Event
    ( BaseEvent (..)
    , GroupEvent (..)
    , Proposal (..)
    )
import KelGroups.Fold (applyEvent)
import KelGroups.Server (mkKeriEvent)
import KelGroups.State
    ( GroupState (..)
    , adminCount
    , emptyState
    , isAdmin
    )
import KelGroups.Store
    ( ChainTip (..)
    , KELStore
    , appendEvent
    , chainTip
    , closeKEL
    , openKEL
    , readState
    )
import KelGroups.Trivial
    ( trivialFold
    , trivialInitial
    )
import KelGroups.Types (Admin (..), Role (..))
import Keri.Event
    ( eventDigest
    , eventPrefix
    , eventSequenceNumber
    )
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import Test.QuickCheck
    ( Gen
    , Property
    , chooseInt
    , elements
    , forAll
    , sized
    )
import Test.QuickCheck.Monadic
    ( assert
    , monadicIO
    , pick
    , run
    )

-- --------------------------------------------------------
-- Store helpers
-- --------------------------------------------------------

-- | Run an action with a fresh temporary SQLite KEL.
withStore :: (KELStore () -> IO a) -> IO a
withStore action = do
    path <- emptySystemTempFile "kelgroups-test-.db"
    store <- openKEL trivialFold trivialInitial path
    result <- action store
    closeKEL store
    removeFile path
    pure result

{- | Append a (signer, groupEvent) pair through the full
KERI event pipeline. Tracks the chain tip via IORef.
-}
appendTestEvent
    :: KELStore ()
    -> IORef (Maybe ChainTip)
    -> (Text, GroupEvent ())
    -> IO ()
appendTestEvent store tipRef (signer, groupEvt) = do
    tip <- readIORef tipRef
    let keriEvt = mkKeriEvent tip signer groupEvt
    appendEvent
        store
        trivialFold
        signer
        keriEvt
        "test-sig"
        groupEvt
    writeIORef tipRef $
        Just
            ChainTip
                { tipPrefix = eventPrefix keriEvt
                , tipSeqNo =
                    eventSequenceNumber keriEvt
                , tipDigest = eventDigest keriEvt
                }

{- | Replay a history of signed events through the
store and return the resulting group state.
-}
replayHistory
    :: [(Text, GroupEvent ())] -> IO (GroupState ())
replayHistory events = withStore $ \store -> do
    tip <- chainTip store
    tipRef <- newIORef tip
    forM_ events $ appendTestEvent store tipRef
    readState store

-- --------------------------------------------------------
-- DSL combinators
-- --------------------------------------------------------

{- | Test a predicate on all reachable states.
Mirrors: @theorem foo (gs : GroupState) : P gs@
-}
onReachable :: (GroupState () -> Bool) -> Property
onReachable p = forAll arbitraryHistory $ \history ->
    monadicIO $ do
        gs <- run $ replayHistory history
        assert $ p gs

{- | Test a predicate on reachable states satisfying
a precondition. Mirrors:
@theorem foo (gs : GroupState) (h : Pre gs) : P gs@
-}
onReachableWhere
    :: (GroupState () -> Bool)
    -> (GroupState () -> Bool)
    -> Property
onReachableWhere pre p =
    forAll arbitraryHistory $ \history -> monadicIO $ do
        gs <- run $ replayHistory history
        run $ pure $ not (pre gs) || p gs

{- | Test a property that generates extra data from
the reached state. Mirrors:
@theorem foo (gs) (mid) (roles) : P (f gs mid roles)@
-}
onReachableWith
    :: Gen [(Text, GroupEvent ())]
    -> (GroupState () -> Gen Bool)
    -> Property
onReachableWith histGen check =
    forAll histGen $ \history -> monadicIO $ do
        gs <- run $ replayHistory history
        result <- pick $ check gs
        assert result

-- --------------------------------------------------------
-- History generators
-- --------------------------------------------------------

{- | Generate a valid event history: bootstrap first,
then random proposals from live admins.
-}
arbitraryHistory :: Gen [(Text, GroupEvent ())]
arbitraryHistory = sized $ \n -> do
    let steps = max 0 (min n 20)
    bootstrapKey <-
        pack . ("key" <>) . show <$> chooseInt (0, 99)
    let bootstrapEvt =
            ( "bootstrap"
            , Base $
                Propose $
                    IntroduceMember
                        bootstrapKey
                        (bootstrapKey <> "@test.example")
                        ( Set.singleton
                            (AdminRole PublicAdmin)
                        )
            )
        gs0 =
            applyEvent
                trivialFold
                (emptyState trivialInitial)
                bootstrapEvt
    go steps [bootstrapEvt] gs0
  where
    go
        :: Int
        -> [(Text, GroupEvent ())]
        -> GroupState ()
        -> Gen [(Text, GroupEvent ())]
    go 0 acc _ = pure (reverse acc)
    go n acc gs
        | adminCount gs == 0 = pure (reverse acc)
        | otherwise = do
            let adminKeys =
                    [ k
                    | (k, _) <- Map.toList (members gs)
                    , isAdmin k gs
                    ]
            signer <- elements adminKeys
            proposal <- arbitraryProposal gs
            let evt = (signer, Base $ Propose proposal)
                gs' =
                    applyEvent trivialFold gs evt
            go (n - 1) (evt : acc) gs'

    arbitraryProposal
        :: GroupState () -> Gen Proposal
    arbitraryProposal gs = do
        key <-
            pack . ("key" <>) . show
                <$> chooseInt (0, 99)
        let memberKeys = Map.keys (members gs)
            isExisting = key `elem` memberKeys
            nAdmins = adminCount gs
        let email = key <> "@test.example"
            adminRole' = AdminRole PublicAdmin
        if isExisting
            then
                if nAdmins >= 2
                    then
                        elements
                            [ RemoveMember key
                            , ChangeRoles
                                key
                                (Set.singleton adminRole')
                            , ChangeRoles key Set.empty
                            ]
                    else
                        elements
                            [ ChangeRoles
                                key
                                (Set.singleton adminRole')
                            , ChangeRoles key Set.empty
                            ]
            else do
                roles <-
                    elements
                        [ Set.singleton adminRole'
                        , Set.empty
                        , Set.fromList
                            [ adminRole'
                            , AppRole "editor"
                            ]
                        ]
                pure $ IntroduceMember key email roles
