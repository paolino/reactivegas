{-# LANGUAGE GHC2021, OverloadedStrings, LambdaCase #-}
module Main (main) where

import Control.Monad (forM_, unless)
import Data.Aeson (decodeStrict)
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import KelGroups.Event qualified as E
import KelGroups.Fold qualified as F
import KelGroups.State (GroupState(..), PendingBase(..), emptyState, groupView)
import KelGroups.Store qualified as S
import KelGroups.Types (Member(..), Admin(..), Role(..), GroupView(..))
import KelGroups.Validate qualified as V
import S28DemoApp
import System.Environment (getArgs)
import System.IO.Temp (withSystemTempDirectory)

assert :: String -> Bool -> IO ()
assert label ok = do
  putStrLn (label ++ "=" ++ show ok)
  unless ok (fail ("ASSERTION: " ++ label))

eq :: (Eq a, Show a) => String -> a -> a -> IO ()
eq label expected actual = do
  unless (expected == actual) $ putStrLn ("EXPECTED " ++ show expected ++ "\nACTUAL " ++ show actual)
  assert label (expected == actual)

pub, priv, blue, red :: Set.Set Role
pub = Set.singleton (AdminRole PublicAdmin)
priv = Set.singleton (AdminRole PrivateAdmin)
blue = Set.singleton (AppRole "blue")
red = Set.fromList [AppRole "red", AppRole "reader"]

seed :: GroupState DemoState
seed = (emptyState (DemoState 7 ["seed"])) { members = Map.fromList
  [ ("a", Member "a" "alpha@example" pub)
  , ("b", Member "b" "beta@example" priv)
  , ("c", Member "c" "gamma@example" pub) ] }

withM :: GroupState DemoState
withM = seed { members = Map.insert "m" (Member "m" "member@example" blue) (members seed) }

withRoles :: GroupState DemoState
withRoles = seed { members = Map.insert "m" (Member "m" "member@example" red) (members seed) }

admit :: E.IntegratedEvent DemoProposal DemoEvent
admit = E.IEDirect (E.AdmitMember "m" "member@example" blue)
proposal :: DemoProposal
proposal = DemoChangeRoles "m" red
pid :: Text
pid = demoDigest proposal

hookLine :: E.BaseChange -> GroupView -> GroupView -> DemoState -> Text
hookLine change pre post st = T.pack (show (change,pre,post,st))

recording :: F.Integration DemoState DemoEvent DemoProposal DemoError
recording = demoIntegration { F.intBaseHook = \change pre post st ->
  Right st { demoCounter = demoCounter st + 13,
             demoLog = demoLog st ++ [hookLine change pre post st] } }

expectedHook :: E.BaseChange -> GroupView -> GroupView -> DemoState -> DemoState
expectedHook change pre post st = DemoState (demoCounter st + 13)
  (demoLog st ++ [T.pack (show (change, pre, post, st))])

accepted :: S.KELStore DemoState -> F.Integration DemoState DemoEvent DemoProposal DemoError -> Text -> E.IntegratedEvent DemoProposal DemoEvent -> IO (F.IntegratedResult DemoState)
accepted store integration signer event = S.appendIntegratedEvent store integration signer event >>= \case
  Left err -> fail ("SETUP: expected accepted event: " ++ show err)
  Right result -> pure result

viewCase :: IO ()
viewCase = do
  -- Negative control exercises the same equality predicate with a corrupted view.
  assert "R1-NC wrong-empty-view-detected" (GroupView Map.empty /= GroupView (members seed))
  store <- S.openIntegratedKEL recording seed ":memory:"
  initial <- S.readState store
  eq "R1 founding exact" seed initial
  eq "R1 founding view" (GroupView (members seed)) (groupView initial)
  eq "R1 direct-validator" (Right ()) (V.validateDirectAdmission demoReserved seed "a" "m" "member@example" blue)
  added <- accepted store recording "a" admit
  let st1 = expectedHook (E.MemberAdmitted "m") (GroupView (members seed)) (GroupView (members withM)) (appFold seed)
      gs1 = withM { appFold = st1 }
      pending = PendingBase (E.ChangeRolesVoted "m" red) "a" (Set.singleton "a")
      gs2 = gs1 { pendingBase = Map.singleton pid pending }
  eq "R1 admission exact pre-post-payload" (F.IntegratedResult gs1 (Just (E.MemberAdmitted "m"))) added
  eq "R1 mutation-validator" (Right ()) (V.validateBaseMutation gs1 "a" (E.ChangeRolesVoted "m" red))
  proposed <- accepted store recording "a" (E.IEPropose proposal)
  eq "R1 proposal exact pending" (F.IntegratedResult gs2 Nothing) proposed
  eq "R1 approval-validator" (Right ()) (V.validateBaseApproval gs2 "b" pid)
  changed <- accepted store recording "b" (E.IEApprove pid)
  let st3 = expectedHook (E.RolesChanged "m") (GroupView (members withM)) (GroupView (members withRoles)) st1
      gs3 = withRoles { appFold = st3 }
  eq "R1 roles exact pre-post-payload" (F.IntegratedResult gs3 (Just (E.RolesChanged "m"))) changed
  before <- S.readEventsFrom store 1
  refused <- S.appendIntegratedEvent store recording "outsider" (E.IEApp (DemoAdd 9))
  eq "R1 nonmember identity" (Left (F.IEValidation (V.NotAMember "outsider"))) refused
  S.readState store >>= eq "R1 nonmember exact state" gs3
  after <- S.readEventsFrom store 1
  eq "R1 nonmember bytes" (map S.seEventBytes before) (map S.seEventBytes after)
  S.closeKEL store

hookCase :: IO ()
hookCase = withSystemTempDirectory "s28-r3" $ \dir -> do
  let file = dir ++ "/hook.db"
      good = expectedHook (E.MemberAdmitted "m") (GroupView (members seed)) (GroupView (members withM)) (appFold seed)
      expected = withM { appFold = good }
      refusal = recording { F.intBaseHook = \change pre post st ->
        Left (DemoHookRefused (hookLine change pre post st)) }
      err = DemoHookRefused (T.pack (show (E.MemberAdmitted "m", GroupView (members seed), GroupView (members withM), appFold seed)))
  assert "R3-NC ignored-hook-output-detected" (good /= appFold seed)
  store <- S.openIntegratedKEL recording seed file
  result <- accepted store recording "a" admit
  eq "R3 success exact output and views" (F.IntegratedResult expected (Just (E.MemberAdmitted "m"))) result
  S.closeKEL store
  reopened <- S.openIntegratedKEL recording seed file
  S.readState reopened >>= eq "R3 success reopened" expected
  S.closeKEL reopened
  let rejectedFile = dir ++ "/refused.db"
  rejectedStore <- S.openIntegratedKEL refusal seed rejectedFile
  result2 <- S.appendIntegratedEvent rejectedStore refusal "a" admit
  eq "R3 refusal exact hook arguments" (Left (F.IEApp err)) result2
  S.readState rejectedStore >>= eq "R3 refusal state restored" seed
  S.kelLength rejectedStore >>= eq "R3 refusal count" 0
  rows <- S.readEventsFrom rejectedStore 1
  eq "R3 refusal rows" 0 (length rows)
  S.closeKEL rejectedStore
  reopened2 <- S.openIntegratedKEL refusal seed rejectedFile
  S.readState reopened2 >>= eq "R3 refusal reopened" seed
  S.closeKEL reopened2

lifecycleCase :: IO ()
lifecycleCase = withSystemTempDirectory "s28-r5" $ \dir -> do
  let file = dir ++ "/lifecycle.db"
      gs1 = withM { appFold = DemoState 7 ["seed","hook admitted m"] }
      gs2 = gs1 { pendingBase = Map.singleton pid (PendingBase (E.ChangeRolesVoted "m" red) "a" (Set.singleton "a")) }
      gs3 = withRoles { appFold = DemoState 7 ["seed","hook admitted m","hook roles m"] }
      gs4 = gs3 { appFold = DemoState 12 ["seed","hook admitted m","hook roles m","add 5"] }
      trace = [("a",admit),("a",E.IEPropose proposal),("a",E.IEApprove pid),("b",E.IEApprove pid),("m",E.IEApp (DemoAdd 5))]
      expectedStates = [seed,gs1,gs2,gs2,gs3,gs4]
      expectedDecisions = [Right (),Right (),Left (V.AlreadyApproved "a" pid),Right (),Right ()]
      changes = [Just (E.MemberAdmitted "m"),Nothing,Nothing,Just (E.RolesChanged "m"),Nothing]
      validate gs signer event = case event of
        E.IEDirect (E.AdmitMember key email roles) -> V.validateDirectAdmission demoReserved gs signer key email roles
        E.IEPropose p -> V.validateBaseMutation gs signer (demoProposalMutation p)
        E.IEApprove ident -> V.validateBaseApproval gs signer ident
        E.IEApp _ -> if Map.member signer (members gs) then Right () else Left (V.NotAMember signer)
  assert "R5-NC skipped-enactment-detected" (gs2 /= gs3)
  store <- S.openIntegratedKEL demoIntegration seed file
  forM_ (zip [0..] trace) $ \(i,(signer,event)) -> do
    before <- S.readState store
    eq ("R5 prestate " ++ show i) (expectedStates !! i) before
    eq ("R5 integrated validation " ++ show i) (expectedDecisions !! i) (validate before signer event)
    result <- S.appendIntegratedEvent store demoIntegration signer event
    case expectedDecisions !! i of
      Left err -> eq ("R5 refusal " ++ show i) (Left (F.IEValidation err)) result
      Right () -> eq ("R5 success " ++ show i) (Right (F.IntegratedResult (expectedStates !! (i+1)) (changes !! i))) result
    S.readState store >>= eq ("R5 poststate " ++ show i) (expectedStates !! (i+1))
    eq ("R5 independent-prefix replay " ++ show i) (expectedStates !! (i+1)) (F.foldIntegratedFrom demoIntegration seed (take (i+1) trace))
  rows <- S.readEventsFrom store 1
  let decoded = traverse (\r -> (S.seSigner r,) <$> decodeStrict (S.seEventBytes r)) rows
      acceptedTrace = [trace !! 0,trace !! 1,trace !! 3,trace !! 4]
  eq "R5 exact persisted accepted events" (Just acceptedTrace) decoded
  eq "R5 full persisted replay" gs4 (F.foldIntegratedFrom demoIntegration seed acceptedTrace)
  S.kelLength store >>= eq "R5 exact length" 4
  S.closeKEL store
  reopened <- S.openIntegratedKEL demoIntegration seed file
  S.readState reopened >>= eq "R5 real reopen" gs4
  S.closeKEL reopened

majorityCase :: IO ()
majorityCase = do
  -- Current denominator grows from three to five admins while this vote is pending.
  let s0 = withM { appFold = appFold seed }
      p = DemoRemove "m"
      ident = demoDigest p
  store <- S.openIntegratedKEL demoIntegration s0 ":memory:"
  pending <- accepted store demoIntegration "a" (E.IEPropose p)
  eq "MAJ pending with one of three" Nothing (F.irChange pending)
  _ <- accepted store demoIntegration "b" (E.IEDirect (E.AdmitMember "d" "delta@example" pub))
  _ <- accepted store demoIntegration "b" (E.IEDirect (E.AdmitMember "e" "epsilon@example" priv))
  partial <- accepted store demoIntegration "b" (E.IEApprove ident)
  eq "MAJ two-of-five stays pending" Nothing (F.irChange partial)
  assert "MAJ target remains before current quorum" (Map.member "m" (members (F.irState partial)))
  assert "MAJ-NC stale-three-admin threshold detected" (2 < (5+1) `div` (2::Int))
  enacted <- accepted store demoIntegration "c" (E.IEApprove ident)
  eq "MAJ three-of-five enacts" (Just (E.MemberRemoved "m")) (F.irChange enacted)
  assert "MAJ enacted effect removes target" (not (Map.member "m" (members (F.irState enacted))))
  assert "MAJ pending consumed" (Map.notMember ident (pendingBase (F.irState enacted)))
  let expectedMembers = Map.fromList
        [ ("a",Member "a" "alpha@example" pub), ("b",Member "b" "beta@example" priv)
        , ("c",Member "c" "gamma@example" pub), ("d",Member "d" "delta@example" pub)
        , ("e",Member "e" "epsilon@example" priv) ]
      expected = (emptyState (DemoState 7 ["seed","hook admitted d","hook admitted e","hook removed m"])) { members = expectedMembers }
  eq "MAJ exact committed result" expected (F.irState enacted)
  S.closeKEL store

main :: IO ()
main = getArgs >>= \case
  ["R1"] -> viewCase
  ["R3"] -> hookCase
  ["R5"] -> lifecycleCase
  ["MAJ"] -> majorityCase
  _ -> fail "SETUP: expected R1, R3, R5 or MAJ"
