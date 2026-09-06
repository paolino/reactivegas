{-# LANGUAGE GHC2021, OverloadedStrings, LambdaCase #-}
module Main (main) where
import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar, readMVar, MVar)
import Control.Concurrent.STM (TVar, newTVarIO, atomically, modifyTVar', readTVarIO)
import Control.Exception (SomeException, try, displayException, bracket)
import Control.Monad (forM, unless)
import Data.Aeson (ToJSON(..), Value, decodeStrict)
import Data.Either (isRight)
import Data.List (isInfixOf)
import Data.Text qualified
import Data.Maybe (mapMaybe)
import Database.SQLite.Simple (Only(..), query_, execute_)
import KelGroups.Event qualified as E
import KelGroups.Fold qualified as F
import KelGroups.State (GroupState(..))
import KelGroups.Store qualified as S
import KelGroups.Validate qualified as V
import S28DemoApp
import SkewStore (skewAppend)
import System.Environment (getArgs)
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

-- The codec only records observations. No wait, delay, release or rendezvous here.
data Observed = Observed DemoEvent (TVar Int)
instance ToJSON Observed where toJSON (Observed e count) = observeJSON e count
{-# NOINLINE observeJSON #-}
observeJSON :: DemoEvent -> TVar Int -> Value
observeJSON e count = unsafePerformIO $ do
  atomically (modifyTVar' count (+1))
  pure (toJSON e)
observedIntegration :: F.Integration DemoState Observed DemoProposal DemoError
observedIntegration = F.Integration demoReserved demoDigest demoProposalMutation
  (\signer pre post st (Observed e _) -> demoAppFold signer pre post st e) demoBaseHook

type Append = S.KELStore DemoState -> F.Integration DemoState Observed DemoProposal DemoError -> DataText -> E.IntegratedEvent DemoProposal Observed -> IO (Either (F.IntegratedError DemoError) (F.IntegratedResult DemoState))
-- Alias keeps the concrete candidate and seed at precisely the same harness type.
type DataText = Data.Text.Text

check :: String -> Bool -> IO ()
check label ok = do
  putStrLn (label ++ "=" ++ show ok)
  unless ok (fail ("ASSERTION: " ++ label))
await :: IO a -> IO a
await io = timeout 5000000 io >>= maybe (fail "SETUP: completion timeout, no semantic kill") pure

counts store = do
  hot <- S.readState store
  rows <- S.readEventsFrom store 1
  [Only count] <- query_ (S.storeConn store) "SELECT COUNT(*) FROM events" :: IO [Only Int]
  len <- S.kelLength store
  let decoded = mapMaybe (\se -> (S.seSigner se,) <$> decodeStrict (S.seEventBytes se)) rows
      replay = F.foldIntegratedFrom demoIntegration foundingDemo decoded
  pure (demoCounter (appFold hot),count,len,demoCounter (appFold replay),length decoded)

onePair :: Append -> Int -> Int -> IO Bool
onePair append a b = withSystemTempDirectory "s28-r2-pair" $ \dir -> do
  let file = dir ++ "/pair.db"
  (ok,live) <- bracket (S.openIntegratedKEL demoIntegration foundingDemo file) S.closeKEL $ \store -> do
    observations <- newTVarIO 0
    readyA <- newEmptyMVar
    readyB <- newEmptyMVar
    release <- newEmptyMVar
    doneA <- newEmptyMVar
    doneB <- newEmptyMVar
    let worker n ready done = forkFinally
          (putMVar ready () >> readMVar release >> append store observedIntegration "admin-key-1" (E.IEApp (Observed (DemoAdd n) observations)))
          (putMVar done)
    _ <- worker a readyA doneA
    _ <- worker b readyB doneB
    -- This start gate is outside both implementations, before either public call.
    await (takeMVar readyA >> takeMVar readyB)
    putMVar release ()
    resultA <- await (takeMVar doneA)
    resultB <- await (takeMVar doneB)
    accepted <- case (resultA,resultB) of
      (Right ra,Right rb) -> pure (isRight ra && isRight rb)
      _ -> fail "SETUP: worker exception, no semantic kill"
    tuple <- counts store
    live <- S.readState store
    rows <- S.readEventsFrom store 1
    seqs <- query_ (S.storeConn store) "SELECT seq_no FROM events ORDER BY id" :: IO [Only Int]
    obs <- readTVarIO observations
    let events = traverse (\r -> (S.seSigner r,) <$> decodeStrict (S.seEventBytes r)) rows
        option1 = [("admin-key-1",E.IEApp (DemoAdd a)),("admin-key-1",E.IEApp (DemoAdd b))]
        option2 = reverse option1
        replay = fmap (F.foldIntegratedFrom demoIntegration foundingDemo) events
        ok = accepted && tuple == (a+b,2,2,a+b,2) && (events == Just option1 || events == Just option2) && seqs == [Only 1,Only 2] && replay == Just live
    putStrLn $ "PAIR " ++ show (a,b) ++ " accepted=" ++ show accepted ++ " tuple=" ++ show tuple ++ " seqs=" ++ show seqs ++ " observations=" ++ show obs ++ " exactReplay=" ++ show (replay == Just live) ++ " conserve=" ++ show ok
    pure (ok,live)
  reopened <- bracket (S.openIntegratedKEL demoIntegration foundingDemo file) S.closeKEL S.readState
  let reopenOK = reopened == live
  putStrLn ("PERSISTED-REOPEN agrees=" ++ show reopenOK)
  pure (ok && reopenOK)

conservation :: IO ()
conservation = do
  let pairs = [(1,2),(3,7),(11,19),(101,307),(5,11),(42,43),(1000,7),(0,999)]
      -- Fixed finite schedules; the same 8-pair domain/20 attempts for both paths.
      domain = concat (replicate 20 pairs)
  negative <- forM domain (uncurry (onePair skewAppend))
  check "LOST-UPDATE-SEED detected semantic nonconservation" (not (and negative))
  positive <- forM domain (uncurry (onePair S.appendIntegratedEvent))
  check "FINAL conservation all 160 schedules" (and positive)
  putStrLn "LIMIT: observed finite schedules only; neither overlap completeness nor all interleavings established"

data FaultingCodec = FaultingCodec
instance ToJSON FaultingCodec where toJSON _ = error "AUDIT-SEED-SERIALIZATION-R2"
codec :: IO ()
codec = bracket (S.openIntegratedKEL demoIntegration foundingDemo ":memory:") S.closeKEL $ \store -> do
  let integration = F.Integration demoReserved demoDigest demoProposalMutation
        (\_ _ _ st FaultingCodec -> Right st) demoBaseHook
      event = E.IEApp FaultingCodec
      expected = Left (F.IEValidation (V.NotAMember "outsider"))
      call signer = try (S.appendIntegratedEvent store integration signer event)
        :: IO (Either SomeException (Either (F.IntegratedError DemoError) (F.IntegratedResult DemoState)))
  member <- await (call "admin-key-1")
  check "CODEC accepted seeded exception" (either (isInfixOf "AUDIT-SEED-SERIALIZATION-R2" . displayException) (const False) member)
  counts store >>= check "CODEC accepted exact zero tuple" . (== (0,0,0,0,0))
  check "CODEC pure exact nonmember refusal" (F.applyIntegratedEvent integration foundingDemo "outsider" event == expected)
  nonmember <- await (call "outsider")
  check "CODEC durable exact nonmember refusal" (either (const False) (== expected) nonmember)
  counts store >>= check "CODEC refused exact zero tuple" . (== (0,0,0,0,0))
  S.readState store >>= check "CODEC whole state unchanged" . (== foundingDemo)

lockRelease :: IO ()
lockRelease = bracket (S.openIntegratedKEL demoIntegration foundingDemo ":memory:") S.closeKEL $ \store -> do
  let call n = await (S.appendIntegratedEvent store demoIntegration "admin-key-1" (E.IEApp (DemoAdd n)))
  r1 <- call 1
  r2 <- call 2
  check "SQL sequential control" (isRight r1 && isRight r2)
  before <- counts store
  check "SQL exact baseline" (before == (3,2,2,3,2))
  execute_ (S.storeConn store) "CREATE TRIGGER audit_refuse BEFORE INSERT ON events BEGIN SELECT RAISE(ABORT, 'audit_sql_refusal'); END"
  failed <- try (call 9) :: IO (Either SomeException (Either (F.IntegratedError DemoError) (F.IntegratedResult DemoState)))
  check "SQL seeded failure caller visible" (either (isInfixOf "audit_sql_refusal" . displayException) (const False) failed)
  counts store >>= check "SQL exact conservation" . (== before)
  execute_ (S.storeConn store) "DROP TRIGGER audit_refuse"
  refused <- call (-7)
  check "SQL domain exact refusal" (refused == Left (F.IEApp (DemoNegative (-7))))
  counts store >>= check "SQL refusal unchanged" . (== before)
  continued <- call 4
  check "SQL lock released successful next append" (isRight continued)
  counts store >>= check "SQL exact final tuple" . (== (7,3,3,7,3))

main :: IO ()
main = getArgs >>= \case
  ["conservation"] -> conservation
  ["codec"] -> codec
  ["lock"] -> lockRelease
  _ -> fail "SETUP: conservation|codec|lock required"
