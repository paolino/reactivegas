{-# LANGUAGE GHC2021, OverloadedStrings, LambdaCase #-}
module Main (main) where
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, readMVar, MVar)
import Control.Exception (SomeException, try, displayException)
import Control.Monad (forM, unless, void)
import Data.Aeson (ToJSON(..), Value, decodeStrict)
import Data.Either (isRight)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Database.SQLite.Simple (Only(..), query_, execute_)
import KelGroups.Event qualified as E
import KelGroups.Fold qualified as F
import KelGroups.State (GroupState(..))
import KelGroups.Store qualified as S
import KelGroups.Validate qualified as V
import S28DemoApp
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

-- Test-only schedule control at serialization, with byte-identical DemoEvent JSON.
-- Original candidate: barrier after readState. Repaired candidate forces rendering before its lock/read.
data Scheduled = Scheduled DemoEvent (MVar ()) (MVar ())
instance ToJSON Scheduled where
  toJSON (Scheduled e entered release) = scheduleJSON e entered release
{-# NOINLINE scheduleJSON #-}
scheduleJSON :: DemoEvent -> MVar () -> MVar () -> Value
scheduleJSON e entered release = unsafePerformIO $ do
  putMVar entered ()
  readMVar release
  pure (toJSON e)

scheduledIntegration :: F.Integration DemoState Scheduled DemoProposal DemoError
scheduledIntegration = F.Integration
  { F.intReserved = demoReserved
  , F.intDigest = demoDigest
  , F.intProposalMutation = demoProposalMutation
  , F.intAppFold = \signer pre post st (Scheduled e _ _) -> demoAppFold signer pre post st e
  , F.intBaseHook = demoBaseHook
  }

consistent :: Int -> Int -> Int -> Int -> Bool
consistent expected hot rows hotLength = hot == expected && rows == 2 && hotLength == rows

readCounts store = do
  hot <- S.readState store
  rows <- S.readEventsFrom store 1
  [Only count] <- query_ (S.storeConn store) "SELECT COUNT(*) FROM events" :: IO [Only Int]
  len <- S.kelLength store
  let decoded = mapMaybe (\se -> (S.seSigner se,) <$> decodeStrict (S.seEventBytes se)) rows
      replay = F.foldIntegratedFrom demoIntegration foundingDemo decoded
  pure (demoCounter (appFold hot),count,len,demoCounter (appFold replay),length decoded)

oneConcurrent :: Int -> Int -> IO Bool
oneConcurrent a b = do
  store <- S.openIntegratedKEL demoIntegration foundingDemo ":memory:"
  enteredA <- newEmptyMVar
  enteredB <- newEmptyMVar
  release <- newEmptyMVar
  doneA <- newEmptyMVar
  doneB <- newEmptyMVar
  let worker n entered done = void $ forkIO $ do
        r <- try (S.appendIntegratedEvent store scheduledIntegration "admin-key-1" (E.IEApp (Scheduled (DemoAdd n) entered release)))
        putMVar done (r :: Either SomeException (Either (F.IntegratedError DemoError) (F.IntegratedResult DemoState)))
  worker a enteredA doneA
  worker b enteredB doneB
  both <- timeout 5000000 (takeMVar enteredA >> takeMVar enteredB)
  case both of
    Nothing -> error "SETUP: serialization rendezvous timed out"
    Just () -> pure ()
  putMVar release ()
  results <- timeout 5000000 ((,) <$> takeMVar doneA <*> takeMVar doneB)
  accepted <- case results of
    Just (Right ra,Right rb) -> pure (isRight ra && isRight rb)
    Just (Left e,_) -> error ("SETUP: append exception " ++ displayException e)
    Just (_,Left e) -> error ("SETUP: append exception " ++ displayException e)
    Nothing -> error "SETUP: append completion timed out"
  (hot,rows,len,replay,decoded) <- readCounts store
  live <- S.readState store
  ordered <- S.readEventsFrom store 1
  seqs <- query_ (S.storeConn store) "SELECT seq_no FROM events ORDER BY id" :: IO [Only Int]
  let events = traverse (\se -> (S.seSigner se,) <$> decodeStrict (S.seEventBytes se)) ordered
      option1 = [("admin-key-1",E.IEApp (DemoAdd a)),("admin-key-1",E.IEApp (DemoAdd b))]
      option2 = [("admin-key-1",E.IEApp (DemoAdd b)),("admin-key-1",E.IEApp (DemoAdd a))]
      fullReplay = fmap (F.foldIntegratedFrom demoIntegration foundingDemo) events
      orderedOK = (events == Just option1 || events == Just option2) && seqs == [Only 1,Only 2] && fullReplay == Just live
  putStrLn $ "ORDER-REPLAY values=" ++ show (a,b) ++ " events=" ++ show events ++ " seqs=" ++ show seqs ++ " fullState=" ++ show (fullReplay == Just live)
  let ok = orderedOK && accepted && consistent (a+b) hot rows len && replay == a+b && decoded == 2
  putStrLn $ "CONCURRENT values=" ++ show (a,b) ++ " accepted=" ++ show accepted ++ " hot=" ++ show hot ++ " sqlRows=" ++ show rows ++ " hotLength=" ++ show len ++ " replay=" ++ show replay ++ " decoded=" ++ show decoded ++ " invariant=" ++ show ok
  S.closeKEL store
  pure ok

-- Deliberately faulting application codec: a refused nonmember must never need it.
-- Both paths below are caught; an injected exception is not an uncaught setup crash.
data FaultingCodec = FaultingCodec
instance ToJSON FaultingCodec where
  toJSON _ = error "AUDIT-SEED-SERIALIZATION"

codecRefusal :: IO Bool
codecRefusal = do
  store <- S.openIntegratedKEL demoIntegration foundingDemo ":memory:"
  let integration = F.Integration demoReserved demoDigest demoProposalMutation
        (\_ _ _ st FaultingCodec -> Right st) demoBaseHook
      event = E.IEApp FaultingCodec
      expected = Left (F.IEValidation (V.NotAMember "outsider"))
      pureDecision = F.applyIntegratedEvent integration foundingDemo "outsider" event
  unless (pureDecision == expected) (error "CODEC-CONTROL pure refusal mismatch")
  acceptedFault <- try (S.appendIntegratedEvent store integration "admin-key-1" event)
    :: IO (Either SomeException (Either (F.IntegratedError DemoError) (F.IntegratedResult DemoState)))
  refusedFault <- try (S.appendIntegratedEvent store integration "outsider" event)
    :: IO (Either SomeException (Either (F.IntegratedError DemoError) (F.IntegratedResult DemoState)))
  counts <- readCounts store
  let seeded = either (const True) (const False) acceptedFault
      preservesRefusal = either (const False) (== expected) refusedFault
      ok = seeded && preservesRefusal && counts == (0,0,0,0,0)
  putStrLn $ "CODEC-CONTROL member-exception-observed=" ++ show seeded ++ " pure-nonmember-refusal=" ++ show pureDecision
  putStrLn $ "CODEC-REFUSAL actual=" ++ either displayException show refusedFault ++ " preserves-refusal=" ++ show preservesRefusal ++ " counts=" ++ show counts
  S.closeKEL store
  pure ok

main :: IO ()
main = do
  unless (not (consistent 3 1 2 1)) (error "NEGATIVE-CONTROL failed")
  putStrLn "NEGATIVE-CONTROL detects seeded lost update=True"
  store <- S.openIntegratedKEL demoIntegration foundingDemo ":memory:"
  r1 <- S.appendIntegratedEvent store demoIntegration "admin-key-1" (E.IEApp (DemoAdd 1))
  r2 <- S.appendIntegratedEvent store demoIntegration "admin-key-1" (E.IEApp (DemoAdd 2))
  counts <- readCounts store
  unless (isRight r1 && isRight r2 && counts == (3,2,2,3,2)) (error ("POSITIVE-CONTROL failed " ++ show counts))
  putStrLn $ "POSITIVE-CONTROL sequential counts=" ++ show counts
  -- Real SQLite acquisition/write error: caller must observe the refusal and no hot-state advance.
  execute_ (S.storeConn store) "CREATE TRIGGER audit_refuse BEFORE INSERT ON events BEGIN SELECT RAISE(ABORT, 'audit_sql_refusal'); END"
  failure <- try (S.appendIntegratedEvent store demoIntegration "admin-key-1" (E.IEApp (DemoAdd 9))) :: IO (Either SomeException (Either (F.IntegratedError DemoError) (F.IntegratedResult DemoState)))
  after <- readCounts store
  unless (either (const True) (const False) failure && after == counts) (error "SQL-ERROR observability failure")
  putStrLn $ "SQL-ERROR callerObserved=True unchanged=" ++ show after
  execute_ (S.storeConn store) "DROP TRIGGER audit_refuse"
  invalid <- S.appendIntegratedEvent store demoIntegration "admin-key-1" (E.IEApp (DemoAdd (-7)))
  invalidCounts <- readCounts store
  unless (not (isRight invalid) && invalidCounts == counts) (error "REFUSAL-CONTROL failed")
  continued <- S.appendIntegratedEvent store demoIntegration "admin-key-1" (E.IEApp (DemoAdd 4))
  afterContinue <- readCounts store
  unless (isRight continued && afterContinue == (7,3,3,7,3)) (error "POST-SQL-ERROR lock-release failed")
  putStrLn $ "REFUSAL-CONTROL unchanged=True POST-SQL-ERROR counts=" ++ show afterContinue
  S.closeKEL store
  results <- forM [(1,2),(3,7),(11,19),(101,307),(5,11),(42,43),(1000,7),(0,999)] (uncurry oneConcurrent)
  putStrLn $ "CONSERVATION passing=" ++ show (length (filter id results)) ++ "/8"
  codecOK <- codecRefusal
  unless (and results && codecOK) exitFailure
