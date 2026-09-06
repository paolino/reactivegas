{-# LANGUAGE OverloadedStrings #-}

{- | Partial, step-addressed replay of the frozen economic corpus. The
covered extent is discovered from the artifact (selected constructors
only), every covered step is recomputed from its stored input through
the production adapter, and the stored result is matched at the
comparison boundary: the complete applied state, or the refusal with
its guard id and declaration. Unsupported constructors stay outside
the replay and are never stepped as successful no-ops.
-}
module CorpusSpec (spec) where

import Control.Applicative ((<|>))
import Control.Monad (filterM, forM_)
import Data.Aeson (
    FromJSON (..),
    Object,
    Value (..),
    decodeFileStrict',
    withObject,
    (.:),
 )
import Data.Aeson.Key qualified as AKey
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import KelGroups.Types (
    Admin (..),
    GroupView (..),
    Member (..),
    Role (..),
 )
import Reactivegas.Economic.Core (
    CustodyEvent (..),
    Key,
    State (..),
 )
import Reactivegas.Economic.KelGroups (stepInView)
import System.Directory (doesFileExist)
import Test.Hspec

{- | One parsed corpus event: either a selected custody event with its
signer, or a constructor outside the selected surface.
-}
data SelectedEvent
    = Selected Key CustodyEvent
    | Unsupported

-- | The stored result at the comparison boundary.
data CorpusResult
    = AppliedResult (State Value)
    | RefusedResult Text Text

data RawStep = RawStep
    { rsName :: Text
    , rsEvent :: SelectedEvent
    , rsInput :: State Value
    , rsResult :: CorpusResult
    }

newtype RawTrace = RawTrace {rtSteps :: [RawStep]}

data Corpus = Corpus {cView :: GroupView, cTraces :: [RawTrace]}

selectedNames :: [Text]
selectedNames = ["deposit", "withdraw", "transferCassa", "donate"]

isSelected :: Text -> Bool
isSelected n = n `elem` selectedNames

instance FromJSON Corpus where
    parseJSON = withObject "corpus" $ \o -> do
        view <- o .: "view"
        traces <- o .: "traces"
        Corpus
            <$> (unView <$> parseJSON (view :: Value))
            <*> mapM parseJSON (traces :: [Value])

instance FromJSON RawTrace where
    parseJSON = withObject "trace" $ \o -> do
        steps <- o .: "steps"
        RawTrace <$> mapM parseJSON (steps :: [Value])

instance FromJSON RawStep where
    parseJSON = withObject "step" $ \o -> do
        (name, sel) <- eventOf o
        input <- o .: "input" >>= corpusState
        result <- o .: "result" >>= resultOf
        pure (RawStep name sel input result)

corpusState :: Value -> Parser (State Value)
corpusState = withObject "state" $ \o -> do
    contiV <- o .: "conti"
    casseV <- o .: "casse"
    let untouchedV = Object (KM.delete "conti" (KM.delete "casse" o))
    pure (State contiV casseV untouchedV)

eventOf :: Object -> Parser (Text, SelectedEvent)
eventOf o = do
    ev <- o .: "event"
    case ev of
        Object eo
            | KM.size eo == 1
            , [(k, body)] <- KM.toList eo ->
                selected (AKey.toText k) body
        _ -> fail "corpus event must be a single-key object"
  where
    selected name body = case name of
        "deposit" ->
            (\(a, u, v) -> (name, Selected a (Deposit u v)))
                <$> memberAmount body
        "withdraw" ->
            (\(a, u, v) -> (name, Selected a (Withdraw u v)))
                <$> memberAmount body
        "transferCassa" ->
            (\(a, f, v) -> (name, Selected a (TransferCassa f v)))
                <$> transferAmount body
        "donate" ->
            (\(a, v) -> (name, Selected a (Donate v)))
                <$> donateAmount body
        _ -> pure (name, Unsupported)

memberAmount :: Value -> Parser (Key, Key, Integer)
memberAmount = withObject "event body" $ \o -> do
    author <- o .: "author"
    user <- o .: "user"
    v <- o .: "v"
    pure (author, user, v)

transferAmount :: Value -> Parser (Key, Key, Integer)
transferAmount = withObject "event body" $ \o -> do
    author <- o .: "author"
    from <- o .: "f"
    v <- o .: "v"
    pure (author, from, v)

donateAmount :: Value -> Parser (Key, Integer)
donateAmount = withObject "event body" $ \o -> do
    author <- o .: "author"
    v <- o .: "v"
    pure (author, v)

resultOf :: Value -> Parser CorpusResult
resultOf = withObject "result" $ \o -> do
    tag <- o .: "tag"
    case tag :: Text of
        "applied" -> AppliedResult <$> (o .: "state" >>= corpusState)
        "refused" -> do
            guardV <- o .: "guard"
            (gid, decl) <-
                withObject
                    "guard"
                    ( \g -> do
                        i <- g .: "id"
                        d <- g .: "declaration"
                        pure (i, d)
                    )
                    guardV
            pure (RefusedResult gid decl)
        other -> fail ("unknown corpus result tag: " <> T.unpack other)

newtype RoleJSON = RoleJSON {unRole :: Role}

instance FromJSON RoleJSON where
    parseJSON = withObject "role" $ \o ->
        (RoleJSON (AdminRole PublicAdmin) <$ (o .: "adminRole" :: Parser Object))
            <|> (RoleJSON . AppRole <$> (o .: "appRole" :: Parser Text))

data MemberJSON = MemberJSON {mjKey :: Key, mjMember :: Member}

instance FromJSON MemberJSON where
    parseJSON = withObject "member-entry" $ \o -> do
        key <- o .: "key"
        m <- o .: "member"
        (email :: Text) <- withObject "member" (.: "email") m
        (roles :: [RoleJSON]) <- withObject "member" (.: "roles") m
        pure
            ( MemberJSON
                key
                (Member key email (Set.fromList (map unRole roles)))
            )

newtype ViewJSON = ViewJSON {unView :: GroupView}

instance FromJSON ViewJSON where
    parseJSON = withObject "view" $ \o -> do
        members <- o .: "members"
        let ms = members :: [MemberJSON]
        pure (ViewJSON (GroupView (Map.fromList [(mjKey m, mjMember m) | m <- ms])))

corpusCandidates :: [FilePath]
corpusCandidates =
    [ "lean/corpus/economic.json"
    , "../lean/corpus/economic.json"
    , "../../lean/corpus/economic.json"
    ]

corpusPath :: IO FilePath
corpusPath = do
    hits <- filterM doesFileExist corpusCandidates
    case hits of
        (p : _) -> pure p
        [] ->
            fail
                "frozen corpus lean/corpus/economic.json not found relative to the test working directory"

loadCorpus :: IO (GroupView, [(Int, Int, RawStep)])
loadCorpus = do
    path <- corpusPath
    decoded <- decodeFileStrict' path
    corpus <- case decoded of
        Nothing -> fail ("cannot parse the frozen corpus at " <> path)
        Just c -> pure c
    let steps =
            [ (t, i, s)
            | (t, trace) <- zip [0 :: Int ..] (cTraces corpus)
            , (i, s) <- zip [0 :: Int ..] (rtSteps trace)
            ]
    pure (cView corpus, steps)

thirdOf :: (Int, Int, RawStep) -> RawStep
thirdOf (_, _, s) = s

where_ :: (Int, Int) -> RawStep -> String
where_ (t, i) s =
    "trace "
        <> show t
        <> " step "
        <> show i
        <> " ("
        <> T.unpack (rsName s)
        <> "): "

replayStep :: GroupView -> (Int, Int) -> RawStep -> IO ()
replayStep view loc s = case rsEvent s of
    Unsupported ->
        expectationFailure (where_ loc s <> "unsupported constructor in covered set")
    Selected signer ev -> case rsResult s of
        AppliedResult want -> case stepInView view (rsInput s) signer ev of
            Nothing ->
                expectationFailure (where_ loc s <> "refused where the corpus applied")
            Just got -> do
                conti got `shouldBe` conti want
                casse got `shouldBe` casse want
                untouched got `shouldBe` untouched want
        RefusedResult gid decl -> do
            stepInView view (rsInput s) signer ev `shouldBe` Nothing
            gid `shouldBe` rsName s
            decl `shouldSatisfy` not . T.null

spec :: Spec
spec = before loadCorpus $
    describe "frozen corpus replay (partial, step-addressed)" $ do
        it "discovers the pinned selected extent" $ \(_, steps) -> do
            let covered = filter (isSelected . rsName . thirdOf) steps
                unsupported = filter (not . isSelected . rsName . thirdOf) steps
                counts =
                    Map.fromListWith
                        (+)
                        [(rsName (thirdOf s), 1 :: Int) | s <- covered]
                count n = maybe 0 id (Map.lookup n counts)
            Map.lookup "deposit" counts `shouldBe` Just 7
            Map.lookup "withdraw" counts `shouldBe` Just 1
            Map.lookup "donate" counts `shouldBe` Just 1
            Map.lookup "transferCassa" counts `shouldBe` Nothing
            length unsupported `shouldSatisfy` (> 0)
            putStrLn $
                "CORPUS-EXTENT deposit="
                    <> show (count "deposit")
                    <> " withdraw="
                    <> show (count "withdraw")
                    <> " donate="
                    <> show (count "donate")
                    <> " transferCassa="
                    <> show (count "transferCassa")
                    <> " unsupported-skipped="
                    <> show (length unsupported)
        it "replays every covered selected step from its stored input" $ \(view, steps) ->
            forM_
                (filter (isSelected . rsName . thirdOf) steps)
                (\s@(_, _, raw) -> replayStep view (fst3 s, snd3 s) raw)
        it "never steps an unsupported constructor as a successful no-op" $ \(_, steps) -> do
            let unsupportedNames =
                    Set.fromList
                        [rsName (thirdOf s) | s <- steps, not (isSelected (rsName (thirdOf s)))]
            Set.null unsupportedNames `shouldBe` False
            Set.disjoint unsupportedNames (Set.fromList selectedNames) `shouldBe` True
  where
    fst3 (a, _, _) = a
    snd3 (_, b, _) = b
