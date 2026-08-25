{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Replays @vectors/reducer.json@ through the real reducer and checks
every step's outcome and post-state projection against the golden
fixture. The PureScript client consumes the same file, so any drift
between this spec and the fixture is a cross-language regression.
-}
module ReduceVectorSpec (
    spec,
) where

import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import Test.Hspec (
    Spec,
    describe,
    it,
    shouldBe,
 )

import Hex (hexDecode)
import Reactivegas.Core.Envelope
import Reactivegas.Core.Projection
import Reactivegas.Core.Reduce

data StepVec = StepVec
    { svEnvelope :: ByteString
    , svReject :: Maybe String
    , svProjection :: Aeson.Value
    }

instance Aeson.FromJSON StepVec where
    parseJSON = Aeson.withObject "StepVec" $ \o ->
        StepVec
            <$> hexF o "envelope"
            <*> o .: "reject"
            <*> o .: "projection"
      where
        hexF obj key = do
            txt <- obj .: key
            either fail pure (hexDecode (TE.encodeUtf8 (txt :: Text)))

data CaseVec = CaseVec
    { cvName :: String
    , cvSteps :: [StepVec]
    }

instance Aeson.FromJSON CaseVec where
    parseJSON = Aeson.withObject "CaseVec" $ \o ->
        CaseVec <$> o .: "name" <*> o .: "steps"

data FileVec = FileVec {fvCases :: [CaseVec]}

instance Aeson.FromJSON FileVec where
    parseJSON = Aeson.withObject "FileVec" $ \o ->
        FileVec <$> o .: "cases"

loadCases :: IO [CaseVec]
loadCases = do
    path <- locateVectorFile
    raw <- BSL.readFile path
    doc <- either fail pure (Aeson.eitherDecode raw)
    pure (fvCases (doc :: FileVec))

locateVectorFile :: IO FilePath
locateVectorFile = getCurrentDirectory >>= go
  where
    go dir =
        let candidate = dir </> "vectors" </> "reducer.json"
         in doesFileExist candidate >>= \found ->
                if found
                    then pure candidate
                    else case takeDirectory dir of
                        parent
                            | parent == dir ->
                                fail "vectors/reducer.json not found above the test directory"
                        parent -> go parent

replayCase :: CaseVec -> IO ()
replayCase c = void (go emptyProjection (cvSteps c))
  where
    go :: Projection -> [StepVec] -> IO Projection
    go _ [] = pure emptyProjection
    go p (s : ss) = do
        env <- either (fail . (("undecodable envelope in " ++ cvName c ++ ": ") ++)) pure (decodeEnvelope (svEnvelope s))
        case step p env of
            Left r -> do
                svReject s `shouldBe` Just (show r)
                go p ss
            Right p' -> do
                svReject s `shouldBe` Nothing
                Aeson.toJSON p' `shouldBe` svProjection s
                go p' ss

spec :: Spec
spec =
    describe "golden reducer vectors" $
        it "replays every fixture case with identical outcomes and projections" $ do
            cases <- loadCases
            for_ cases replayCase
