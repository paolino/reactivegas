{- |
Module      : Main
Description : kelgroups-server executable
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0
-}
module Main (main) where

import Control.Concurrent.STM (newBroadcastTChanIO)
import Control.Exception (bracket)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text, pack)
import KelGroups.Jwk
    ( decodeJwkJson
    , encodeJwkJson
    , keyPairToJwk
    )
import KelGroups.Server (ServerEnv (..), mkApp)
import KelGroups.Store
    ( closeKEL
    , openKEL
    , openKELWithIdentity
    , serverCesrKey
    , serverKeyPair
    )
import KelGroups.Trivial
    ( trivialConfig
    , trivialFold
    , trivialInitial
    )
import Network.Wai.Application.Static
    ( defaultFileServerSettings
    , staticApp
    )
import Network.Wai.Handler.Warp qualified as Warp
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
    ( hPutStrLn
    , stderr
    )

usage :: String
usage =
    unlines
        [ "Usage:"
        , "  kelgroups-server <port> <db> <pass>"
        , "  kelgroups-server export-key <db>"
        , "  kelgroups-server import-key <db> <jwk-file>"
        ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["export-key", dbPath] ->
            exportKey dbPath
        ["import-key", dbPath, jwkPath] ->
            importKey dbPath jwkPath
        [portStr, dbPath, pass] ->
            let port = read portStr
            in  runServer port dbPath (pack pass)
        _ -> putStr usage

runServer :: Int -> FilePath -> Text -> IO ()
runServer port dbPath passphrase =
    bracket
        (openKEL trivialFold trivialInitial dbPath)
        closeKEL
        $ \store -> do
            ch <- newBroadcastTChanIO
            let env =
                    ServerEnv
                        { envStore = store
                        , envConfig = trivialConfig
                        , envAppFold = trivialFold
                        , envPassphrase = passphrase
                        , envBroadcast = ch
                        }
                staticDir =
                    "client/kelgroups-trivial/dist"
                fallback =
                    staticApp
                        (defaultFileServerSettings staticDir)
                app = mkApp env (Just fallback)
            putStrLn $
                "Listening on port " <> show port
            Warp.run port app

-- | Write the server private key as JWK JSON to stdout.
exportKey :: FilePath -> IO ()
exportKey dbPath =
    bracket
        (openKEL trivialFold trivialInitial dbPath)
        closeKEL
        $ \store -> do
            LBS.putStr (encodeJwkJson (keyPairToJwk (serverKeyPair store)))
            putStrLn ""

{- | Load a JWK file and install it as the server
identity of a fresh database. Refuses stores that
already have an identity or events.
-}
importKey :: FilePath -> FilePath -> IO ()
importKey dbPath jwkPath = do
    raw <- LBS.readFile jwkPath
    case decodeJwkJson raw of
        Left err -> do
            hPutStrLn stderr ("Invalid JWK: " <> err)
            exitFailure
        Right kp -> do
            store <-
                openKELWithIdentity
                    trivialFold
                    trivialInitial
                    dbPath
                    kp
            putStrLn
                ( "Server identity imported: "
                    <> show (serverCesrKey store)
                )
            closeKEL store
