{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Server.Opzioni (
    parseArgs,
    Argomenti (..),
) where

import Lib.Tokens (Token (..))
import Options.Applicative (
    Parser,
    auto,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    showDefault,
    strArgument,
    value,
 )

data Argomenti = Argomenti
    { directory :: String
    -- ^ working directory
    , porta :: Int
    -- ^ CGI port
    , lmov :: Int
    -- ^ max group movements queue size
    , lsess :: Int
    -- ^ max memories per session
    , lrem :: Int
    -- ^ max simultaneous sessions
    , tokpass :: Token
    -- ^ token read password
    }
    deriving (Show)

argomentiParser :: Parser Argomenti
argomentiParser = do
    porta <-
        option
            auto
            ( long "port"
                <> short 'p'
                <> metavar "PORT"
                <> value 5000
                <> showDefault
                <> help "CGI server port"
            )
    lmov <-
        option
            auto
            ( long "movements"
                <> short 'm'
                <> metavar "COUNT"
                <> value 15
                <> showDefault
                <> help "Max group movements in memory"
            )
    lsess <-
        option
            auto
            ( long "sessions"
                <> short 's'
                <> metavar "COUNT"
                <> value 200
                <> showDefault
                <> help "Max sessions in memory"
            )
    lrem <-
        option
            auto
            ( long "memories"
                <> short 'r'
                <> metavar "COUNT"
                <> value 20
                <> showDefault
                <> help "Max memories per session"
            )
    directory <-
        strArgument
            ( metavar "DIRECTORY"
                <> help "Working directory"
            )
    tokpass <-
        Token
            <$> strArgument
                ( metavar "PASSWORD"
                    <> help "Administration password"
                )
    pure Argomenti{..}

parseArgs :: IO Argomenti
parseArgs =
    execParser $
        info
            (helper <*> argomentiParser)
            ( fullDesc
                <> progDesc "Run the reactivegas server"
                <> header "reactivegas - social tool for groups"
            )
