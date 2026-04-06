{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Lib.SCGI
Description : SCGI server implementation for network >= 3.0
-}
module Lib.SCGI (
    -- * CGI monad
    CGI,
    CGIT (..),
    CGIResult,
    MonadCGI (..),

    -- * Request handling
    getInput,
    getInputs,
    getVars,
    output,
    setHeader,
    handleErrors,

    -- * Cookies
    Cookie (..),
    getCookie,
    setCookie,
    newCookie,

    -- * SCGI server
    runSCGIConcurrent',
) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, bracket, catch)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Char (isDigit, toLower)
import Data.List (intercalate, lookup)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Network.URI (unEscapeString)
import System.IO (hPutStrLn, stderr)

-- | CGI environment
data CGIEnv = CGIEnv
    { cgiVars :: [(String, String)]
    , cgiInputs :: [(String, String)]
    , cgiBody :: S.ByteString
    }

-- | CGI monad transformer
newtype CGIT m a = CGIT {unCGIT :: ReaderT CGIEnv (WriterT [Header] m) a}
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans CGIT where
    lift = CGIT . lift . lift

-- | CGI monad
type CGI = CGIT IO

-- | CGI result (response body)
type CGIResult = L.ByteString

-- | Response header
data Header = Header String String

-- | Cookie data
data Cookie = Cookie
    { cookieName :: String
    , cookieValue :: String
    , cookieExpires :: Maybe UTCTime
    , cookieDomain :: Maybe String
    , cookiePath :: Maybe String
    , cookieSecure :: Bool
    }

-- | MonadCGI class for CGI operations
class (Monad m) => MonadCGI m where
    cgiGet :: (CGIEnv -> a) -> m a
    cgiAddHeader :: String -> String -> m ()

instance (MonadIO m) => MonadCGI (CGIT m) where
    cgiGet f = CGIT $ asks f
    cgiAddHeader name value = CGIT $ tell [Header name value]

-- | Get a form input by name
getInput :: (MonadCGI m) => String -> m (Maybe String)
getInput name = lookup name <$> getInputs

-- | Get all form inputs
getInputs :: (MonadCGI m) => m [(String, String)]
getInputs = cgiGet cgiInputs

-- | Get all CGI environment variables
getVars :: (MonadCGI m) => m [(String, String)]
getVars = cgiGet cgiVars

-- | Output a string as the response
output :: (MonadCGI m) => String -> m CGIResult
output s = do
    setHeader "Content-Type" "text/html; charset=utf-8"
    return $ L.pack s

-- | Set a response header
setHeader :: (MonadCGI m) => String -> String -> m ()
setHeader = cgiAddHeader

-- | Handle errors in CGI computation
handleErrors :: CGI CGIResult -> CGI CGIResult
handleErrors action =
    action `catchCGI` \e ->
        output $ "Error: " ++ show e
  where
    catchCGI :: CGI a -> (SomeException -> CGI a) -> CGI a
    catchCGI (CGIT m) h = CGIT $ do
        env <- ask
        result <- liftIO $ runCGIT' env m `catch` \e -> runCGIT' env (unCGIT (h e))
        case result of
            (a, hdrs) -> tell hdrs >> return a

runCGIT' :: CGIEnv -> ReaderT CGIEnv (WriterT [Header] IO) a -> IO (a, [Header])
runCGIT' env m = runWriterT (runReaderT m env)

-- | Get a cookie by name
getCookie :: (MonadCGI m) => String -> m (Maybe String)
getCookie name = do
    vars <- getVars
    let cookies = fromMaybe "" $ lookup "HTTP_COOKIE" vars
    return $ lookup name (parseCookies cookies)

parseCookies :: String -> [(String, String)]
parseCookies s = mapMaybe parseCookie (splitOn "; " s)
  where
    parseCookie c = case break (== '=') c of
        (n, '=' : v) -> Just (trim n, v)
        _ -> Nothing
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

splitOn :: String -> String -> [String]
splitOn _ [] = []
splitOn delim str =
    let (before, after) = breakOn delim str
     in before : if null after then [] else splitOn delim (drop (length delim) after)
  where
    breakOn d = go []
      where
        go acc [] = (reverse acc, [])
        go acc xs@(x : xs')
            | d `isPrefixOf'` xs = (reverse acc, xs)
            | otherwise = go (x : acc) xs'
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (p : ps) (x : xs) = p == x && isPrefixOf' ps xs

-- | Create a new cookie
newCookie :: String -> String -> Cookie
newCookie name value =
    Cookie
        { cookieName = name
        , cookieValue = value
        , cookieExpires = Nothing
        , cookieDomain = Nothing
        , cookiePath = Just "/"
        , cookieSecure = False
        }

-- | Set a cookie in the response
setCookie :: (MonadCGI m) => Cookie -> m ()
setCookie cookie = setHeader "Set-Cookie" (formatCookie cookie)

formatCookie :: Cookie -> String
formatCookie c =
    cookieName c
        ++ "="
        ++ cookieValue c
        ++ maybe "" (\e -> "; Expires=" ++ formatExpires e) (cookieExpires c)
        ++ maybe "" ("; Domain=" ++) (cookieDomain c)
        ++ maybe "" ("; Path=" ++) (cookiePath c)
        ++ if cookieSecure c then "; Secure" else ""
  where
    formatExpires = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"

-- | Run an SCGI server concurrently
runSCGIConcurrent' ::
    -- | Fork function (typically forkIO)
    (IO () -> IO ()) ->
    -- | Max concurrent connections (for compatibility, not enforced)
    Int ->
    -- | Port to listen on
    PortNumber ->
    -- | Request handler
    CGI CGIResult ->
    IO ()
runSCGIConcurrent' fork _maxConns port handler = do
    addr <-
        head
            <$> getAddrInfo
                (Just (defaultHints{addrSocketType = Stream}))
                (Just "0.0.0.0")
                (Just (show port))
    bracket (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        listen sock 128
        putStrLn $ "SCGI server listening on port " ++ show port
        forever $ do
            (conn, _) <- accept sock
            void $
                fork $
                    handleConnection conn handler
                        `catch` \(e :: SomeException) -> do
                            hPutStrLn stderr $ "SCGI error: " ++ show e
                            close conn

handleConnection :: Socket -> CGI CGIResult -> IO ()
handleConnection conn handler = do
    raw <- readAll conn S.empty
    case parseSCGI raw of
        Just (headers, body) -> do
            let env = mkEnv headers body
            (result, hdrs) <- runWriterT (runReaderT (unCGIT handler) env)
            sendAll conn (formatResponse hdrs result)
        Nothing ->
            sendAll conn (S.pack "Status: 400 Bad Request\r\n\r\nBad SCGI request")
    close conn

readAll :: Socket -> S.ByteString -> IO S.ByteString
readAll sock acc = do
    chunk <- recv sock 65536
    if S.null chunk
        then return acc
        else readAll sock (acc `S.append` chunk)

-- | Parse SCGI netstring format: "length:headers,body"
parseSCGI :: S.ByteString -> Maybe ([(String, String)], S.ByteString)
parseSCGI input
    | S.null input = Nothing
    | otherwise =
        let (lenStr, rest) = S.span isDigit input
         in if S.null lenStr
                then Nothing
                else
                    let len = read (S.unpack lenStr) :: Int
                        afterColon = S.drop 1 rest
                        (headersBS, afterComma) = S.splitAt len afterColon
                        body = S.drop 1 afterComma
                        headers = parseHeaders headersBS
                     in Just (headers, body)

-- | Parse null-separated SCGI headers
parseHeaders :: S.ByteString -> [(String, String)]
parseHeaders bs = go (S.split '\0' bs)
  where
    go (k : v : rest) = (S.unpack k, S.unpack v) : go rest
    go _ = []

-- | Create CGI environment from headers and body
mkEnv :: [(String, String)] -> S.ByteString -> CGIEnv
mkEnv headers body =
    CGIEnv
        { cgiVars = headers
        , cgiInputs = parseQueryString queryString ++ parseBody headers body
        , cgiBody = body
        }
  where
    queryString = fromMaybe "" $ lookup "QUERY_STRING" headers

-- | Parse query string into key-value pairs
parseQueryString :: String -> [(String, String)]
parseQueryString "" = []
parseQueryString qs = mapMaybe parseParam (splitOn "&" qs)
  where
    parseParam p = case break (== '=') p of
        (k, '=' : v) -> Just (unEscapeString k, unEscapeString v)
        _ -> Nothing

-- | Parse POST body based on content type
parseBody :: [(String, String)] -> S.ByteString -> [(String, String)]
parseBody headers body =
    case lookup "CONTENT_TYPE" headers of
        Just ct
            | "application/x-www-form-urlencoded" `isInfixOf'` map toLower ct ->
                parseQueryString (S.unpack body)
        _ -> []
  where
    isInfixOf' needle haystack = any (needle `isPrefixOf'`) (tails haystack)
    isPrefixOf' [] _ = True
    isPrefixOf' _ [] = False
    isPrefixOf' (p : ps) (x : xs) = p == x && isPrefixOf' ps xs
    tails [] = [[]]
    tails xs@(_ : xs') = xs : tails xs'

-- | Format HTTP response
formatResponse :: [Header] -> CGIResult -> S.ByteString
formatResponse hdrs body =
    S.pack (unlines headerLines ++ "\r\n") `S.append` L.toStrict body
  where
    headerLines =
        if any isStatus hdrs
            then map formatHeader hdrs
            else "Status: 200 OK" : map formatHeader hdrs
    formatHeader (Header n v) = n ++ ": " ++ v
    isStatus (Header n _) = map toLower n == "status"
