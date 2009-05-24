{-# LANGUAGE StandaloneDeriving, ExistentialQuantification, ScopedTypeVariables #-}
module Rete where
import Prelude hiding (catch)
import Codec.Crypto.RSA
import Data.Digest.Pure.SHA
import Control.Concurrent.STM
import Control.Applicative
import Network
import System.IO

import Control.Exception
import Anagrafe (responsabili)
import qualified Data.ByteString.Lazy.Char8 as B
import Lib0

type UP = (PublicKey,B.ByteString,[String])
type GP = (String,(B.ByteString,[UP]))
type DB = [GP]


type BoardValue = (DB,(String,[UP]),[PublicKey])
mkBoardValue :: String -> [PublicKey] -> BoardValue
mkBoardValue x ys = ([],(x,[]),ys)

data Protocol = Aggiornamento String | Patch UP | UPS | GroupPatch (String,B.ByteString,[PublicKey],B.ByteString) | Validi  deriving (Read,Show)
type SelectorProtocol  = (PublicKey,Protocol)

data PBox = forall a . Show a => PBox a 
instance Show PBox where 
	show (PBox a) = show a
query host p y = do
	h <- connectTo host (PortNumber (fromIntegral p))
	hSetBuffering h NoBuffering
	hPutStrLn h y
	l <- hGetLine h
	case reads l of
		[] -> error "errore di protocollo"
		[(Right l,_)] -> return l
		[(Left l,_)] -> error l

