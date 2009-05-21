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

data Protocol = Aggiornamento String | Patch UP | UPS | GroupPatch (String,B.ByteString,[UP],[PublicKey]) deriving (Read,Show)
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

