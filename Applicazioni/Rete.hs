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

