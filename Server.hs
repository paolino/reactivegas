{-# LANGUAGE StandaloneDeriving, ExistentialQuantification, ScopedTypeVariables #-}
module Server where
import Prelude hiding (catch)
import Codec.Crypto.RSA
import Data.Digest.Pure.SHA
import Control.Concurrent.STM
import Control.Applicative
import Network
import System.IO

import Control.Exception

import qualified Data.ByteString.Lazy.Char8 as B
import Lib0

type UP = (PublicKey,B.ByteString,[String])

data Protocol = Aggiornamento String | Patch UP | UPS | GroupPatch (String,B.ByteString) deriving (Read,Show)
data PBox = forall a . Show a => PBox a 
instance Show PBox where 
	show (PBox a) = show a

