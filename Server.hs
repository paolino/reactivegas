{-# LANGUAGE StandaloneDeriving, ExistentialQuantification, ScopedTypeVariables #-}

import Prelude hiding (catch)
import Codec.Crypto.RSA
import Data.Digest.Pure.SHA
import Control.Concurrent.STM
import Control.Applicative
import Network
import System.IO

import Control.Exception

import qualified Data.ByteString.Lazy.Char8 as B
import Rete
import Lib0
maxdb = 1000
maxthreads = 1000

type GP = (String,(B.ByteString,[UP]))
type DB = [GP]


data Board = Board {gpatches:: TVar DB, upatches :: TVar [UP], validi :: TVar [PublicKey]}
type Cambiamento = Board -> STM ()
type Query a = Board -> STM a

mkBoard = atomically $ do
	db <- newTVar []
	ups <- newTVar []
	vs <- newTVar []
	return $ Board db ups vs


aggiornamento :: String -> Query [(B.ByteString,[UP])]
aggiornamento x (Board db _ _) = map snd . snd . break ((== x).fst) <$> readTVar db

nuovaUP :: UP -> Cambiamento
nuovaUP up (Board _ tvups _) = readTVar tvups >>= \ups -> writeTVar tvups (up:ups)

type Firmante = [UP] -> GP

nuovaGP :: PublicKey -> (String,B.ByteString,[PublicKey]) -> Query (Either String String)
nuovaGP puk (stato,firma,ws) (Board tvdb tvups tvvs) = do
	ups <- readTVar tvups
	db <- readTVar tvdb
	case verify puk (B.pack (stato ++ show ups)) firma of
		False -> return (Left "test di integrita fallito")
		True -> do
			writeTVar tvups []
			writeTVar tvdb (reverse . take maxdb $ (stato,(firma,ups)) : reverse db)
			writeTVar tvvs ws
			return (Right "Aggiornato")

leggiUPS :: Query [UP]
leggiUPS (Board _ tvups _) = readTVar tvups


protocol :: PublicKey -> String -> Query (Either String PBox)
protocol puk x z = case reads x of 
	[] -> return (Left "Fallimento di protocollo")
	[(Aggiornamento y,_)] -> Right . PBox <$> aggiornamento y z
	[(Patch y,_)] -> Right . PBox <$> nuovaUP y z
	[(UPS,_)] -> Right . PBox  <$> leggiUPS z
	[(GroupPatch y,_)] -> fmap PBox <$> nuovaGP puk y z 

server puk p b = do
	s <- listenOn (PortNumber (fromIntegral  p))
	let r = do
		(h,_,_) <- accept s
		x <- hGetLine h
		l <- atomically (protocol puk x b)
		hPutStrLn h (show l)
		hClose h
		r
	sequence_ (replicate maxthreads r)
		`catch` (\(_::SomeException) -> sClose s)

	
main = do
	s <- read <$> readFile "sincronizzatore.publ"
	mkBoard >>= server s 9090 
