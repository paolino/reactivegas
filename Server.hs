{-# LANGUAGE StandaloneDeriving, ViewPatterns, ExistentialQuantification, ScopedTypeVariables #-}

import Prelude hiding (catch)
import Codec.Crypto.RSA
import Data.Digest.Pure.SHA
import Control.Concurrent.STM
import Control.Applicative
import Network
import System.IO

import Control.Arrow
import Control.Exception
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import Rete
import Lib0
import Debug.Trace
import Control.Monad.Error

maxdb = 1000
maxthreads = 1000

type GP = (String,(B.ByteString,[UP]))
type DB = [GP]


data Board = Board {gpatches:: TVar DB, nuovo :: TVar (String,[UP]) , validi :: TVar [PublicKey], nt :: TVar Int}


writeBoard (Board db ups ws _) = do
	(db,ups,ws) <- atomically $ do
		db <- readTVar db
		ups <- readTVar ups
		ws <- readTVar ws
		return (db,ups,ws)
	writeFile "server.stato" $ show (db,ups,ws)

readBoard = do
	(db,ups,ws) <- read <$> readFile "server.stato"
	atomically $ do
		db <- newTVar db
		ups <- newTVar ups
		vs <- newTVar ws
		nt <- newTVar 0
		return $ Board db ups vs nt

type Cambiamento = Board -> STM ()
type Query a = Board -> STM a

mkBoard s0 = atomically $ do
	db <- newTVar []
	ups <- newTVar (s0,[])
	vs <- newTVar []
	nt <- newTVar 0
	return $ Board db ups vs nt

checkInizializzato tvdb = do 
	db <- lift $ readTVar tvdb 
	when (null db) $ throwError "Server: Servizio non inizializzato"

aggiornamento :: String -> Query (Either String [(B.ByteString,[UP])])
aggiornamento x (Board tvdb tvups _ _) = do
	runErrorT $ do
		checkInizializzato tvdb
		db <- lift $ readTVar tvdb
		case  map snd . snd . break ((== x).fst) $ db of 
			[] -> do 	(h,_) <- lift $ readTVar tvups
					when (h /= x) $ throwError "Server: lo stato a cui ci si riferisce non esiste"
					return []
			rs -> return rs
				
nuovaUP :: UP -> Query (Either String String)
nuovaUP up@(puk,firma,es) (Board tvdb tvups tvvs _) = do
	runErrorT $ do
		checkInizializzato tvdb
		(s, ups) <- lift $ readTVar tvups 
		vs <- lift $ readTVar tvvs
		when (not $ puk `elem` vs) $ throwError "Server: Responsabile sconosciuto"
		when (not $ verify puk (B.pack $ s ++ concat es) firma)  $ throwError "Server: Patch non integra"
		lift $ writeTVar tvups (s, up: ups)
		return  "Server: Patch accettata"
		
nuovaGP :: PublicKey -> (String,B.ByteString,[PublicKey],B.ByteString) -> Query (Either String String)
nuovaGP puk (s',firma0,ws,firma1) (Board tvdb tvups tvvs _) = do
	db <- readTVar tvdb
	(s,ups) <- readTVar tvups
	runErrorT $ do 
		when (null ups) $ throwError "Server: nessuna patch responsabile ricevuta dall'ultima patch sincronizzatore"
		when (not $  verify puk (B.pack (s ++ show ups)) firma0) $ throwError "Server: Test di integrita patches fallito"
		when (not $ verify puk (B.pack (s ++ show ws)) firma1) $ throwError "Server: Test di integrita responsabili validi fallito"
		lift $ do	writeTVar tvups (s',[])
				writeTVar tvdb (reverse . take maxdb . reverse $ db ++ [(s,(firma0,ups))])
				writeTVar tvvs ws
		return "Server: Aggiornato"

leggiUPS :: Query [UP]
leggiUPS (Board _ tvups _ _) = snd <$> readTVar tvups


protocol :: PublicKey -> String -> Query (Either String PBox)
protocol puk x z = case reads x of 
	[] -> return (Left "Server: Errore di protocollo")
	[(Aggiornamento y,_)] -> fmap  PBox <$> aggiornamento y z
	[(Patch y,_)] -> fmap PBox <$> nuovaUP y z
	[(UPS,_)] -> Right . PBox  <$> leggiUPS z
	[(GroupPatch y,_)] -> fmap PBox <$> nuovaGP puk y z 
	[(Validi,_)] -> Right . PBox <$> readTVar (validi z)

server puk p b@(Board _ _ _ tvnt) = do
	s <- listenOn (PortNumber (fromIntegral  p))
	let t = do
		nt <- atomically (readTVar tvnt)
		if (nt < maxthreads) then do
			(h,_,_) <- accept s
			atomically (writeTVar tvnt (nt + 1))
			forkIO $ do
				flip finally (hClose h) $ do
					x <- hGetLine h
					l <- atomically (protocol puk x b)
					writeBoard b
					hPutStrLn h (show l)
				nt <- atomically (readTVar tvnt)
				atomically (writeTVar tvnt (nt - 1))	
			return ()
		 else threadDelay 1000000
	forever t  `finally` sClose s
	
main = do
	puk <- read <$> readFile "sincronizzatore.publ"
	b <- readBoard `catch` (\(_::IOException) ->  readFile "stato.rif" >>= mkBoard )
	server puk 9090 b
