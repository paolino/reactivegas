{-# LANGUAGE StandaloneDeriving, ViewPatterns, ExistentialQuantification, ScopedTypeVariables #-}

import Prelude hiding (catch)
import Codec.Crypto.RSA
import Data.Digest.Pure.SHA
import Control.Concurrent.STM
import Control.Applicative
import Network
import System.IO
import System.Directory
import System.FilePath
import System.Random
import Data.Maybe

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


data Board = Board {gpatches:: TVar DB, nuovo :: TVar (String,[UP]) , validi :: TVar [PublicKey]}

data  Boards = Boards {boards :: TVar [(PublicKey,Board)], nt :: TVar Int}

writeBoard (Board db ups ws) = do
		db <- readTVar db
		ups <- readTVar ups
		ws <- readTVar ws
		return (db,ups,ws)
		
writeBoards (Boards bs _) = do
	r <- atomically $ do 
		(unzip -> (puks,bs'))  <- readTVar bs 
		zip puks <$> mapM writeBoard bs'
	writeFile "server.stato" $ show r

readBoard (db,ups,ws) = do
		db <- newTVar db
		ups <- newTVar ups
		vs <- newTVar ws
		return $ Board db ups vs 

readBoards :: IO (Boards)		
readBoards = do
	(unzip -> (puks,bs)) <- read <$> readFile "server.stato" 
	atomically $ do
		bs' <- mapM readBoard bs
		bs'' <- newTVar (zip puks bs')
		Boards bs'' <$>  newTVar 0

mkBoards = atomically $ liftM2 Boards (newTVar []) (newTVar 0)

type Gruppo = (PublicKey,BoardValue)

presences :: IO [Gruppo]
presences = do
	ls <- getDirectoryContents "."
	z <- map read <$> mapM readFile (filter (\x -> takeExtension x == ".gruppo") ls)
	return z

aggiornaGruppi :: TVar [(PublicKey,Board)] -> [Gruppo] -> STM ()
aggiornaGruppi tv ls = do
	attivi <-  readTVar tv
	ns <- forM (filter (\(i,_) -> not $ i `elem` map fst attivi) ls) $ \(i,b) -> (,) i <$> readBoard  b
	writeTVar tv (filter (\(i,_)  ->  i `elem` map fst ls) attivi ++ ns) 

updateService t (Boards bs _) = forever $ do
	threadDelay (t*1000000)
	ls <- presences `catch` (\(e::SomeException) -> print e >> return [])
	(atomically $ aggiornaGruppi bs ls) `catch` (\(e::SomeException) -> print e)

--------------------------------------------------------------------
	
checkInizializzato tvdb = do 
	db <- lift $ readTVar tvdb 
	when (null db) $ throwError "Server: Servizio non inizializzato"

aggiornamento :: String -> Board -> ErrorT String STM [(B.ByteString,[UP])]
aggiornamento x (Board tvdb tvups _ ) = do
	checkInizializzato tvdb
	db <- lift $ readTVar tvdb
	case  map snd . snd . break ((== x).fst) $ db of 
		[] -> do 	(h,_) <- lift $ readTVar tvups
				when (h /= x) $ throwError "Server: lo stato a cui ci si riferisce non esiste"
				return []
		rs -> return rs
				
-- nuovaUP :: UP -> Query (Either String String)
nuovaUP up@(puk,firma,es) (Board tvdb tvups tvvs ) = do
	checkInizializzato tvdb
	(s, ups) <- lift $ readTVar tvups 
	vs <- lift $ readTVar tvvs
	when (not $ puk `elem` vs) $ throwError "Server: Responsabile sconosciuto"
	when (not $ verify puk (B.pack $ s ++ concat es) firma)  $ throwError "Server: Patch non integra"
	lift $ writeTVar tvups (s, up: ups)
	return  "Server: Patch accettata"
		
-- nuovaGP :: PublicKey -> (String,B.ByteString,[PublicKey],B.ByteString) -> Query (Either String String)
nuovaGP puk (s',firma0,ws,firma1) (Board tvdb tvups tvvs ) = do
	db <- lift $ readTVar tvdb
	(s,ups) <- lift $ readTVar tvups
	when (null ups && not (null db)) $ throwError "Server: nessuna patch responsabile ricevuta dall'ultima patch sincronizzatore"
	when (not $  verify puk (B.pack (s ++ show ups)) firma0) $ throwError "Server: Test di integrita patches fallito"
	when (not $ verify puk (B.pack (s ++ show ws)) firma1) $ throwError "Server: Test di integrita responsabili validi fallito"
	lift $ do	writeTVar tvups (s',[])
			writeTVar tvdb (reverse . take maxdb . reverse $ db ++ [(s,(firma0,ups))])
			writeTVar tvvs ws
	return "Server: Aggiornato"

-- leggiUPS :: Query [UP]
leggiUPS (Board _ tvups _ ) = snd <$> lift (readTVar tvups)
----------------------------------------------------------------

protocol :: String -> Boards -> STM (Either String PBox)
protocol x (Boards tvbs _) = runErrorT $ 
	case reads x of 
		[] -> throwError "Server: Errore di protocollo"
		[((puk,l),_)] -> do
			bs  <- lift $ readTVar tvbs
			when (not $ puk `elem` map fst bs) $ throwError "Server: richiesta su un gruppo non attivo"
			let b = fromJust $ lookup puk bs
			case l of
				Aggiornamento y -> PBox <$> aggiornamento y b
				Patch y -> PBox <$> nuovaUP y b
				UPS -> PBox  <$> leggiUPS b
				GroupPatch y -> PBox <$> nuovaGP puk y b
				Validi -> PBox <$> lift (readTVar (validi b))
				

server p b@(Boards _ tvnt) = do
	s <- listenOn (PortNumber (fromIntegral  p))
	let t = do
		nt <- atomically (readTVar tvnt)
		if (nt < maxthreads) then do
			(h,_,_) <- accept s
			atomically (writeTVar tvnt (nt + 1))
			forkIO $ do
				flip finally (hClose h) $ do
					x <- hGetLine h
					l <- atomically (protocol x b)
					writeBoards b
					hPutStrLn h (show l)
				nt <- atomically (readTVar tvnt)
				atomically (writeTVar tvnt (nt - 1))	
			return ()
		 else threadDelay 1000000
	forever t  `finally` sClose s
main = do
	b <- readBoards `catch` (\(_::IOException) -> mkBoards )
	forkIO (updateService 5 b)
	server 1433 b

