{-# LANGUAGE TypeOperators, ViewPatterns, TypeSynonymInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}

module ClientLib where

import System.Environment
import Control.Applicative ((<$>))
import System.IO
import Codec.Crypto.RSA
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad.State
import Control.Monad.Cont
import Control.Monad.Identity
import Data.List
import Control.Arrow
import Data.Maybe
import Control.Monad.Error
import Codec.Binary.UTF8.String
import Data.IORef
import System.Directory
import System.Random
import System.FilePath
import Network

import Core (Reazione)
import Anagrafe
import Costruzione
import Rete
import Controllo 
import Applicazione
import Prioriti
import Control.Monad.Reader
import Control.Concurrent.STM

tagga :: MonadError String m => [Char] -> m a -> m a
tagga x f = f `catchError` (\s -> throwError $ x ++ ":" ++ s)

throwLefts :: Monad m => ErrorT [Char] m a -> m a
throwLefts f = runErrorT f >>= either error return

catchFromIO :: MonadIO m => IO a -> ErrorT String m a
catchFromIO f = ErrorT . liftIO $ ((Right <$> f) `catch` (return . Left . show))

contentReads :: (Read a, MonadError String m) => String -> m a
contentReads x = case reads x of 	
		[] -> throwError $ "errore nell'interpretare " ++ take 50 (show x)
		[(y,_)] -> return y

type Patch = (Maybe (Utente,PrivateKey),[String]) 
data Board = Board (TVar (Maybe Configurazione)) (TVar (Maybe Q)) (TVar Patch)
data Configurazione = Configurazione (HostName,Integer) Q PublicKey deriving (Read,Show)

query' host p y = do
	l <- catchFromIO $ do
		h <- connectTo host (PortNumber (fromIntegral p))
		hSetBuffering h NoBuffering
		hPutStrLn h y
		hGetLine h
	case reads l of
		[] -> error "errore di protocollo"
		[(Right l,_)] -> return l
		[(Left l,_)] -> throwError l

-- query :: (Read b, MonadIO m, Show a) => Board -> a -> ErrorT String m b
query y = tagga "interrogazione servente internet" $ do
	b@(Board tc _ _ ) <- ask
	tc <- liftIO $ atomically (readTVar tc)
	case tc of 
		Nothing -> throwError $ "la configurazione non é stata caricata"
		Just (Configurazione (h,p) _ _) -> query' h (fromIntegral p) (show y) where
			query' host p y = do
				l <- catchFromIO $ do
					h <- connectTo host (PortNumber (fromIntegral p))
					hSetBuffering h NoBuffering
					hPutStrLn h y
					hGetLine h
				case reads l of
					[] -> throwError "errore di protocollo"
					[(Right l,_)] -> return l
					[(Left l,_)] -> throwError l

newtype Program a = Program (ReaderT Board IO a) deriving (Monad, Functor, MonadIO, MonadReader Board )
fromIO = Program . liftIO

runProgram :: Board  -> Program a -> IO a
runProgram b (Program p) = runReaderT p b

instance MonadState Patch Program where
	get = Program $ ask >>= \(Board  _ _ tp) -> liftIO (atomically $ readTVar tp)
	put x = Program $ ask >>= \(Board  _ _ tp) -> liftIO (atomically $ writeTVar tp x) 

creaChiaviIO :: MonadIO m => String -> m (Either String ())
creaChiaviIO l = runErrorT . tagga "creazione chiavi in IO" . catchFromIO $ do
	let (p1,p2) = (l ++ ".priv", l ++ ".publ")
	(pu,pr,_) <- flip generateKeyPair 512 <$> newStdGen
	writeFile p1 (show pr)
	writeFile p2 (show pu)

aggiornamentoIO :: (MonadIO m, MonadReader Board m) => m (Either String (Log Utente))
aggiornamentoIO =  runErrorT . tagga "ggiornamento:" $ do
	b@(Board tc ts tp) <- ask
	tc <- liftIO $ atomically (readTVar tc)
	case tc of 
		Nothing -> throwError $ "la configurazione non é stata caricata"
		Just (Configurazione _ s0 g) -> do 
			((uprk,xs),s') <- liftIO $ atomically (liftM2 (,) (readTVar tp) (readTVar ts))
			let 	s = maybe s0 id s'
				h = showDigest $ sha512 $ B.pack (show s)
			ps <- query (g,Aggiornamento h)
			(s',ls) <- aggiornaStato g s ps 
			vs <- query (g,Validi)
			when (responsabiliQ s' /= vs) $ throwError "Il sicronizzatore sta truffando sui responsabili validi"
			liftIO $ atomically (writeTVar ts (Just s'))
			liftIO $ writeFile "stato" (show s')
			return ls

sincronizzaIO :: (Read a, MonadIO m, MonadReader Board m) => m (Either String a)
sincronizzaIO = runErrorT . tagga "sincronizzazione" $ do
	b@(Board tc ts tp) <- ask
	prk <- tagga "lettura chiave privata sincronizzatore" $  catchFromIO (readFile "sincronizzatore.priv") >>= contentReads
	tc <- liftIO $ atomically (readTVar tc)
	case tc of 
		Nothing -> throwError $ "la configurazione non é stata caricata"
		Just (Configurazione _ s0 puk) -> do 
			(_,s') <- liftIO $ atomically (liftM2 (,) (readTVar tp) (readTVar ts))

			let 	s = maybe s0 id s'
				h = showDigest $ sha512 $ B.pack (show s)
			ps <-  query (puk,UPS)
			let 	ps' = filter (\((pu,firma,es)::UP) -> pu `elem` responsabiliQ s && 
					verify pu (B.pack (h ++ concat es)) firma) ps
				f0 = sign prk (B.pack $ h ++ show ps') 
			(s',ls) <- aggiornaStato puk s [(f0 ,ps')] 
			let 	ws = responsabiliQ $ s'
				f1 = sign prk (B.pack $ h ++ show ws)
				h' = showDigest . sha512 . B.pack $ show s'
			query (puk,GroupPatch (h',f0 ,ws, f1))
			
		
statoCorrettoIO :: (MonadReader Board m, MonadIO m) => [Reazione T c Utente] -> [R] -> m (Either String (T, Log Utente))
statoCorrettoIO rs bs = runErrorT . tagga "computazione stato corretto" $ do
	(Board tc ts tp) <- ask
	tc <- liftIO $ atomically (readTVar tc)
	case tc of 
		Nothing -> throwError $ "la configurazione non é stata caricata"
		Just (Configurazione _ s0 puk) -> do
			((uprk,xs),s') <- liftIO $ atomically (liftM2 (,) (readTVar tp) (readTVar ts))
			let 	s = maybe s0 id s'
			let (r,_,log) = runIdentity . runProgramma rs s  $ 
				case uprk of 
					Nothing -> fst <$> get
					Just (u,prk) -> caricaEventi bs (zip (repeat u) xs) >> (fst <$> get)
			return (r,log)

testAutenticazione = runErrorT . tagga "test autenticazione" $ do
	b@(Board tc ts tp) <- ask
	tc <- liftIO $ atomically (readTVar tc)
	case tc of 
		Nothing -> throwError $ "la configurazione non é stata caricata"
		Just (Configurazione _ s0 puk) -> do

			((uprk,es),s') <- liftIO $ atomically (liftM2 (,) (readTVar tp) (readTVar ts))
			let 	s = maybe s0 id s'
			case uprk of 
				Nothing -> throwError "manca l'autenticazione"
				Just l -> return l

spedizionePatchIO :: (Read a, MonadIO m, MonadReader Board m) => m (Either String a)
spedizionePatchIO  = runErrorT . tagga "spedizione patch di eventi" $ do
	b@(Board tc ts tp) <- ask
	tcr <- liftIO $ atomically (readTVar tc)
	case tcr of 
		Nothing -> throwError $ "la configurazione non é stata caricata"
		Just (Configurazione _ s0 puk) -> do	
			((uprk,es),s') <- liftIO $ atomically (liftM2 (,) (readTVar tp) (readTVar ts))
			let 	s = maybe s0 id s'
			(u,prk) <- case uprk of 
				Nothing -> throwError "manca l'autenticazione"
				Just l -> return l
			pu <- tagga "lettura chiave pubblica responsabile" $ catchFromIO (readFile $ u ++ ".publ") >>= contentReads
			let 	h = showDigest . sha512 . B.pack . show $ s
				r = (pu,sign prk (B.pack $ h ++ concat es),es)
			r <- runErrorT $ query (puk,Patch r) 
			case r of
				Right s -> liftIO $ atomically (writeTVar tp $ (uprk,[]))
				Left _ -> return ()
			ErrorT $ return r

cercaChiaveIO :: (MonadIO m, Read a) => String -> m (Either String a)
cercaChiaveIO s = runErrorT . tagga ("lettura chiave privata di" ++ decodeString s) $ do
	ls <- liftIO $ getDirectoryContents "."
	case find ((==) $ s ++ ".priv") ls of
		Nothing -> throwError "file assente" 
		Just x -> catchFromIO (readFile x) >>= contentReads

			
listaChiaviIO :: (Functor m, MonadIO m) => m [(String,String)]
listaChiaviIO = map (takeBaseName &&& id) . filter ((/=) "sincronizzatore.publ") . filter ((==) ".publ". takeExtension) <$> liftIO (getDirectoryContents ".")


creaGruppo :: [PublicKey] -> Q -> String -> IO (Either String ()) 
creaGruppo puks s n = runErrorT . tagga "creazione file di gruppo" $ do
	p <- tagga "lettura chiave pubblica sincronizzatore" $ 
		catchFromIO (readFile "sincronizzatore.publ") >>= contentReads
	liftIO $ writeFile (n ++ ".gruppo") $ show (p :: PublicKey , mkBoardValue (showDigest . sha512 $ B.pack (show s)) puks)


	
