{-# LANGUAGE TypeOperators, ViewPatterns, TypeSynonymInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
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

import Data.Maybe
import Control.Monad.Error
import Codec.Binary.UTF8.String
import System.Directory
import System.Random
import Network

import Core (Reazione)
import Anagrafe
import MakePatch 
import Costruzione
import Rete
import Controllo 
import Applicazione
import Prioriti
import Control.Monad.Reader
import Control.Concurrent.STM

tagga x f = f `catchError` (\s -> throwError $ x ++ ":" ++ s)

throwLefts f = do 
	x <- runErrorT f 
	case x of 	Left y -> error y
			Right y -> return y	
catchFromIO f = ErrorT ((Right <$> f) `catch` (return . Left . show))

liftErrorT :: (MonadIO m) => ErrorT e IO a -> ErrorT e m a
liftErrorT f = ErrorT (liftIO (runErrorT f))

contentReads x = case reads x of 	
		[] -> throwError $ "errore nell'interpretare " ++ show x
		[(y,_)] -> return y

data Board = Board Configurazione PublicKey (TVar Q) (TVar Patch)
data Configurazione = Configurazione HostName Integer deriving (Read,Show)

query host p y = do
	l <- liftErrorT . catchFromIO $ do
		h <- connectTo host (PortNumber (fromIntegral p))
		hSetBuffering h NoBuffering
		hPutStrLn h y
		hGetLine h
	case reads l of
		[] -> error "errore di protocollo"
		[(Right l,_)] -> return l
		[(Left l,_)] -> throwError l

query' (Board (Configurazione h p) _ _ _) = query h (fromIntegral p) . show

newtype Program a = Program (ReaderT Board IO a) deriving (Monad, Functor, MonadIO, MonadReader Board )
fromIO = Program . liftIO

runProgram :: Board  -> Program a -> IO a
runProgram b (Program p) = runReaderT p b

instance MonadState Patch Program where
	get = Program $ ask >>= \(Board _ _ _ tp) -> liftIO (atomically $ readTVar tp)
	put x = Program $ ask >>= \(Board _ _ _ tp) -> liftIO (atomically $ writeTVar tp x)

main =	do
	print "lettura cartella ............"
	b <- throwLefts $ do 
		c <- tagga "lettura configurazione" $ catchFromIO (readFile "configurazione") >>= contentReads 
		s <- tagga "lettura file di stato" $ catchFromIO (readFile "stato" >>= \x -> length x `seq` return x ) >>= contentReads >>= lift . atomically . newTVar
		puk <- tagga "lettura chiave pubblica sincronizzatore" $ catchFromIO (readFile "sincronizzatore.publ") >>= contentReads
		pa <- lift . atomically . newTVar =<< do 	t <- lift . runErrorT $ tagga "lettura patch" $ catchFromIO (readFile "patch") >>= contentReads 
								either (\e -> liftIO (print e) >> return (Nothing, [])) return t
		return $ Board c puk s pa 
		
	hSetBuffering stdout NoBuffering 
	runProgram b (svolgi interfaccia >>= runCostruzioneIO) 

creaChiaviIO l = runErrorT . tagga "creazione chiavi in IO" . liftErrorT . catchFromIO $ do
	let (p1,p2) = (l ++ ".priv", l ++ ".publ")
	(pu,pr,_) <- flip generateKeyPair 512 <$> newStdGen
	writeFile p1 (show pr)
	writeFile p2 (show pu)

aggiornamentoIO pl =  runErrorT . tagga "aggiornamentoIO:" $ do
	b@(Board _ g ts tp) <- ask
	((uprk,xs),s) <- liftIO $ atomically (liftM2 (,) (readTVar tp) (readTVar ts))
	let h = showDigest $ sha512 $ B.pack (show s)
	ps <- query' b (g,Aggiornamento h)
	(s',ls) <- aggiornaStato g s ps 
	pl ls
	vs <- query' b (g,Validi)
	when (responsabiliQ s' /= vs) $ throwError "Il sicronizzatore sta truffando sui responsabili validi"
	liftIO $ atomically (writeTVar ts s')
	liftIO $ writeFile "stato" (show s')

sincronizzaIO = runErrorT . tagga "sincronizzazioneIO:" $ do
	b@(Board _ puk ts tp) <- ask
	(_,s) <- liftIO $ atomically (liftM2 (,) (readTVar tp) (readTVar ts))
	let h = showDigest $ sha512 $ B.pack (show s)
	prk <- liftErrorT $ tagga "lettura chiave privata sincronizzatore" $ catchFromIO (readFile "sincronizzatore.priv") >>= contentReads
	ps <-  query' b (puk,UPS)
	let 	ps' = filter (\((pu,firma,es)::UP) -> pu `elem` responsabiliQ s && 
			verify pu (B.pack (h ++ concat es)) firma) ps
		f0 = sign prk (B.pack $ h ++ show ps') 
	(s',ls) <- aggiornaStato puk s [(f0 ,ps')] 
	let 	ws = responsabiliQ $ s'
		f1 = sign prk (B.pack $ h ++ show ws)
		h' = showDigest . sha512 . B.pack $ show s'
	query' b (puk,GroupPatch (h',f0 ,ws, f1))
		

-- interfaccia :: (String,Int) -> PublicKey -> String -> MakePatch r m ()
interfaccia = let c = correggiStato (liftIO . stampaLogs) reattori priorities in 
	nodo (liftIO . logerrore) [
		nuovechiavi creaChiaviIO , 
		sincronizzazione sincronizzaIO , 
		update $ aggiornamentoIO (liftIO . stampaLogs),
		commit send, 

		autenticazione (c, liftIO . cercaChiave), 
		const $ trattamentoEvento (liftIO . logerrore, c) makers
		]

logerrore  =  putStrLn .( ++ "****" ) . ("****" ++)

correggiStato :: (MonadIO m, MonadReader Board m)
	=> (Log Utente -> m a)
	-> [Reazione T c Utente]
	-> [R]
	-> m T

correggiStato pl rs bs = do
	(Board _ _ ts tp) <- ask
	((uprk,xs),s) <- liftIO $ atomically (liftM2 (,) (readTVar tp) (readTVar ts))
	let ys = case uprk of 
		Just (u,prk) -> zip (repeat u) xs
		Nothing -> []
	case null ys of
		False -> let (r,_,log) = runIdentity $ runProgramma rs s (caricaEventi bs ys >> (fst <$> get)) in
			pl log >> return r
		True -> let (r,_,log) = runIdentity $ runProgramma rs s (fst <$> get) in return r


send :: (MonadIO m, MonadReader Board m) => (Utente,PrivateKey,[String]) ->  m (Either String String)
send  (u,prk,es) = runErrorT . tagga "spedizione patch di eventi" $ do
	b@(Board _ puk ts tp) <- ask
	pu <- liftErrorT $ tagga "lettura chiave pubblica responsabile" $ catchFromIO (readFile $ u ++ ".publ") >>= contentReads
	s <- liftIO $ atomically (readTVar ts)
	let 	h = showDigest . sha512 . B.pack . show $ s
	 	r = (pu,sign prk (B.pack $ h ++ concat es),es)
	query' b (puk,Patch r) 
cercaChiave s = runErrorT . tagga ("lettura chiave privata di" ++ decodeString s) $ do
	ls <- liftIO $ getDirectoryContents "."
	case find ((==) $ s ++ ".priv") ls of
		Nothing -> throwError "file assente" 
		Just x -> catchFromIO (readFile x) >>= contentReads
			
runCostruzioneIO :: (Monad m, MonadIO m) =>  Costruzione m  a -> m (Maybe a)
runCostruzioneIO  c = flip runContT return . callCC $ \k -> 
	let zeta c@(Costruzione l f) = do 
		let riprova s  = nl >> msg s >> zeta c
		r <- runErrorT $ nl >> case l of
			Libero z -> do	(encodeString -> x) <- pgl z
					backifnull (errorOrAhead (lift . lift . f) . stringOrNot) x
			Scelta t as -> do 	let bs = ("fine",undefined) : as
						liftIO $ mapM_ putStrLn  [show n ++ ". " ++ decodeString a | (n,a) <- zip [0..] (map fst bs)]
						nl
						let 	q y = do 	when (y < 0 || y > length as) $ throwError "scelta impossibile"
									when (y == 0) $ lift (k Nothing)
									lift . lift .  f . snd $ (as !! (y - 1))	
						pgl t >>= backifnull (errorOrAhead q . toMaybe . reads)
		either riprova return r 
		where
			backifnull f x = if null x then return Nothing else f x
			errorOrAhead q =  maybe (throwError "errore di lettura") 
				(\x -> q x >>= lift . zeta >>= maybe (lift $ zeta c) (return . Just)) 
			stringOrNot s = toMaybe $ case reads s of
				[] -> reads ("\"" ++ s ++ "\"")
				x -> x
			nl 	= liftIO (putStrLn "")
			prompt 	= liftIO . putStr . (++ ": ")
			pgl s 	= prompt s >> gl
			msg s 	=  nl >> (liftIO . putStrLn . (++ " ****").("**** " ++). decodeString $ s)
			gl 	= liftIO getLine
			toMaybe =  fmap fst . listToMaybe
	in zeta c			
			

	
