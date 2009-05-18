{-# LANGUAGE TypeOperators, ViewPatterns, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
module MakePatch where

import Core 
import Serializzazione
import Controllo

import Aspetti (ParteDi) 
import Costruzione 
import Anagrafe (responsabili, Responsabili, Utente)
import Prioriti

import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity

import System.Environment
import Data.List
import Control.Arrow
import Control.Applicative ((<$>))
import Data.Maybe
import System.Directory
import Codec.Binary.UTF8.String
import Control.Monad.Error
import Codec.Crypto.RSA (PrivateKey)

----------------------------------- sezione input nuovo evento -----------------------------------------
-- | uno Svolgimento specializzato alla costruzione di una String (si intende uno show di evento)
type MEvento r m a = Svolgimento r String m a 

autenticazione :: (ParteDi Responsabili r, Monad m) =>
		(Utente -> m (Maybe PrivateKey)) 
		-> MEvento r m (Utente,PrivateKey)
autenticazione q = z where	
	z = do	rs <- map fst . responsabili <$> ask -- lo stato si trova nella reader
		p <- parametro $ Scelta "autore degli eventi" (map (id &&& id) rs)
		t <- lift (q p) -- usiamo la monade parametrica per trovare la chiave privata del responsabile scelto
		case t of 	Nothing -> z
				Just  n -> return (p,n)

type Chiusura r m = String -> MEvento r m ()
costruttori :: (Monad m) =>
               [Chiusura r m -> [(String, MEvento r m String)]]
               -> Chiusura r m 
               -> MEvento r m (Maybe String)
costruttori cs k = join . parametro $ Scelta "selezione dell'evento da produrre" (map (second $ fmap Just) $ concatMap ($k) cs)

inserimentoEvento :: (Monad m) =>
                     [Chiusura r m -> [(String, MEvento r m String)]]
                     -> (Utente -> PrivateKey ->  String -> m r)
                     -> (Utente,PrivateKey)
                     -> MEvento r m String		
inserimentoEvento cs q (w,prk) = evento where
		evento = do 
			r <- callCC $ costruttori cs . flip (.) Just
			case r of 	
				Nothing -> evento 
				Just x -> do 	s <- lift (q w prk x)
						local (const s) evento

----------------------  un driver per utilizzare una Costruzione ----------------------------------------						
runCostruzioneIO :: (Monad m, MonadIO m) =>  Costruzione m r String -> m (Maybe String)
runCostruzioneIO  c = flip runContT return . callCC $ \k -> 
	let zeta c@(Costruzione l f) = do 
		let riprova s  = nl >> msg s >> zeta c
		r <- runErrorT $ nl >> case l of
			Libero z -> do	(encodeString -> x) <- pgl z
					backifnull (errorOrAhead (lift . lift . f) . stringOrNot) x
			Scelta t as -> do 	let bs = ("* fine programma *",undefined) : as
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
			
----------------------------------------------building patch application layer  -----------------------------------
type BuildingPatch r = ReaderT (Utente -> [String] -> (r,Log Utente)) (StateT  (Utente,PrivateKey,[String]) IO) 
runBuildingPatch :: (Show r,Read r, ParteDi Responsabili r) 
	=> [R]
	-> [Chiusura r (BuildingPatch r) -> [(String,MEvento r (BuildingPatch r) String)]]
	-> (Log Utente -> IO ()) 
	-> [Reazione r c Utente] -- le reazioni di base
	-> String -- lo stato serializzato
	-> IO (Utente,PrivateKey,[String])
runBuildingPatch bs cs ls rs s = let
	(r,_,_) = runIdentity $ runProgramma rs s (fst <$> get)
	provaEventi u xs = let (r,_,log) = runIdentity $ runProgramma rs s (caricaEventi bs (zip (repeat u) xs) >> (fst <$> get))
		in (r,log)
	in execStateT (runReaderT (action cs ls r) provaEventi) (undefined,undefined,[]) 


intercezione :: (Log Utente -> IO ()) -> Utente -> PrivateKey -> String -> BuildingPatch r r 
intercezione sl w prk x = do
	(_,_,xs) <- get 
	(r,ls) <- asks $ \f -> f w (x:xs) 
	put (w,prk,x:xs)
	liftIO (sl ls)
	return r

cercaChiave :: Utente -> BuildingPatch r (Maybe PrivateKey)
cercaChiave s = liftIO $ do
	ls <- getDirectoryContents "."
	case find ((==) $ s ++ ".priv") ls of
		Nothing -> liftIO (print ("file chiave privata di " ++ decodeString s ++ " non trovato")) >> return Nothing
		Just x -> Just . read <$> readFile x

action :: (ParteDi Responsabili r) 
	=>	[Chiusura r (BuildingPatch r) -> [(String,MEvento r (BuildingPatch r) String)]]
	->	(Log Utente -> IO ()) -- cosa fare con i log creati durante i vari caricamenti
	-> 	r -- lo stato di riferimento, sul quale appoggiare gli eventi
	-> 	BuildingPatch r ()
action cs sl r = svolgi (autenticazione cercaChiave >>= inserimentoEvento cs (intercezione sl)) r >>= runCostruzioneIO >> return ()



