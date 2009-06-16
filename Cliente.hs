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
import Control.Arrow
import Data.Maybe
import Control.Monad.Error
import Codec.Binary.UTF8.String
import System.Directory
import System.Random
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
import ClientLib
import Aspetti

----------------------------------- sezione input nuovo evento -----------------------------------------

type MakePatch m a = Svolgimento () m a 

type Prompt m a = (String , MakePatch m a)
type Recupera m = String -> MakePatch m ()
type Errante m a = Recupera m -> Prompt m a

nodo :: Monad m => (String -> m ()) -> [Errante m ()] -> MakePatch m ()
nodo d cs = callCC $ forever . (callCC . dentro)  where
	dentro k ki = join . parametro . Scelta "scegli una strada" $ ("nodo superiore", k ()): map ($ ki2) cs where
		ki2 x = lift (d x) >> ki ()


sincronizzazione :: Monad m => m (Either String String) -> Errante m () 
sincronizzazione f k = (,) "sincronizza il gruppo" $ lift f >>= either k (\s -> k ("sincronizzazione:" ++ s))

autenticazione :: (ParteDi Responsabili r, MonadState Patch m) => (m r, Utente -> m (Either String PrivateKey)) -> Errante m ()
autenticazione (qr,q) k = (,) "autenticazione" $  do
		rs <- map fst . responsabili <$> lift qr -- lo stato si trova nella reader
		p <- parametro $ Scelta "autore degli eventi" (map (id &&& id) rs)
		lift (q p) >>= either k (\n -> lift . modify . first $ const (Just (p,n)))

cancellaEvento :: MonadState Patch m => Errante m ()
cancellaEvento k = (,) "elimina evento" $ do
		es <- snd <$> lift get	
		when (null es) $ k "non ci sono eventi da cancellare"
		s <- parametro $ Scelta "evento da cancellare" (map (id &&& id) es)
		lift . modify . second $ delete s 
		k "evento cancellato"

trattamentoEvento :: forall m r . (MonadState Patch m, Show r) => (String -> m (), m r) -> [Recupera m -> (String , r -> MakePatch m String)] -> Prompt m ()
trattamentoEvento (d,q) cs = (,) "manipola eventi" $ do
	let 	wrap :: (Recupera m -> (String , r -> MakePatch m String)) -> (Errante m ())
		wrap f k = let 	(s,c) = f k 
				y = do	r <- lift q
					c r >>= z
					lift q >> return ()
				in (s,y) 
			
	nodo d  $ cancellaEvento: map wrap cs
	where 	z x = lift . modify . second $ (x:)



nuovechiavi :: Monad m => (Utente -> m (Either String ())) -> Errante m ()
nuovechiavi f k = (,) "crea nuove chiavi responsabile" $ do
	s <- parametro $ Libero "nome del responsabile delle nuove chiavi (attenzione!)"
	lift (f s) >>= either k (\_ -> k "nuove chiavi create")


main =	do
	print "lettura cartella ............"
	b <- throwLefts $ do 
		c <- tagga "lettura configurazione" $ catchFromIO (readFile "configurazione") >>= contentReads 
		s <- tagga "lettura file di stato" $ catchFromIO (readFile "stato" >>= \x -> length x `seq` return x ) >>= 
			contentReads >>= lift . atomically . newTVar
		puk <- tagga "lettura chiave pubblica sincronizzatore" $ catchFromIO (readFile "sincronizzatore.publ") >>= contentReads
		pa <- lift . atomically . newTVar =<< do 	t <- lift . runErrorT $ tagga "lettura patch" $ catchFromIO (readFile "patch") 
									>>= contentReads 
								either (\e -> liftIO (print e) >> return (Nothing, [])) return t
		return $ Board c puk s pa 
		
	hSetBuffering stdout NoBuffering 
	x <- runProgram b $ runErrorT $ svolgi interfaccia >>= runCostruzioneIO 
	case x of 
		Left s -> liftIO $ putStrLn "impossibile"
		Right r -> print "fine"
		
interfaccia :: (Functor m , MonadIO m, MonadReader Board m, MonadState Patch m) => MakePatch m ()

interfaccia = let c = statoCorrettoIO reattori priorities in 
	nodo (liftIO . logerrore) [
		nuovechiavi creaChiaviIO , 
		sincronizzazione sincronizzaIO , 
		\k -> ("aggiornamento dello stato" , lift $ aggiornamentoIO >>= either (liftIO . putStrLn) (liftIO . stampaLogs)), 
		\k -> ("spedizione patch", lift $ spedizionePatchIO >>= either (liftIO . putStrLn) return),
		autenticazione (fst <$> c, cercaChiaveIO),
		const $ trattamentoEvento (liftIO . logerrore, fst <$> c) makers
		]

logerrore  =  putStrLn .( ++ "****" ) . ("****" ++)

listingIO = runErrorT . tagga "listing della cartella" . catchFromIO $ do
	liftIO $ getDirectoryContents "."

runCostruzioneIO :: (MonadIO m) => Costruzione (ErrorT String m) t -> ErrorT String m (Maybe a)
	
runCostruzioneIO  c = flip runContT return . callCC $ \k -> 
	let zeta c@(Costruzione l f) = do 
		let riprova s  = nl >> msg s >> zeta c
		r <- runErrorT $ nl >> case l of
			Libero z -> do	(encodeString -> x) <- pgl z
					ls <- catchFromIO $ getDirectoryContents "."

					case x of 	"/" -> lift . zeta . Costruzione (Scelta "scegli un file" $ map (id &&& id) ls) $ \x -> do
									r <-  tagga "lettura dato da file" $
										catchFromIO (readFile x) >>= contentReads --guaio , bisogna gestire gli errori in f
									f r
							x -> 	backifnull (errorOrAhead joke . stringOrNot) x
			Scelta t as -> do 	let bs = ("fine",undefined) : as
						liftIO $ mapM_ putStrLn  [show n ++ ". " ++ decodeString a | (n,a) <- zip [0..] (map fst bs)]
						nl
						let 	q y = do 	when (y < 0 || y > length as) $ throwError "scelta impossibile"
									when (y == 0) $ lift (k Nothing)
									joke . snd $ (as !! (y - 1))	
						pgl t >>= backifnull (errorOrAhead q . toMaybe . reads)
		either riprova return r 
		where
			joke =  ErrorT . lift . lift . runErrorT . f  
			backifnull f x = if null x then return Nothing else f x
			errorOrAhead q =  
				maybe (throwError "errore di lettura") 
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
			

	
