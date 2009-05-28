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

type Patch = ((Utente,PrivateKey),[String]) 
type MakePatch r m a = Svolgimento r () m a 

type Indietro r m = MakePatch r m ()

data Prompt r m a = Prompt String (MakePatch r m a)

type Errante r m a = (String -> MakePatch r m ()) -> Prompt r m a

mkPrompt (x,y) = Prompt x y 
fromPrompt (Prompt x y) = (x,y)

onPrompt f (Prompt x g) =  Prompt x (f g)

nodo :: Monad m => (String -> m ()) -> [Errante r m ()] -> MakePatch r m ()
nodo d cs = callCC $ forever . (callCC . dentro)  where
	dentro k ki = join . parametro . Scelta "scegli una strada" $ ("indietro", k ()): map (fromPrompt . ($ki2)) cs where
		ki2 x = lift (d x) >> ki ()

autenticazione :: (ParteDi Responsabili r, MonadState Patch m) =>
		(Utente -> m (Maybe PrivateKey))  -- un modo per ottenere la chiave privata da un nome utente
		-> Errante r m ()
autenticazione q k = Prompt "autenticazione" $  do
		rs <- map fst . responsabili <$> ask -- lo stato si trova nella reader
		p <- parametro $ Scelta "autore degli eventi" (map (id &&& id) rs)
		lift (q p) >>= maybe (k "errore nel caricamento della chiave privata") 
				(\n -> lift . modify . first $ const (p,n))
cancellaEvento :: MonadState Patch m => Errante r m ()
cancellaEvento k = Prompt "elimina evento" $ do
		es <- snd <$> lift get	
		when (null es) $ k "non ci sono eventi da cancellare"
		s <- parametro $ Scelta "evento da cancellare" (map (id &&& id) es)
		lift . modify . second $ delete s 
		k "evento cancellato"
trattamentoEvento :: (MonadState Patch m) => (String -> m (), m r) -> [Errante r m String] -> Prompt r m ()
trattamentoEvento (d,q) cs = Prompt "Eventi" . nodo d . map (fmap $ onPrompt under) $ cancellaEvento: map (fmap $ onPrompt (>>= z)) cs
	where 	z x = lift (modify . second $ (x:)) 
		under f = lift q >>= flip local f. const 


base :: (ParteDi Responsabili r, MonadState Patch m) =>  (String -> m (), m r, Utente -> m (Maybe PrivateKey)) -> [Errante r m String] -> MakePatch r m ()
base  (print,state,privatekey)  cs = nodo print [autenticazione privatekey ,const $ trattamentoEvento  (print, state) cs] 

------------------------------------------------------------------------------------
cercaChiave s = do
	ls <- getDirectoryContents "."
	case find ((==) $ s ++ ".priv") ls of
		Nothing -> print ("file chiave privata di " ++ decodeString s ++ " non trovato") >> return Nothing
		Just x -> Just . read <$> readFile x
------------------------------------building patch application layer  -----------------------------------

correggiStato :: (Show r, Read r, MonadState Patch m)
	=> ([R],[Reazione r c Utente]) 
	-> (Log Utente -> m ()) 
	-> String -- lo stato serializzato
	-> m r
correggiStato (bs,rs) pl s  = do
	((u,pk),xs) <- get 
	let (r,_,log) = runIdentity $ runProgramma rs s (caricaEventi bs (zip (repeat u) xs) >> (fst <$> get))
	pl log
	return r

azione :: (MonadIO m, Show r, Read r, MonadState Patch m, ParteDi Responsabili r) =>
          ([R], [Reazione r c Utente])
          -> (Log Utente -> m ())
          -> String
          -> [(String -> MakePatch r m ()) -> (String, MakePatch r m String)]
          -> m ()

azione q pl s (map (fmap mkPrompt) -> cs) = svolgi (base (liftIO . putStrLn, correggiStato q pl s, liftIO . cercaChiave) cs) undefined >>= runCostruzioneIO >> return ()

makePatch  q pl s cs = do
	((u,pk),es) <- execStateT (azione q pl s cs) ((undefined, undefined),[])

