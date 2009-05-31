{-# LANGUAGE TypeOperators, ViewPatterns, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
module MakePatch where

import Core 
import Serializzazione
import Controllo
import Costruzione

import Aspetti (ParteDi) 
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
import Debug.Trace
import System.Directory
import Control.Monad.Error
import Codec.Crypto.RSA (PrivateKey)

----------------------------------- sezione input nuovo evento -----------------------------------------

type Patch = (Maybe (Utente,PrivateKey),[String]) 
type MakePatch m a = Svolgimento () m a 

type Prompt m a = (String , MakePatch m a)
type Recupera m = String -> MakePatch m ()
type Errante m a = Recupera m -> Prompt m a

nodo :: Monad m => (String -> m ()) -> [Errante m ()] -> MakePatch m ()
nodo d cs = callCC $ forever . (callCC . dentro)  where
	dentro k ki = join . parametro . Scelta "scegli una strada" $ ("indietro", k ()): map ($ ki2) cs where
		ki2 x = lift (d x) >> ki ()


autenticazione :: (ParteDi Responsabili r, MonadState Patch m) => (m r, Utente -> m (Maybe PrivateKey)) -> Errante m ()
autenticazione (qr,q) k = (,) "autenticazione" $  do
		rs <- map fst . responsabili <$> lift qr -- lo stato si trova nella reader
		p <- parametro $ Scelta "autore degli eventi" (map (id &&& id) rs)
		lift (q p) >>= maybe (k "errore nel caricamento della chiave privata") 
				(\n -> lift . modify . first $ const (Just (p,n)))

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



commit ::  (MonadState Patch m) => ((Utente,PrivateKey,[String]) -> m String) -> Errante m ()
commit s k = (,) "invia eventi" $ do
	(uprk,es) <- lift get
	case uprk of
		Nothing -> k "bisogna ancora autenticarsi"
		Just (u,prk) -> case null es of
			True -> k "mi rifiuto di spedire una lista di eventi vuota"
			False -> lift (s (u,prk,es)) >>= k
	lift $ modify (second (const []))


