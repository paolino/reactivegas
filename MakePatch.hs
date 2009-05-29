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
import System.Directory
import Control.Monad.Error
import Codec.Crypto.RSA (PrivateKey)

----------------------------------- sezione input nuovo evento -----------------------------------------

type Patch = (Maybe (Utente,PrivateKey),[String]) 
type MakePatch r m a = Svolgimento r () m a 

type Prompt r m a = (String , MakePatch r m a)
type Recupera r m = String -> MakePatch r m ()
type Errante r m a = Recupera r m -> Prompt r m a

nodo :: Monad m => (String -> m ()) -> [Errante r m ()] -> MakePatch r m ()
nodo d cs = callCC $ forever . (callCC . dentro)  where
	dentro k ki = join . parametro . Scelta "scegli una strada" $ ("indietro", k ()): map ($ ki2) cs where
		ki2 x = lift (d x) >> ki ()


autenticazione :: (ParteDi Responsabili r, MonadState Patch m) => (m r, Utente -> m (Maybe PrivateKey)) -> Errante r m ()
autenticazione (qr,q) k = (,) "autenticazione" $  do
		rs <- map fst . responsabili <$> lift qr -- lo stato si trova nella reader
		p <- parametro $ Scelta "autore degli eventi" (map (id &&& id) rs)
		lift (q p) >>= maybe (k "errore nel caricamento della chiave privata") 
				(\n -> lift . modify . first $ const (Just (p,n)))

cancellaEvento :: MonadState Patch m => Errante r m ()
cancellaEvento k = (,) "elimina evento" $ do
		es <- snd <$> lift get	
		when (null es) $ k "non ci sono eventi da cancellare"
		s <- parametro $ Scelta "evento da cancellare" (map (id &&& id) es)
		lift . modify . second $ delete s 
		k "evento cancellato"

trattamentoEvento :: (MonadState Patch m) => (String -> m (), m r) -> [Errante r m String] -> Prompt r m ()
trattamentoEvento (d,q) cs = (,) "manipola eventi" . nodo d . map (fmap $ second under) $ cancellaEvento: map (fmap $ second (>>= z)) cs
	where 	z x = lift . modify . second $ (x:)
		under f = lift q >>= flip local f. const 

commit ::  MonadState Patch m => ((Utente,PrivateKey,[String]) -> m String) -> Errante r m ()
commit s k = (,) "invia eventi" $ do
	(uprk,es) <- lift get
	case uprk of
		Nothing -> k "bisogna ancora autenticarsi"
		Just (u,prk) -> case null es of
			True -> k "mi rifiuto di spedire una lista di eventi vuota"
			False -> lift (s (u,prk,es)) >>= k
	lift $ modify (second (const []))
	
