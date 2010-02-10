{-# LANGUAGE  TypeSynonymInstances, FlexibleContexts #-}
module Core.Patch -- (Patch, fromPatch, mkPatch, Group, fromGroup, mkGroup, Checker, runChecker, Ambiente (..)) where
	where

import Data.List (find, lookup)
import Data.Maybe (fromJust)
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Monad.Error (MonadError , when, throwError)
import Control.Monad.Reader (MonadReader,ask, asks)
import Data.Monoid (mappend)

import Core.Types (Esterno,Evento,Message)
import Core.Costruzione (Supporto,libero,scelte)
import Lib.Aspetti (ParteDi)
import Lib.Firmabile (sign , verify, Firma, Chiave, Segreto, Password)

import Eventi.Anagrafe (Responsabili,Utente,costrResponsabili,responsabili)


-- | una patch è un insieme di eventi firmati da un responsabile
type Patch = (Chiave,Firma,[Evento])

-- | controlla che una patch sia accettabile, ovvero che il responsabile sia presente e che la firma sia corretta
fromPatch :: (Responsabili `ParteDi` s, Show s, MonadReader s m, MonadError String m) => Patch -> m [Esterno Utente]
fromPatch (c,f,xs) =  do
	(rs,_) <- asks $ responsabili 
	s <- ask
	when (not $ c `elem` map (fst . snd) rs) $ throwError "l'autore della patch è sconosciuto"
	when (not $ verify c (xs,s) f) $ throwError "la firma della patch è corrotta"
	let u = fst. head . filter ((==c) . fst . snd) $ rs
	return $ zip (repeat u) xs

-- | costruisce una patch da un insieme di eventi
mkPatch :: (Responsabili `ParteDi` s, Show s, Monad m ) => Utente -> [Evento] -> Supporto m s c Patch
mkPatch u xs  = do
	(rs,_) <- asks responsabili 
	when (not $ u `elem` map fst rs) $ throwError "il tuo nome non risulta tra i responsabili"
	let (c,s) = fromJust . lookup u $ rs 
	p <- libero "la tua password di responsabile" 
	b <- ask
	case sign (s,p) (xs,b) of 
		Nothing -> throwError $ "password errata"
		Just f -> return (c, f , xs)

-- | una patch di gruppo è un insieme di patch firmate da uno dei responsabili
type Group = (Chiave,Firma,[Patch])


-- | restituisce gli eventi estratti dalla patch di gruppo, insieme al nome del responsabile che la ha firmata
fromGroup :: (Responsabili `ParteDi` s, Show s, MonadReader s m , MonadError String m, Functor m) => Group -> m  (Utente,[Esterno Utente])
fromGroup (c,f,ps) = do 
	s <- ask
	(rs,_) <- asks responsabili 
	when (not $ c `elem` map (fst . snd) rs) $ throwError "l'autore dell'aggiornamento di gruppo è sconosciuto"
	when  (not $ verify c (ps,s) f) $ throwError "la firma del responsabile dell'aggiornamento di gruppo è corrotta" 
	let u = fst. head . filter ((==c) . fst . snd) $ rs
	(,) u <$> concat <$> mapM fromPatch ps
	

-- | costruisce una patch di gruppo da un insieme di patch responsabile
mkGroup :: (Show s, Monad m, Responsabili `ParteDi` s) => Utente -> Supporto m s c ([Patch] -> Group)
mkGroup u = do
	b <- ask
	(rs,_) <- asks responsabili 	
	when (not $ u `elem` map fst rs) $ throwError "il tuo nome non risulta tra i responsabili"
	let (c,s) = fromJust . lookup u $ rs 
	p <- libero "la tua password di responsabile"
	case  sign (s,p) ([]::[Patch],b) of 
		Nothing -> throwError $ "password errata"
		Just _ -> return (\ps -> (c,fromJust $ sign (s,p) (ps,b),ps))
	
--------------------------------------------------------------


