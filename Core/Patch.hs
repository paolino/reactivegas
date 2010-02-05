{-# LANGUAGE  TypeSynonymInstances, FlexibleContexts #-}
module Core.Patch -- (Patch, fromPatch, mkPatch, Group, fromGroup, mkGroup, Checker, runChecker, Ambiente (..)) where
	where

import Data.List (find, lookup)
import Data.Maybe (fromJust)
import Control.Applicative ((<$>))
import Control.Monad.Error (when, ErrorT, throwError, runErrorT)
import Control.Monad.Reader (ReaderT, ask, runReaderT,asks)
import Data.Monoid (mappend)

import Core.Types (Esterno,Evento,Message)
import Core.Costruzione (Supporto,libero,scelte)
import Lib.Aspetti (ParteDi)
import Lib.Firmabile (sign , verify, Firma, Chiave, Segreto, Password)

import Eventi.Anagrafe (Responsabili,Utente,costrResponsabili,responsabili)
import Eventi.Sincronizzatore (sincronizzatore,Sincronizzatore)


-- | una patch è un insieme di eventi firmati da un responsabile
type Patch = (Chiave,Firma,[Evento])

-- | controlla che una patch sia accettabile, ovvero che il responsabile sia presente e che la firma sia corretta
-- fromPatch :: (Responsabili `ParteDi` s, Show s, Monad m) => Patch -> Supporto m s c [Esterno Utente]
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

selezionaAutore :: (Responsabili `ParteDi` s, Monad m) => Supporto m s c Utente
selezionaAutore = do 
	(rs,_) <- asks responsabili 
	(u,_) <- scelte (zip (map fst rs) rs) "seleziona l'autore della patch"
	return u

-- | una patch di gruppo è un insieme di patch firmate dal sincronizzatore
type Group = (Firma,[Patch])


-- | controlla l'integrità di una patch di gruppo
-- fromGroup :: (Sincronizzatore `ParteDi` s, Responsabili `ParteDi` s, Show s, Monad m ) => Group -> Supporto m s c [Esterno Utente]
fromGroup (f,ps) = do 
	s <- ask
	(_,(c,_)) <- asks sincronizzatore
	when  (not $ verify c (ps,s) f) $ throwError "la firma del sincronizzatore è corrotta" 
	concat <$> mapM fromPatch ps
	

-- | costruisce una patch di gruppo da un insieme di patch responsabile
mkGroup :: (Show s, Monad m, Sincronizzatore `ParteDi` s) => [Patch] -> Supporto m s c Group
mkGroup ps = do
	b <- ask
	(_,(c,s)) <- asks sincronizzatore
	p <- libero "password di sincronizzatore"
	case  sign (s,p) (ps,b) of 
		Nothing -> throwError $ "password errata"
		Just f -> return (f,ps)
	
--------------------------------------------------------------


