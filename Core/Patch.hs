{-# LANGUAGE  TypeSynonymInstances, ExistentialQuantification, FlexibleContexts, 
	ScopedTypeVariables, Rank2Types,  NoMonomorphismRestriction, StandaloneDeriving #-}
module Core.Patch -- (Patch, fromPatch, mkPatch, Group, fromGroup, mkGroup, Checker, runChecker, Ambiente (..)) where
	where

import Data.List (find, lookup)
import Data.Maybe (fromJust)
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Monad.Error (MonadError , when, throwError)
import Control.Monad.Reader (MonadReader,ask, asks)
import Data.Monoid (mappend)
import Debug.Trace

import Core.Types (Esterno,Evento,Message,Utente,Responsabile)
import Core.Costruzione (Supporto,libero,scelte,password)
import Lib.Firmabile (sign , verify, Firma, Chiave, Segreto, Password)
import Lib.States


-- | una patch è un insieme di eventi firmati da un responsabile
type Patch = (Chiave,Firma,[Evento])
firma :: Patch -> Firma
firma (_,x, _) =  x
-- | controlla che una patch sia accettabile, ovvero che il responsabile sia presente e che la firma sia corretta
fromPatch :: (Show s, Transition s, MonadReader s m, MonadError String m) => (s -> [Responsabile]) -> Patch -> m [Esterno Utente]
fromPatch grs (c,f,xs) =  do
	s <- ask
	let rs = grs s
	when (not $ c `elem` map (fst . snd) rs) $ throwError "l'autore della patch è sconosciuto"
	when (not $ tryShowF (\s -> verify c (xs,s) f) (ToPast s)) $ throwError "la firma della patch utente è corrotta"
	let u = fst. head . filter ((==c) . fst . snd) $ rs
	return $ zip (repeat u) xs

-- | costruisce una patch da un insieme di eventi

-- | una patch di gruppo è un insieme di patch firmate da uno dei responsabili
type Group = (Chiave,Firma,[Patch])


-- | restituisce gli eventi estratti dalla patch di gruppo, insieme al nome del responsabile che la ha firmata
fromGroup :: (Show s, Transition s, MonadReader s m , MonadError String m, Functor m) => (s -> [Responsabile]) -> Group -> m  (Utente,[Esterno Utente])
fromGroup grs (c,f,ps) = do 
	s <- ask
	let rs = grs s
	when (not $ c `elem` map (fst . snd) rs) $ throwError "l'autore dell'aggiornamento di gruppo è sconosciuto"
	when  (not $ tryShowF (\s -> verify c (ps,s) f) (ToPast s)) $ throwError "la firma del responsabile dell'aggiornamento di gruppo è corrotta" 
	let u = fst. head . filter ((==c) . fst . snd) $ rs
	(,) u <$> concat <$> mapM (fromPatch grs) ps
	
-- newtype SignerBox = SignerBox 
-- | costruisce una patch di gruppo da un insieme di patch responsabile


firmante :: forall m s c . (Transition s, Show s, Monad m) => Responsabile -> Supporto m s c (Firmante s)
firmante r@(u,(c,s)) = do  
	p <- password  $ u ++ ",la tua password di responsabile:"
	when (null p) $ throwError "password errata"
	case  sign (s,p) (undefined :: (),undefined :: s) of 
		Nothing -> throwError $ "password errata"
		Just _ -> return $ Firmante $ \b ps -> (c,fromJust $ sign (s,p) (ps,b),ps)

--------------------------------------------------------------

newtype Firmante b = Firmante (forall a. Show a => b -> [a] -> (Chiave, Firma,[a]))
