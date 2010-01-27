{-# LANGUAGE  TypeSynonymInstances, FlexibleInstances #-}
module Core.Patch (Patch, fromPatch, mkPatch, Group, fromGroup, mkGroup, Checker, runChecker, Ambiente (..)) where

import Data.List (find, lookup)
import Control.Applicative ((<$>))
import Control.Monad.Error (when, ErrorT, throwError, runErrorT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Monoid (mappend)

import Core.Types (Esterno,Evento,Message)

import Lib.Aspetti (ParteDi)
import Lib.Error (liftMaybe, liftBool)
import Lib.Firmabile (Firmabile (..), sign , verify, Firma, Chiave, Segreto)

import Eventi.Anagrafe (Responsabile,Utente)
import Data.ByteString.Lazy.Char8 (pack)

-- | l'ambiente immutabile dove eseguire le operazioni di patch
data Ambiente = Ambiente {
	sincronizzatore :: Chiave,	-- ^ chiave pubblica di sincronizzazione
	responsabili :: [Responsabile],	-- ^ responsabili validi
	blob :: String 			-- ^ hash di riferimento per la patch
	}

-- | la monade di esecuzione delle operazioni di patch
type Checker m = ReaderT Ambiente (ErrorT Message m)

-- | esegue una azione di tipo checker nel suo ambiente
runChecker :: Ambiente -> Checker m a -> m (Either Message a)
runChecker rs c = runErrorT $ runReaderT c rs  

-- | una patch è un insieme di eventi firmati da un responsabile
type Patch = (Chiave,Firma,[Evento])

instance Firmabile ([Evento],String) where
	hash (xs,b) = pack $ b ++ concat xs

-- | controlla che una patch sia accettabile, ovvero che il responsabile sia presente e che la firma sia corretta
checkPatch :: Monad m => Patch -> Checker m ()
checkPatch (c,f,xs) = do
	Ambiente _ rs b <- ask
	liftBool (c `elem` map snd rs) "l'autore della patch è sconosciuto"
	liftBool (verify c (xs,b) f) "la firma della patch è corrotta"

-- | costruisce una patch da un insieme di eventi
mkPatch :: Monad m => [Evento] -> (Utente, Segreto) -> Checker m Patch
mkPatch xs (u,s) = do
	Ambiente _ rs b <- ask
	c <- liftMaybe (lookup  u rs) "l'utente non è registrato tra i responsabili"
	return (c, sign s (xs,b), xs)

-- | estrae gli eventi da una patch giudicandone l'integrita'
fromPatch :: Monad m => Patch -> Checker m [Esterno Utente]
fromPatch p@(c,_,xs) = do
	checkPatch p
	Ambiente _ rs _ <- ask
	(n,_) <- liftMaybe (find ((==) c . snd) $ rs) $ "la patch proviene da un responsabile sconosciuto"
	return $ map ((,) n) xs 

-- | una patch di gruppo è un insieme di patch firmate dal sincronizzatore
type Group = (Firma,[Patch])

instance Firmabile ([Patch],String) where
	hash (ps,b) = foldl mappend (pack b) $ map firma ps where
		firma (_,f,_) = f

-- | controlla l'integrità di una patch di gruppo
checkGroup :: Monad m => Group -> Checker m ()
checkGroup (f,ps) = do 
	Ambiente c _ b  <- ask
	liftBool (verify c (ps,b) f) "la firma del sincronizzatore è corrotta" 

-- | costruisce una patch di gruppo da un insieme di patch responsabile
mkGroup :: Monad m => [Patch] -> Segreto -> Checker m Group
mkGroup ps s = do
	Ambiente _ _ b  <- ask
	return (sign s (ps,b),ps)
	
-- | estrae gli eventi, come eventi esterni assegnati al loro autore, da una patch di gruppo
fromGroup :: Monad m 
	=> Group 			-- ^ patch di gruppo
	-> Checker m [Esterno Utente]	-- ^ o errore o lista di eventi
fromGroup g@(_,ps) = do
	checkGroup g
	concat <$> mapM fromPatch ps
	

