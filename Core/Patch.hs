{-# LANGUAGE TypeOperators, ViewPatterns, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
module Core.Patch where

import Data.Maybe
import Control.Arrow
import System.Environment
import Data.List
import Control.Applicative ((<$>))
import System.IO
import Codec.Binary.UTF8.String
import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.Reader
import System.Directory
import Codec.Crypto.RSA
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as B

import Control.Monad (msum)

import Core.Types (Esterno,Evento,Message)
import Core.Inserimento ()
import Core.Controllo (SNodo)
import Core.Programmazione
import Core.Parsing
import Core.Contesto

import Lib.Aspetti (ParteDi)
import qualified Eventi.Anagrafe as Anagrafe


-- | Lo stato e' sempre accompagnato dalla struttura di eventi necessari a trasformarlo in attuale, quegli eventi che non potevano trovare riscontro nella serializzazione, detti eventi aperti
type Stato s = (s,[SNodo s Anagrafe.Utente])

-- | estrae i responsabili da uno Stato
responsabili :: (Anagrafe.Responsabili `ParteDi` s) => Stato s  -> [Anagrafe.Responsabile]
responsabili s = runReader (Anagrafe.responsabili (\x -> error $ "Patch: " ++ x)) . fst $ s

type Chiave = PublicKey
type Firma = B.ByteString
type Blob = String


-- | una patch è un insieme di eventi firmati da un responsabile
type Patch = (Chiave,Firma,[Evento])

firma :: Patch -> Firma
firma (_,f,_) = f

digest = showDigest . sha512 . B.pack . show

type Checker m = ReaderT ([Anagrafe.Responsabile],Blob) (ErrorT Message m)

runChecker :: ([Anagrafe.Responsabile],Blob) -> Checker m a -> m (Either Message a)
runChecker rs c = runErrorT $ runReaderT c rs  

liftMaybe :: MonadError Message m => Maybe a -> Message -> m a
liftMaybe Nothing s = throwError s
liftMaybe (Just x) _ = return x 

-- | controlla che una patch sia accettabile, ovvero che il responsabile sia presente e che la firma sia corretta
checkPatch :: Monad m => Patch -> Checker m ()
checkPatch (c,f,xs) = do
	(rs,b) <- ask
	when (not $ c `elem` map snd rs) $ throwError "l'autore della patch è sconosciuto"
	when (verify c (B.pack $ b ++ concat xs) f) $ throwError "la firma della patch è corrotta"

-- | costruisce una patch da un insieme di eventi
mkPatch :: [Evento] -> Utente -> Checker m Patch
mkPatch = undefined


eventiPatch :: Monad m => Patch -> Checker m [Esterno Anagrafe.Utente]
eventiPatch (c,_,xs) = do
	(rs,_) <- ask
	(n,_) <- liftMaybe (find ((==) c . snd) $ rs) $ "errore interno: lo stato ha una chiave senza nome"
	return $ map ((,) n) xs 
-- | una patch di gruppo è un insieme di patch firmate dal sincronizzatore
type Group = (Firma,[Patch])

-- | controlla l'integrità di una patch di gruppo
checkGroup :: Monad m => Chiave -> Group -> Checker m ()
checkGroup c (f,ps) = do 
	mapM checkPatch ps
	(_,b) <- ask
	when (not $ verify c (B.pack b `mappend` B.concat (map firma ps)) f) $ throwError
		 "la firma del sincronizzatore è corrotta" 

mkGroup :: Monad m => 
-- | estrae gli eventi, come eventi esterni assegnati al loro autore, da una patch di gruppo
eventi :: Monad m 
	=> [Anagrafe.Responsabile] 	-- ^ responsabili validi
	-> Blob 			-- ^ hash dello stato di riferimento
	-> Chiave 			-- ^ chiave pubblica del sincronizzatiore
	-> Group 			-- ^ patch di gruppo
	-> m (Either Message [Esterno Anagrafe.Utente])	-- ^ o errore o lista di eventi
eventi rs b c g = runChecker (rs,b) $ do
	checkGroup c g
	concat <$> mapM eventiPatch ps
	

