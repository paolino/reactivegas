{-# LANGUAGE NoMonomorphismRestriction,FlexibleContexts,ScopedTypeVariables  #-}

-- | un gestore eventi per servizi istanziabili
module Eventi.Servizio (
	servizio0,	
	nuovoStatoServizio,
	osservaStatoServizio,
	modificaStatoServizio,
	eliminaStatoServizio,
	elencoSottoStati,
	Servizio
	)
	
	where

import qualified Data.ByteString.Lazy.Char8 as BL (foldr)
import Data.Char (ord)

import Control.Monad.State (get,lift)
import Control.Applicative ((<$>))
import Control.Arrow ((***))

import Core.Inserimento (MTInserzione, osserva, modifica, fallimento,logga)
import Core.Programmazione (Inserzione)
import Lib.Aspetti ((.<),ParteDi,see)
import Lib.Assocs (assente,(?),updateM,elimina)
import Lib.Firmabile (hash,hashOver)
import Lib.QInteger 

data (Read a,Show a,Eq a) => Servizio a = Servizio {sottostato :: [(QInteger,(String,a))]} deriving 
	(Show, Read,Eq)

servizio0 :: (Show a, Read a,Eq a) => Servizio a
servizio0 = Servizio []


-- | aggiunge una nuova istanza per il servizio di tipo a, integrando con una descrizione,  restituisce la chiave 
nuovoStatoServizio :: (ParteDi (Servizio a) s, Read a,Eq a, Show a, Integer `ParteDi` s) 
	=> a 
	-> (String,String) 
	-> MTInserzione s c d QInteger
nuovoStatoServizio s (u,q) = do
	Servizio ls <- osserva
	n <- osserva
	let p = makeQInteger $ (n + (BL.foldr (\x y -> 256 * y + fromIntegral (ord x)) 0 . hash $ u ++ q) `mod` 10 ^ 12)
	modifica $ \_ -> Servizio ((p,(q,s)):ls)
	logga $ "riferimento " ++ show p
	return p

-- | controlla la presenza di una chiave presso il servizio di tipo a, in caso di successo restituisce il servizio
servizioPresente :: (ParteDi (Servizio a) s, Read a,Eq a, Show a) 
	=> QInteger 
	-> MTInserzione s c d (Servizio a)
servizioPresente j = do
	s@(Servizio ls) <- osserva  
	fallimento (assente j ls) $ "richiesta di servizio fallita , indice " ++ show j ++ " non trovato"
	return s

-- | restituisce il valore del servizio di tipo a indicizzato dalla chiave passata 
-- osservaStatoServizio :: (ParteDi (Servizio a) s, Read a,Eq a, Show a) => Int ->  MTInserzione s c d a
osservaStatoServizio j = do 
	Servizio ls <- servizioPresente j 	
	snd <$> return (ls ? (j,error "osservaStatoServizio: the impossible happened"))

-- | modifica il valore del servizio di tipo a indicizzato dalla chiave passata
modificaStatoServizio :: (ParteDi (Servizio a) s, Read a,Eq a, Show a) 
	=> QInteger 
	-> (a ->  MTInserzione s c d a) 
	-> MTInserzione s c d ()

modificaStatoServizio j f = do
	Servizio ls <- servizioPresente j 
	ls' <- updateM j (\(q,a) -> (,) q <$> f a)  undefined ls
	modifica $ \_ -> Servizio ls'

-- | elimina lo stato di servizio di tipo a alla chiave passata, necessita di un valore di tipo a inutile
eliminaStatoServizio :: forall a s c d. (ParteDi (Servizio a) s,
                        Show a,Read a,Eq a
                        ) =>
                       QInteger -> a -> MTInserzione s c d ()
eliminaStatoServizio j proxy = do
	Servizio ls <- servizioPresente j :: MTInserzione s c d (Servizio a)
	modifica $ \_ -> Servizio (elimina j ls)

-- | restituisce la lista di associazione (chiave, descrizione) degli stati presenti
elencoSottoStati :: (ParteDi (Servizio a) s, Show a,Read a,Eq a) 
	=> s -> [(QInteger,(String,a))]
elencoSottoStati = sottostato . see



