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

import Control.Applicative ((<$>))
import Control.Arrow ((***))

import Core.Inserimento (MTInserzione, osserva, modifica, fallimento)
import Core.Programmazione (Inserzione)
import Lib.Aspetti ((.<),ParteDi,see)
import Lib.Assocs (assente,(?),updateM,elimina)

data (Read a,Show a) => Servizio a = Servizio {contatore :: Int, sottostato :: [(Int,(String,a))]} deriving 
	(Show, Read)

servizio0 :: (Show a, Read a) => Servizio a
servizio0 = Servizio 0 []


-- | aggiunge una nuova istanza per il servizio di tipo a, integrando con una descrizione,  restituisce la chiave 
nuovoStatoServizio :: (ParteDi (Servizio a) s, Read a, Show a) 
	=> a 
	-> String 
	-> MTInserzione s c d Int
nuovoStatoServizio s q = do
	Servizio p ls <- osserva
	modifica $ \_ -> Servizio (p + 1) ((p,(q,s)):ls)
	return p

-- | controlla la presenza di una chiave presso il servizio di tipo a, in caso di successo restituisce il servizio
servizioPresente :: (ParteDi (Servizio a) s, Read a, Show a) 
	=> Int 
	-> MTInserzione s c d (Servizio a)
servizioPresente j = do
	s@(Servizio p ls) <- osserva  
	fallimento (assente j ls) $ "richiesta di servizio fallita , indice " ++ show j ++ " non trovato"
	return s

-- | restituisce il valore del servizio di tipo a indicizzato dalla chiave passata 
osservaStatoServizio :: (ParteDi (Servizio a) s, Read a, Show a) => Int ->  MTInserzione s c d a
osservaStatoServizio j = do 
	Servizio p ls <- servizioPresente j 	
	snd <$> return (ls ? (j,error "osservaStatoServizio: the impossible happened"))

-- | modifica il valore del servizio di tipo a indicizzato dalla chiave passata
modificaStatoServizio :: (ParteDi (Servizio a) s, Read a, Show a) 
	=> Int 
	-> (a ->  MTInserzione s c d a) 
	-> MTInserzione s c d ()

modificaStatoServizio j f = do
	Servizio p ls <- servizioPresente j 
	ls' <- updateM j (\(q,a) -> (,) q <$> f a)  undefined ls
	modifica $ \_ -> Servizio p ls'

-- | elimina lo stato di servizio di tipo a alla chiave passata, necessita di un valore di tipo a inutile
eliminaStatoServizio :: forall a s c d. (ParteDi (Servizio a) s,
                        Show a,Read a
                        ) =>
                       Int -> a -> MTInserzione s c d ()
eliminaStatoServizio j proxy = do
	Servizio p ls <- servizioPresente j :: MTInserzione s c d (Servizio a)
	modifica $ \_ -> Servizio p (elimina j ls)

-- | restituisce la lista di associazione (chiave, descrizione) degli stati presenti
elencoSottoStati :: (ParteDi (Servizio a) s, Show a,Read a) 
	=> s -> [(Int,(String,a))]
elencoSottoStati = sottostato . see


