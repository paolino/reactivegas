{-# LANGUAGE ExistentialQuantification #-}

{- | 
Un modulo per la costruzione controllata di valori.

I passi possibili per la creazione di valori sono limitati al datatype Passo.

Il valore Cont b c , che ha come parametro una funzione (a -> c) si presta a racchiudere i dati di tipo (Passo c). Infatti i "passi" rilevanti prendono come ultimo parametro delle funzioni (a -> Passo b) che rappresentano il passo successivo costruito dal valore di uscita del passo attuale.

Essendo (Cont (Passo b)) una monade, la scrittura monadica ci permette un linguaggio sequenziale di costruzione dei valori.

Nell'esempio la funzione interazione descrive la costruzione di un intero attraverso l'inserimento sequenziale di 2 interi e la selezione dell'operazione binaria da eseguire sui 2 numeri.

interazioneb :: Cont (Passo Int) Int
interazioneb = do
	x <- libero "primo:" 
	y <- libero "secondo:" 
	z <- scelte [("somma",(+)),("differenza",(-))] "operazione:" 
	return $ z x y

interazioneu :: Cont (Passo Int) Int
interazioneu k = do

La funzione svolgi passa dal linguaggio monadico alla struttura Passo Int, svolgendo la monade.

costruzione :: Passo Int
costruzione = svolgi interazione

I dati di tipo (Passo b) vanno sucessivamente percorsi dai driver adatti all'interazione.

-}
module Lib.Costruzione where

import Control.Monad.Cont (Cont (..) , runCont, callCC, join)

-- | i possibili sviluppi di una costruzione
data Passo b 
	= forall a. Scelta String [(String,a)] (a -> Passo b) 	-- ^ scelta vincolata ad una lista di possibilità
	| forall a. Read a => Libero String (a -> Passo b)	-- ^ scelta da leggere da una stringa
	| forall a. Read a => DaFile String (a -> Passo b)
	| Costruito b						-- ^ valore calcolato

-- | I tipo di ritorno della descrizione dei passi
type Descrizione b = Cont (Passo b) 

-- | inserisce un passo di valore Libero, si intende che il valore a sia fornito come show di a
libero :: (Read a) 	=> String  -- ^ significato
			-> Descrizione b a -- ^ continuazione di un passo Libero
libero prompt = Cont $ Libero prompt

-- | inserisce un passo di valore DaFile, si intende che il valore a sia il nome del file contenente lo show di a
dafile :: (Read a) 	=> String 
			-> Descrizione b a
dafile prompt = Cont $ DaFile prompt

-- | inserisce un passo di valore Scelta, si intende che il valore a scelto corrisponsa alla String ricevuta ,che deve essere una delle chiavi della lista
scelte :: 	[(String,a)] 	-- ^ lista associativa tra una chiave, e il valore corrispondente
		-> String 	-- ^ significato
		-> Descrizione b a	-- ^ continuazione di un passo Scelta
scelte xs prompt = Cont $ Scelta prompt xs

-- | Eliminazione della descrizione. Viene costruito il (Passo b) 
svolgi :: Descrizione b b -> Passo b
svolgi = flip runCont Costruito 

-- | Una libreria per montare i Passo. Incrocio e' una funzione che avendo ricevuto la continuazione per un valore di tipo b, fornisce un insieme di Passo (descritti)	che possono essere eseguiti prima della continuazione fornita.
type Incrocio b d a  = 	(d -> Descrizione b a) -> [(String,Descrizione b a)]
		
-- | presenta un menu di scelte operative
incrocio 	:: String -- ^ descrizione
		-> Incrocio b d a -- ^ menu a partire da un gestore di a
		-> Descrizione b d	-- ^ il passo risultante
incrocio x q = callCC $ \k -> do
		join . scelte (q k) $ x
		error "Lib.Costruzione.menu: the impossible happened"



