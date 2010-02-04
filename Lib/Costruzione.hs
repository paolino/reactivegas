{-# LANGUAGE ExistentialQuantification #-}
-- | modulo per la creazione di valori dove i passi sono esplicitati attraverso le continuazioni
-- esempio
-- f :: Passo Int
-- f = svolgi $ do
-- 	x <- libero "primo:" 
-- 	y <- libero "secondo:" 
-- 	z <- scelte [("somma",(+)),("differenza",(-))] "operazione:" 
-- 	return $ z x y
-- le funzioni che trasformano il passo in un'azione reale sono fornite altrove

module Lib.Costruzione where

import Control.Monad.Cont (Cont (..) , runCont, callCC, join)

-- | i possibili sviluppi di una costruzione
data Passo b 
	= forall a. Scelta String [(String,a)] (a -> Passo b) 	-- ^ scelta vincolata ad una lista di possibilità
	| forall a. Read a => Libero String (a -> Passo b)	-- ^ scelta da leggere da una stringa
	| forall a. Read a => DaFile String (a -> Passo b)
	| Costruito b						-- ^ valore calcolato

-- | produce un passo di valore Libero nella monade Cont
libero :: (Read a) => String -> Cont (Passo b) a
libero prompt = Cont $ Libero prompt

dafile prompt = Cont $ DaFile prompt
-- | produce un passo di valore Scelta
scelte :: [(String,a)] -> String -> Cont (Passo b) a
scelte xs prompt = Cont $ Scelta prompt xs

-- | costruzione di un valore b 
type Costruzione b = Cont (Passo b) 

-- | da una costruzione ad un passo che la esegue
svolgi :: Costruzione b b -> Passo b
svolgi = flip runCont Costruito 
	
		
-- | presenta un menu di scelte operative
incrocio 	:: String -- ^ descrizione
		-> Incrocio b a -- ^ menu a partire da un gestore di a
		-> Passo b	-- ^ il passo risultante
incrocio x q = svolgi . callCC $ \k -> do
		join . scelte (q k) $ x
		error "Lib.Costruzione.menu: the impossible happened"

type Incrocio b a  = (b -> Costruzione b a) -> [(String,Costruzione b a)]


