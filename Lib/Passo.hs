{-# LANGUAGE ExistentialQuantification, NoMonomorphismRestriction #-}
-- | modulo per la creazione di valori dove i passi sono esplicitati attraverso le continuazioni
-- esempio
-- f :: Passo Int
-- f = svolgi $ do
-- 	x <- libero "primo:" 
-- 	y <- libero "secondo:" 
-- 	z <- scelte [("somma",(+)),("differenza",(-))] "operazione:" 
-- 	return $ z x y
-- le funzioni che trasformano il passo in un'azione reale sono fornite altrove

module Lib.Passo where

import Control.Monad.Cont (forever, ContT (..) , runContT, callCC, join)
import Lib.Response (Response)
-- | i possibili sviluppi di una costruzione
data Passo m b 
	= forall a. Scelta String [(String,a)] (a -> m (Passo m b)) 	-- ^ scelta vincolata ad una lista di possibilità
	| forall a. Read a => Libero String (a -> m (Passo m b))	-- ^ scelta da leggere da una stringa
	| forall a. Read a => Upload String (a -> m (Passo m b))
	| Output Response (m (Passo m b)) 
	| Errore Response (m (Passo m b)) 
	| forall a. Show a => Download String a (m (Passo m b)) 
	| Costruito b							-- ^ valore calcolato

-- | produce un passo di valore Libero nella monade Cont
libero :: (Read a, Monad m) => String -> Costruzione m b a
libero prompt = ContT $ return . Libero prompt

output :: Monad m => Response -> Costruzione m b ()
output s = ContT $ return . Output s . ($())

errore :: Monad m => Response -> Costruzione m b ()
errore s = ContT $ return . Errore s . ($())

upload prompt = ContT $ return . Upload prompt

download s x = ContT $ return . Download s x . ($())
-- | produce un passo di valore Scelta
scelte :: Monad m => [(String,a)] -> String -> Costruzione m b a 
scelte xs prompt = ContT $ return . Scelta prompt xs

-- | costruzione di un valore b 
type Costruzione m b = ContT (Passo m b) m

-- | da una costruzione ad un passo che la esegue
svolgi :: Monad m => Costruzione m b b -> m (Passo m b)
svolgi = flip runContT $ return . Costruito 
	
-- | presenta un menu di scelte operative
menu 	:: Monad m => String -- ^ descrizione
		-> [(String,Costruzione m b a)] -- ^ menu a partire da un gestore di a
		-> Costruzione m b a	-- ^ il passo risultante
menu x = join . flip scelte x

rotonda f = callCC $ \k -> forever (f k)

rmenu k s xs = menu s $ ("esci",k ()):xs

