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

import Control.Monad.Cont -- (forever, ContT (..) , runContT, callCC, join)
import Control.Monad.State
import Control.Arrow ((***), first)
import Control.Applicative ((<$>))
import Lib.Response (Response)
-- | i possibili sviluppi di una costruzione

data Passo m b 
	= forall a. Scelta String [(String,a)] (a -> m (HPasso m b)) 	-- ^ scelta vincolata ad una lista di possibilità
	| forall a. Read a => Libero String (a -> m (HPasso m b)) 		-- ^ scelta da leggere da una stringa
	| forall a. Read a => Upload String (a -> m (HPasso m b))
	| Output Response (m (HPasso m b)) 
	| Errore Response (m (HPasso m b)) 
	| forall a. Show a => Download String a (m (HPasso m b)) 
	| Costruito b							-- ^ valore calcolato

type HPasso m b = (Passo m b, [m (Passo m b)])

type Costruzione m b = StateT [m (Passo m b)] (ContT (HPasso m b) m)

wrap :: Monad m => ((a -> m (HPasso m b)) -> Passo m b ) ->  Costruzione m b a
wrap f  = StateT $ \ns -> ContT $ \k -> return $ let 
		c a = k (a, (fst `liftM` c a) : ns)
		in  (f c, ns)

back = modify (subtract 1)
-- | da una costruzione ad un passo che la esegue
svolgi :: Monad m => Costruzione m b b -> m (HPasso m b)
svolgi = flip runContT (return . first Costruito) . flip runStateT []



-- | produce un passo di valore Libero nella monade Cont
libero :: (Read a, Monad m) => String -> Costruzione m b a
libero prompt = wrap $ Libero prompt

output :: (Monad m) => Response -> Costruzione m b ()
output s = wrap $ (\c -> Output s  $ c ()) 

errore :: (Monad m) => Response -> Costruzione m b ()
errore s = wrap $ (\c -> Errore s $ c ())

upload prompt = wrap $ Upload prompt

download s x = wrap $ (\c -> Download s x $ c ())

-- | produce un passo di valore Scelta
scelte :: Monad m => [(String,a)] -> String -> Costruzione m b a 
scelte xs prompt = wrap $ (\c -> Scelta prompt xs c)


	
-- | presenta un menu di scelte operative
menu 	:: (Functor m , Monad m) => String -- ^ descrizione
		-> [(String,Costruzione m b a)] -- ^ menu a partire da un gestore di a
		-> Costruzione m b a	-- ^ il passo risultante
menu x = join . flip scelte x

rotonda f = callCC $ \k -> forever $ get >>= \c -> f k >> put c

mano s xs = rotonda (rmenu s xs)

rmenu s xs k = menu s $ ("esci",k ()):xs


