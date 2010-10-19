{-# LANGUAGE ExistentialQuantification, NoMonomorphismRestriction #-}


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
	| Output Response (Maybe (m (HPasso m b)))
	| Errore Response (Maybe (m (HPasso m b)))
	| forall a. Show a => Download String String a (m (HPasso m b)) 
	| forall a. Read a => Password String (a -> m (HPasso m b))
	| Costruito b							-- ^ valore calcolato

-- | Historied Passo. An HPasso gives a positive continuation along a set of runned continuations.
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

password :: (Read a, Monad m) => String -> Costruzione m b a
password prompt = wrap $ Password prompt

output :: (Monad m) => Bool -> Response -> Costruzione m b ()
output t s = wrap $ (\c -> Output s  $ if t then Just (c ()) else Nothing) 

errore :: (Monad m) => Bool -> Response -> Costruzione m b ()
errore t s = wrap $ (\c -> Errore s $ if t then Just (c ()) else Nothing)

upload prompt = wrap $ Upload prompt

download s f x = wrap $ (\c -> Download s f x $ c ())

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

rmenu s xs k = menu s $ ("<uscita>",k ()):xs


