
{-# LANGUAGE ExistentialQuantification #-}
-- | Un semplice linguaggio descrittivo di una interazione. Non prevedendo la monade interna il modulo risulta meramente didattico.

module Lib.Interazione.Passo where

import Control.Monad.Cont
import Lib.Interazione
import Lib.Response

data Passo b 
	= forall a. Scelta String [(String,a)] (Passo b) (a -> Passo b) 	-- ^ scelta vincolata ad una lista di possibilità
	| forall a. Read a => Libero String (Passo b) (a -> Passo b) 		-- ^ scelta da leggere da una stringa
	| forall a. Read a => Upload String (Passo b) (a -> Passo b)
	| Output Response (Passo b) (Passo b) 
	| Errore Response (Passo b) (Passo b) 
	| forall a. Show a => Download String a (Passo b) (Passo b) 
	| forall a. Read a => Password String (Passo b) (a -> Passo b)
	| Costruito b							-- ^ valore calcolato

type PDescription b = Description (Passo b)

-- | produce un passo di valore Libero nella monade Cont
libero :: Read a => String -> PDescription b a
libero prompt = mkDescription $ Libero prompt

password :: Read a => String -> PDescription b a
password prompt = mkDescription $ Password prompt

voidCall f d k = f d $ k ()

output :: Response -> PDescription b ()
output s = mkDescription $ voidCall (Output s) 

errore :: Response -> PDescription b ()
errore s = mkDescription $ voidCall (Errore s)

upload prompt = mkDescription $ Upload prompt

download s x = mkDescription $ voidCall (Download s x)

-- | produce un passo di valore Scelta
scelte :: [(String,a)] -> String -> PDescription b a 
scelte xs prompt = mkDescription $ Scelta prompt xs 


	
-- | presenta un menu di scelte operative
menu 		:: String -- ^ descrizione
		-> [(String,PDescription b a)] -- ^ menu a partire da un gestore di a
		-> PDescription b a	-- ^ il passo risultante
menu x = join . flip scelte x

evalPDescription x = evalDescription (Costruito x) Costruito
--------------------- esempio -----------------
esempio :: PDescription a Double
esempio = let 
	r1 = (*) `fmap` libero "fattore"
	r2 = (+) `fmap` libero "addendo"
	r3 = (flip (/)) `fmap` libero "divisore"
	r4 = (flip subtract) `fmap` libero "sottraendo"
	in do 
		x <- libero "operando"
		f <- menu "operazione" [
			("moliplicazione",r1),
			("addizione",r2),
			("divisione",r3),
			("sottrazione",r4)
			]
		r <- password "indovina"
		return $ f x - r


