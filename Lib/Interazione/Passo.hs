
{-# LANGUAGE ExistentialQuantification #-}

module Lib.Interazione.Passo where

import Control.Monad (join, forever, when)
import Control.Monad.Cont (callCC)
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
menu 	::  String 			-- ^ descrizione
	-> [(String,PDescription b a)] 	-- ^ menu a partire da un gestore di a
	-> PDescription b a		-- ^ il passo risultante
menu x = join . flip scelte x

rotonda :: ((a -> PDescription b c) -> PDescription b c) -> PDescription b a
rotonda f = callCC $ \k -> forever $ f k

mano :: String -> a -> [(String,PDescription b a)] -> PDescription b a
mano s z xs = rotonda $ \k -> menu s $ ("<uscita>",k z):xs

esempio :: PDescription Int Int
esempio = mano "esempio" 0 [
		("*2", (*2) `fmap` libero "numero"),
		("*", do
			x <- libero "addendo 1"
			y <- libero "addendo 2"
			return $ x * y)
		]
	
