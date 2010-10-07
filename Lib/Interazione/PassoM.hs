
{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}
-- | La versione monadica del modulo Passo. La monade interna rappresenta gli effetti dell'interazione. Gli effetti non sono reversibili tramite l'undo e quindi ogni lift è da considerarsi un commit all'esterno. La presenza della monade interna ci permette di vivere nell'interazione 
module Lib.Interazione.PassoM where

import Control.Monad (join, forever, when)
import Control.Monad.Cont (callCC)
import Control.Monad.State (MonadState,modify,lift)
import Lib.Interazione
import Lib.Response

data PassoM m b 
	= forall a. Scelta String [(String,a)] (m (PassoM m b)) (a -> m (PassoM m b)) 	-- ^ scelta vincolata ad una lista di possibilità
	| forall a. Read a => Libero String (m (PassoM m b)) (a -> m (PassoM m b)) 		-- ^ scelta da leggere da una stringa
	| forall a. Read a => Upload String (m (PassoM m b)) (a -> m (PassoM m b))
	| Output Response (m (PassoM m b)) (m (PassoM m b)) 
	| Errore Response (m (PassoM m b)) (m (PassoM m b)) 
	| forall a. Show a => Download String a (m (PassoM m b)) (m (PassoM m b)) 
	| forall a. Read a => Password String (m (PassoM m b)) (a -> m (PassoM m b))
	| Costruito b							-- ^ valore calcolato

type PDescriptionM m b = DescriptionM m (PassoM m b)
-- | produce un passo di valore Libero nella monade Cont

libero :: (Read a, Monad m) => String -> PDescriptionM m b a
libero prompt = mkDescriptionM  $ Libero prompt

password :: (Read a,Monad m) => String -> PDescriptionM m b a
password prompt = mkDescriptionM $ Password prompt

voidCall f d k = f d $ k ()

output :: Monad m => Response -> PDescriptionM m b ()
output s = mkDescriptionM $ voidCall (Output s) 

errore :: Monad m =>  Response -> PDescriptionM m b ()
errore s = mkDescriptionM $ voidCall (Errore s)

upload prompt = mkDescriptionM $ Upload prompt

download s x = mkDescriptionM $ voidCall (Download s x)

-- | produce un passo di valore Scelta
scelte :: Monad m => [(String,a)] -> String -> PDescriptionM m b a 
scelte xs prompt = mkDescriptionM $ Scelta prompt xs 

-- | presenta un menu di scelte operative
menu 	::  Monad m => String 			-- ^ descrizione
	-> [(String,PDescriptionM m b a)] 	-- ^ menu a partire da un gestore di a
	-> PDescriptionM m b a		-- ^ il passo risultante
menu x = join . flip scelte x

rotonda :: Monad m => ((a -> PDescriptionM m b c) -> PDescriptionM m b c) -> PDescriptionM m b a
rotonda f = callCC $ forever . f 

mano :: Monad m => String -> a -> [(String,PDescriptionM m b a)] -> PDescriptionM m b a
mano s z xs = rotonda $ \k -> menu s $ ("<uscita>",k z):xs

esempio :: MonadState [Int] m => PDescriptionM m a ()
esempio = mano "esempio" () . map (second (>>= append)) $ [
		("*2", (*2) `fmap` libero "numero"),
		("+", do
			x <- libero "primo addendo"
			y <- libero "secondo addendo"
			return (x + y))
		]

	where append x = lift . modify (x:)
