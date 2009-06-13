{-# LANGUAGE NoMonomorphismRestriction,FlexibleContexts,ScopedTypeVariables  #-}
module Servizio where

import Control.Monad.RWS
import Control.Applicative
import Control.Arrow
import Data.List
import Data.Maybe
import Control.Monad.Maybe

import Core
import Lib0
import Lib1
import Aspetti ((.<),ParteDi,see)

data (Read a,Show a) => Servizio a = Servizio {contatore :: Int, sottostato :: [(Int,(String,a))]} deriving 
	(Show, Read)

statoInizialeServizio x = Servizio 0 [] .< x
nuovoStatoServizio s q = do
	Servizio p ls <- osserva
	modifica $ \_ -> Servizio (p + 1) ((p,(q,s)):ls)
	return p

servizioPresente j = do
	s@(Servizio p ls) <- osserva  
	fallimento (assente j ls) $ "richiesta di servizio fallita , indice " ++ show j ++ " non trovato"
	return s

osservaStatoServizio j = do 
	Servizio p ls <- servizioPresente j 	
	snd <$> return (ls ? (j,undefined))

modificaStatoServizio j f = do
	Servizio p ls <- servizioPresente j 
	ls' <- updateM j (\(q,a) -> (,) q <$> f a)  undefined ls
	modifica $ \_ -> Servizio p ls'

eliminaStatoServizio :: forall a s c d. (ParteDi (Servizio a) s,
                        Show a,Read a
                        ) =>
                       Int -> a -> MaybeT (Inserzione s c d) ()
eliminaStatoServizio j proxy = do
	Servizio p ls <- servizioPresente j :: MaybeT (Inserzione s c d) (Servizio a)
	modifica $ \_ -> Servizio p (elimina j ls)

elencoSottoStati :: forall a s c d. (ParteDi (Servizio a) s,
                        Show a,Read a
                        ) =>
                       a -> s -> [(Int,String)]
elencoSottoStati proxy s = let
	Servizio p ls = see s :: Servizio a
	in map (id *** fst) ls

seeStatoServizio :: forall a s c d. (ParteDi (Servizio a) s,
                        Show a,Read a
                        ) =>
                       a -> s -> Int -> Maybe (String,a)
seeStatoServizio proxy s i =
	let Servizio p ls = see s :: Servizio a
	in lookup i $ ls

