{-# LANGUAGE NoMonomorphismRestriction #-}
-- | un wrapper intorno a Lib.Costruzione m per semplificare la costruzione di interfacce
module Core.Costruzione (libero, scelte, dafile, Supporto, runSupporto, CostrAction) where


import Control.Applicative ((<$>))
import Control.Monad.Error (lift, runErrorT, ErrorT)
import Control.Monad.Reader (runReaderT, ReaderT)

import qualified Lib.Passo as P (Costruzione , libero, dafile, scelte)

-- | monade di supporto per la costruzione di valori con il valore interrogativo in reader e con la possibilita di fallire 
type Supporto m s b = ReaderT s (ErrorT String (P.Costruzione m b))

-- | eleva la costruzione nel supporto
toSupporto :: Monad m => P.Costruzione m b a -> Supporto m s b a
toSupporto = lift . lift

-- | dato lo stato interrogativo e le continuazioni in caso di errore o meno esegue una azione di Supporto m
runSupporto:: Monad m => s -> (String -> P.Costruzione m b c) -> (a -> P.Costruzione m b c) -> Supporto m s b a -> P.Costruzione m b c
runSupporto s kn kp f = runErrorT (runReaderT f s) >>= either kn kp

-- | passo libero elevato al supporto
libero :: (Monad m ,Read a) => String -> Supporto m s b a
libero = toSupporto . P.libero

-- | passo dafile elevato al supporto
dafile :: (Read a , Monad m) => String -> Supporto m s b a
dafile = toSupporto . P.dafile

-- | passo scelte elevato al supporto
scelte :: Monad m => [(String, a)] -> String -> Supporto m s b a
scelte xs = toSupporto . P.scelte xs

-- | il tipo degli insiemi di azioni costruttive
type CostrAction m c q s = 
	s  					-- ^ stato in lettura
	-> (q -> P.Costruzione m c ())		-- ^ azione di successo
 	-> (String -> P.Costruzione m c ())	-- ^ azione di fallimento
 	-> [(String, P.Costruzione m c ())]	-- ^ lista di azioni costruttive taggate con il loro nome di selezione
