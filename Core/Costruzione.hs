{-# LANGUAGE NoMonomorphismRestriction #-}
-- | un wrapper intorno a Lib.Costruzione per semplificare la costruzione di interfacce
module Core.Costruzione (libero, scelte, dafile, Supporto, runSupporto, CostrAction) where


import Control.Applicative ((<$>))
import Control.Monad.Error (lift, runErrorT, ErrorT)
import Control.Monad.Reader (runReaderT, ReaderT)

import qualified Lib.Costruzione as C (Costruzione, libero, dafile, scelte)

-- | monade di supporto per la costruzione di valori con il valore interrogativo in reader e con la possibilita di fallire 
type Supporto s b = ReaderT s (ErrorT String (C.Costruzione b))

-- | eleva la costruzione nel supporto
toSupporto :: C.Costruzione b a -> Supporto s b a
toSupporto = lift . lift

-- | dato lo stato interrogativo e le continuazioni in caso di errore o meno esegue una azione di Supporto
runSupporto :: s -> (String -> C.Costruzione b c) -> (a -> C.Costruzione b c) -> Supporto s b a -> C.Costruzione b c
runSupporto s kn kp f = runErrorT (runReaderT f s) >>= either kn kp

-- | passo libero elevato al supporto
libero :: Read a => String -> Supporto s b a
libero = toSupporto . C.libero

-- | passo dafile elevato al supporto
dafile :: Read a => String -> Supporto s b a
dafile = toSupporto . C.dafile

-- | passo scelte elevato al supporto
scelte :: [(String, a)] -> String -> Supporto s b a
scelte xs = toSupporto . C.scelte xs

-- | il tipo degli insiemi di azioni costruttive
type CostrAction c q s = 
	s  					-- ^ stato in lettura
	-> (q -> C.Costruzione c ())		-- ^ azione di successo
 	-> (String -> C.Costruzione c ())	-- ^ azione di fallimento
 	-> [(String, C.Costruzione c ())]	-- ^ lista di azioni costruttive taggate con il loro nome di selezione
