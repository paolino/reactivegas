{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}
-- | un wrapper intorno a Lib.Costruzione m per semplificare la costruzione di interfacce
module Core.Costruzione (libero, password, scelte, upload, Supporto, runSupporto, CostrAction, download) where

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Control.Monad.Error (lift, runErrorT, ErrorT,MonadError)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Reader (runReaderT, ReaderT, MonadReader(..))

import qualified Lib.Passo as P (Costruzione , libero, upload, scelte, download,password)

import Debug.Trace

-- | monade di supporto per la costruzione di valori con il valore interrogativo in reader e con la possibilita di fallire 
newtype Supporto m s b a = Supporto {unSupporto :: ReaderT (m s) (ErrorT String (P.Costruzione m b)) a} deriving
	(Monad
	,Functor
	,MonadError String
	)

instance Monad m => MonadReader s (Supporto m s b) where
	ask = Supporto $ ask >>= lift . lift . lift . lift
	local f (Supporto k) = Supporto $ local (liftM f) k

-- | eleva la costruzione nel supporto
toSupporto :: Monad m => P.Costruzione m b a -> Supporto m s b a
toSupporto = Supporto . lift . lift

-- | dato lo stato interrogativo e le continuazioni in caso di errore o meno esegue una azione di Supporto m
runSupporto	:: Monad m 
		=> m s 
		-> (String -> P.Costruzione m b c) 
		-> (a -> P.Costruzione m b c) 
		-> Supporto m s b a 
		-> P.Costruzione m b c
runSupporto s kn kp (Supporto f) = runErrorT (runReaderT f s) >>= either kn kp

-- | passo libero elevato al supporto
libero :: (Monad m ,Read a) => String -> Supporto m s b a
libero = toSupporto . P.libero

-- | passo libero elevato al supporto
password :: (Monad m ,Read a) => String -> Supporto m s b a
password = toSupporto . P.password


-- | passo upload elevato al supporto
upload :: (Read a , Monad m) => String -> Supporto m s b a
upload = toSupporto . P.upload

download :: (Show a , Monad m) => String -> a -> Supporto m s b ()
download q = toSupporto . P.download q

-- | passo scelte elevato al supporto
scelte :: (Monad m) => [(String, a)] -> String -> Supporto m s b a
scelte xs = toSupporto . P.scelte xs


-- | il tipo degli insiemi di azioni costruttive
type CostrAction m c q s = 
	m s  					-- ^ stato in lettura
	-> (q -> P.Costruzione m c ())		-- ^ azione di successo
 	-> (String -> P.Costruzione m c ())	-- ^ azione di fallimento
 	-> [(String, P.Costruzione m c ())]	-- ^ lista di azioni costruttive taggate con il loro nome di selezione
