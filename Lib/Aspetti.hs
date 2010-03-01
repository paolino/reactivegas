{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, FunctionalDependencies , UndecidableInstances, OverlappingInstances, TypeOperators, NoMonomorphismRestriction #-}

-- modulo di programmazione dello stato pluggable
module Lib.Aspetti where

import Control.Applicative ((<$>)) 
import Control.Concurrent.STM (readTVar, writeTVar, atomically, newTVar)

class ParteDi l ls where
	see :: ls -> l
	set :: l -> ls -> ls

instance ParteDi l (l,ls) where 
	see (l,_) = l 
	set l (_,ls) = (l,ls) 
instance ParteDi l ls => ParteDi l (l',ls) where 
	see (_,ls) = see ls
	set l (l',ls) = (l',set l ls)

seeset :: ParteDi a ls => (a -> a) -> ls -> ls	
seeset f x =  set  (f $ see x) x




infixr 8 .<
-- | by hand adding an annotation
(.<) :: l -> ls -> (l,ls)
(.<) = (,) 

infixr 8 :*:
type a :*: b = (a,b)
{-
data MonadicBox m a = MonadicBox { get :: m a , modify :: (a -> a) -> m ()}
mkConcurrentMB :: a -> IO (MonadicBox IO a)
mkConcurrentMB a = do 
	b <- atomically $ newTVar a
	return $ MonadicBox (atomically $ readTVar b) (\f -> atomically $ readTVar b >>= writeTVar b . f)
-}



