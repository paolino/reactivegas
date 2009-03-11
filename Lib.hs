module Lib where

import Control.Arrow
import Control.Monad
import Control.Concurrent.STM
import qualified Data.Map as M


partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM f [] = return ([],[])
partitionM f (x:xs) = do
	r <- f x 
	br <- partitionM f $ xs
	return . (if r then first (x:) else second (x:)) $ br

iterateM :: Monad m => (a -> m a) -> a -> m [a]
iterateM f x = f x >>= \y -> iterateM f y >>= return . (x:)

injectM :: Monad m => a -> [a -> m a] -> m a
injectM = foldM (flip ($))

-----------------------------------------

modifica  :: Ord k => (a -> a) -> k -> a -> M.Map k a -> M.Map k a
modifica f k z m = case k `M.member` m of
	True -> M.insertWith (const f) k undefined m
	False -> M.insert k (f z) m 

modifyTVar :: (a -> a) -> TVar a -> STM ()
modifyTVar f x = readTVar x >>= writeTVar x . f
