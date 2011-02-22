{-# LANGUAGE ExistentialQuantification #-}

module Core.Differenze where

import Control.Monad.Trans (lift)
import Core.Types
import Core.Costruzione

class FromDiff s q where
	maybeDiff :: s -> s -> Maybe q

data FDbox s = forall q. (Show q, FromDiff s q) => FDbox (Maybe q)

tryFDbox :: s -> s -> FDbox s -> FDbox s
tryFDbox s0 s1 (FDbox x) = FDbox (maybeDiff s0 s1 `asTypeof` x)

useCostrActionDiff ::  m s -> (m s -> Costruzione m b s) -> [FDbox s] -> Costruzione m b [FDbox s] 
useCostrActionDiff gs ac xs = do
	s' <- ac gs
	s'' <- lift gs
	return $ map (tryFDbox s'' s') xs
