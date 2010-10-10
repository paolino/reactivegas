module Lib.Interazione where 

import Control.Monad.Cont (MonadCont, Cont (..) , runCont, callCC, join, ContT (..), forever)
import Control.Monad.State (StateT (..), evalStateT)


-- | description of an interface. In the resumable state we keep the last 2 evaluated step, while the Cont result will be a step also.

type Description a = StateT (a,a) (Cont a)

-- | a library to build a Description from a generic constructor. The constructor should be waiting for the step to go back and the future as a continuation from the value it's producing.
mkDescription :: (a -> (b -> a) -> a) -> Description a b
mkDescription f = StateT $ \(h,h') -> Cont $ \k -> let
			c x = k (x,(h',c x)) in f h c

evalDescription :: a -> (b -> a) -> Description a b -> a
evalDescription z c d = flip runCont c . flip evalStateT (z,evalDescription z c d) $ d



-- | description of an interface. In the resumable state we keep the last 2 evaluated step, while the Cont result will be a step also.

type DescriptionM m a = StateT (m a,m a) (ContT a m)

-- | a library to build a Description from a generic constructor. The constructor should be waiting for the step to go back and the future as a continuation from the value it's producing.
mkDescriptionM :: Monad m => (m a -> (b -> m a) -> a) -> DescriptionM m a b
mkDescriptionM f = StateT $ \(h,h') -> ContT $ \k -> let
			c x = k (x,(h',c x)) in return $ f h c

evalDescriptionM :: Monad m => m a -> (b -> m a) -> DescriptionM m a b -> m a
evalDescriptionM z c d = flip runContT c . flip evalStateT (z,evalDescriptionM z c d) $ d

roundAbout ::  MonadCont m => ((a -> m b) -> m a) -> m a
roundAbout f = callCC $ forever . f
