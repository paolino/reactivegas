module Lib.Missing where

import Control.Applicative ((<$>))
import Control.Arrow (second)

deleteM 	:: (Functor m, Monad m) 
		=> (a -> m Bool) 	-- ^ judging function	
		-> [a] 			-- ^ judged list
		-> m (Maybe [a])	-- ^ the menomated list or signal that no elem was deleted
deleteM f [] = return Nothing
deleteM f (x:xs) = f x >>= bool (return $ Just xs) (fmap (x:) <$>  deleteM f xs) 

deleteMb 	:: (Functor m, Monad m) 
		=> (b -> a -> m (Maybe b)) 	-- ^ state update and judging function	
		-> b			-- ^ initial state
		-> [a] 			-- ^ judged list
		-> m (Maybe (b,[a]))	-- ^ the final state and menomated list or signal that no elem was used

deleteMb f y [] = return Nothing
deleteMb f y (x:xs) = f y x >>= maybe (fmap (second (x:)) <$>  deleteMb f y xs) (\y' -> return $ Just (y',xs))

bool f g x = if x then f else g

foldDeleteMb 	:: (Functor m, Monad m)
		=> (b -> a -> m (Maybe b))	-- ^ state update and judging function
		-> b				-- ^ initial state
		-> [a]				-- ^ judged list
		-> m (b,[a])			-- ^ the final state and the unacceptable elements

foldDeleteMb	f y xs = deleteMb f y xs >>= maybe (return (y,xs)) (uncurry $ foldDeleteMb f) 
