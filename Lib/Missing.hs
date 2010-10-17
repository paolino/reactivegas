
{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}

module Lib.Missing where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Monad.Error (throwError, ErrorT)
import Data.Typeable
import Data.Char (toLower)
import Data.Function (on)
import Data.List (lookup, sortBy)
-----------------------------------------
sortLower = sortByLower id
sortByLower f = sortBy (compare `on` (map toLower . f))
----------------------------------

secondM :: (Monad m) => (a -> m b) -> (c, a) -> m (c, b)
secondM f (x,y) = f y >>= return . (,) x

firstM :: (Monad m) => (a -> m b) -> (a, c) -> m (b, c)
firstM f (x,y) = f x >>= return . flip (,) y


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

-- | erroring on Nothing
onNothing :: Monad m => String -> Maybe a -> ErrorT String m a
onNothing x = maybe (throwError x) return  

infixr 8 >$>
(>$>) :: Functor f => (a -> b) -> (c -> f a) -> c -> f b
(>$>) = (.) . (<$>)

catchRead :: forall a. (Read a,Typeable a) => String -> String -> a
catchRead err tok = case reads tok of
	[] -> e
	xs -> case last xs of 
		(x,"") -> x
		_ -> e
	where e = error $ err ++ " reading " ++ show (take 20 tok) ++ " as type " ++ show (typeOf (undefined::a))

untilNothing :: (Monad m, Functor m) =>  b -> (b -> m (Maybe a, b)) -> m [a]
untilNothing r f = do
	(x,r') <- f r
	case x of
		Just y -> (y:) `fmap` untilNothing r' f
		Nothing -> return []


