module Lib.Missing where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Monad.Error (throwError, ErrorT)


import Data.List (lookup)
-----------------------------------------
update :: (Eq a) => a -> (b -> b) -> b -> [(a, b)] -> [(a, b)]
update k dv v kvs = case lookup k kvs of
	Nothing -> (k,dv v):kvs
	Just v -> (k,dv v):filter ((/=) k . fst) kvs

assente :: (Eq a) => a -> [(a, b)] -> Bool
assente k kvs = case lookup k kvs of
	Nothing -> True
	Just _ -> False

elimina :: (Eq a) => a -> [(a, b)] -> [(a, b)]
elimina k kvs = filter ((/=) k . fst) kvs

secondM :: (Monad m) => (a -> m b) -> (c, a) -> m (c, b)
secondM f (x,y) = f y >>= return . (,) x

firstM :: (Monad m) => (a -> m b) -> (a, c) -> m (b, c)
firstM f (x,y) = f x >>= return . flip (,) y

updateM :: (Monad m ,Eq a) => a -> (b -> m b) -> b -> [(a,b)] -> m [(a,b)]
updateM k dv v kvs = case lookup k kvs of
	Nothing -> dv v >>= \v' -> return $ (k,v'):kvs
	Just v -> dv v >>= \v' -> return $ (k,v'):filter ((/=) k . fst) kvs
(?) :: Eq a => [(a,b)] -> (a,b) -> b
xs ? (k,t) = maybe t id $ lookup k xs 

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

