module Lib.Assocs where

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

updateM :: (Monad m ,Eq a) => a -> (b -> m b) -> b -> [(a,b)] -> m [(a,b)]
updateM k dv v kvs = case lookup k kvs of
	Nothing -> dv v >>= \v' -> return $ (k,v'):kvs
	Just v -> dv v >>= \v' -> return $ (k,v'):filter ((/=) k . fst) kvs
(?) :: Eq a => [(a,b)] -> (a,b) -> b
xs ? (k,t) = maybe t id $ lookup k xs 

