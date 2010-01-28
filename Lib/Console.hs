{-# LANGUAGE ScopedTypeVariables #-}
-- | una funzione di elaborazione di Lib.Costruzione.Passo a con interazione in console.
module Lib.Console (runPasso) where

import Control.Monad (forM)
import Lib.Costruzione (Passo (..))

-- | la funzione smonta i passi in caso di undo
runPasso :: [Passo b] -> IO b

runPasso [] = error "lista vuota"

runPasso (Costruito x:_) = return x 
runPasso  w@(c@(Libero p f) : u) = do
	putStr $ p ++ ": "
	x <- getLine 
	n <- case x of 
		[] -> return u
		_ -> case reads x of 
			[] -> return w
			(x,_):_ -> return $ f x : w 
	runPasso n

runPasso w@(c@(Scelta p xs f): u) = do
	putStrLn p 
	forM (zip [1..] xs) $ \(n,(p,_)) -> putStrLn $ "\t" ++ show n ++ ") " ++ p
	putStr "scelta: "
	x <- getLine 
	n <- case x of
		[] -> return u
		_ -> case reads x of 
			[] -> return w
			(x,_):_ -> case x  `elem` [1 .. length xs] of
					True -> return $ (f . snd $ xs !! (x - 1)) : w
					False -> return w
	runPasso n	  
	


