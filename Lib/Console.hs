{-# LANGUAGE ScopedTypeVariables #-}
-- | una funzione di elaborazione di Lib.Costruzione.Passo a con interazione in console.
module Lib.Console (runPasso) where

import Data.Maybe
import Control.Monad (forM)
import Control.Monad.Trans (liftIO)
import Lib.Costruzione (Passo (..))
import Control.Exception
import System.Console.Haskeline

-- | la funzione smonta i passi in caso di undo
runPasso :: [Passo b] -> InputT IO b

runPasso [] = error "lista vuota"

runPasso (Costruito x:_) = return x 
runPasso  w@(c@(Libero p f) : u) = do
	x <- getInputLine $ p ++ ": "
	n <- case fromJust x of 
		[] -> return u
		_ -> case reads $ fromJust x of 
			[] -> case reads $ "\"" ++ fromJust x ++ "\"" of
				[] -> outputStrLn "valore non valido" >> return w
				(x,_):_ -> return $ f x : w 
			(x,_):_ -> return $ f x : w 

	runPasso n

runPasso w@(c@(Scelta p xs f): u) = do
	outputStrLn p 
	forM (zip [1..] xs) $ \(n,(p,_)) -> outputStrLn $ "\t" ++ show n ++ ") " ++ p
	x <- getInputLine  "scelta: "

	n <- case fromJust x of
		[] -> return u
		_ -> case reads $ fromJust x of 
			[] -> return w
			(x,_):_ -> case x  `elem` [1 .. length xs] of
					True -> return $ (f . snd $ xs !! (x - 1)) : w
					False -> return w
	runPasso n	  
	
runPasso  w@(c@(DaFile p f) : u) = do
	x <- getInputLine $  p ++ "[nome del file da caricare]: "
	n <- case fromJust x of 
		[] -> return u
		fn -> do 	
			k <- liftIO $ tryJust (\(SomeException e) -> Just (show e)) (readFile fn) 
			case k of 
				Left e -> outputStrLn e >> return w
				Right x -> do 
					case reads x of 
						[] -> outputStrLn "valore non valido" >> return w
						(x,_):_ -> return $ f x : w 
	runPasso n


