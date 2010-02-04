{-# LANGUAGE ScopedTypeVariables #-}
-- | una funzione di elaborazione di Lib.Costruzione.Passo a con interazione in console.
module Lib.Console (interazione) where

import Data.Maybe
import Control.Monad (forM)
import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO, MonadIO, lift)
import Lib.Passo (Passo (..), svolgi, Costruzione)
import Control.Exception
import System.Console.Haskeline

-- | la funzione smonta i passi in caso di undo
runPasso :: MonadException m => [Passo m b] -> InputT m b

runPasso [] = error "lista vuota"

runPasso (Costruito x:_) = return x 
runPasso  w@(c@(Libero p f) : u) = do
	x <- getInputLine $ p ++ ": "
	n <- case fromJust x of 
		[] -> return u
		_ -> case reads $ fromJust x of 
			[] -> case reads $ "\"" ++ fromJust x ++ "\"" of
				[] -> outputStrLn "valore non valido" >> return w
				(x,_):_ -> (:w) <$> lift (f x) 
			(x,_):_ -> (:w) <$> lift (f x)

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
					True -> (: w) <$> (lift . f . snd $ xs !! (x - 1)) 
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
						(x,_):_ -> (:w) <$> lift (f x) 
	runPasso n

interazione :: (MonadException m) => b -> Costruzione m b b -> m b
interazione base f = svolgi f >>= runInputT defaultSettings . runPasso . (:[Costruito base]) 

