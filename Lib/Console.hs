{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
-- | una funzione di elaborazione di Lib.Costruzione.Passo a con interazione in console.
module Lib.Console (interazione) where

import Data.Maybe
import Control.Monad (forM)
import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO, MonadIO, lift)
import Lib.Passo (Passo (..), svolgi, Costruzione, HPasso)
import Lib.Response 
import Control.Exception
import System.Console.Haskeline

-- | la funzione smonta i passi in caso di undo
--runPasso :: (MonadException m, Functor m) => [Passo m b] -> InputT m b

-- runPasso [x] = runPasso [x,x]

runPasso :: MonadException m => HPasso m b -> InputT m b
runPasso (Output x (Just l),_) = do
	outputStrLn $  (show x)
	lift l >>= runPasso

runPasso (Errore x (Just l),_) = do
	outputStrLn $ "\n" ++ (show x)
	lift l >>= runPasso

runPasso (Costruito x,_) = return x 

runPasso  w@(c@(Libero p f),us) = do
	x <- getInputLine $ "\n** " ++ show p ++ ": "
	n <- case fromJust x of 
		[] -> case us of 
			[] -> outputStrLn "bottom" >> return w
			(u:us) -> (,us) <$> lift u	
		_ -> case reads $ fromJust x of 
			[] -> case reads $ "\"" ++ fromJust x ++ "\"" of
				[] -> outputStrLn "valore non valido" >> return w
				(x,_):_ -> lift (f x) 
			(x,_):_ -> lift (f x)

	runPasso n
runPasso (Password p f,us) = runPasso (Libero (ResponseOne p) f,us)

runPasso w@(c@(Scelta p xs f),u) = do
	outputStrLn ("\n** " ++ show p) 
	forM (zip [1..] xs) $ \(n,(p,_)) -> outputStrLn $ "\t" ++ show n ++ ") " ++ take 100 p
	x <- getInputLine  "scelta: "

	n <- case fromJust x of
		[] -> case u of 
			[] -> outputStrLn "bottom" >> return w
			(u:us) -> (,us) <$> lift u	
		_ -> case reads $ fromJust x of 
			[] -> return w
			(x,_):_ -> case x  `elem` [1 .. length xs] of
					True -> lift . f . snd $ xs !! (x - 1)
					False -> return w
	runPasso n	  
	
runPasso  w@(c@(Upload p f),u) = do
	x <- getInputLine $  "\n** " ++ p ++ "[nome del file da caricare]: "
	n <- case fromJust x of 
		[] -> case u of 
			[] -> outputStrLn "bottom" >> return w
			(u:us) -> (,us) <$> lift u
		fn -> do 	
			k <- liftIO $ tryJust (\(SomeException e) -> Just (show e)) (readFile fn) 
			case k of 
				Left e -> outputStrLn e >> return w
				Right x -> do 
					case reads x of 
						[] -> outputStrLn "valore non valido" >> return w
						(x,_):_ ->  lift (f x) 
	runPasso n

runPasso w@(c@(Download t x y f),u) = do
		k <- liftIO $ tryJust (\(SomeException e) -> Just (show e)) (writeFile x (show y)) 
		n <- case k of 
			Left e -> outputStrLn e >> return w
			Right () -> outputStrLn "salvato." >>  lift f
		runPasso n

-- | la funzione svolge la Costruzione nella monade della libreria haskeline
interazione :: (MonadException m, Functor m) => HPasso m b -> m b
interazione  = runInputT defaultSettings . runPasso  

