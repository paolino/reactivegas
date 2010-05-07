{-# LANGUAGE ScopedTypeVariables #-}
-- | una funzione di elaborazione di Lib.Costruzione.Passo a con interazione in console.
module Lib.Console (interazione) where

import Data.Maybe
import Control.Monad (forM)
import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO, MonadIO, lift)
import Lib.Passo (Passo (..), svolgi, Costruzione)
import Lib.Response 
import Control.Exception
import System.Console.Haskeline

-- | la funzione smonta i passi in caso di undo
runPasso :: (MonadException m, Functor m) => [Passo m b] -> InputT m b

runPasso [x] = runPasso [x,x]

runPasso (Output x l:u) = do
	outputStrLn $ "\n" ++ (show x)
	getInputLine "continua .."
	lift l >>= runPasso . (:u) . fst

runPasso (Errore x l:u) = do
	outputStrLn $ "\n" ++ (show x)
	getInputLine "continua .."
	lift l >>= runPasso . (:u) . fst

runPasso (Costruito x:_) = return x 
runPasso  w@(c@(Libero p f) : u) = do
	x <- getInputLine $ "\n** " ++ p ++ ": "
	n <- case fromJust x of 
		[] -> return u
		_ -> case reads $ fromJust x of 
			[] -> case reads $ "\"" ++ fromJust x ++ "\"" of
				[] -> outputStrLn "valore non valido" >> return w
				(x,_):_ -> (:w) <$> lift (fst <$> f x) 
			(x,_):_ -> (:w) <$> lift (fst <$> f x)

	runPasso n

runPasso w@(c@(Scelta p xs f): u) = do
	outputStrLn ("\n** " ++ p) 
	forM (zip [1..] xs) $ \(n,(p,_)) -> outputStrLn $ "\t" ++ show n ++ ") " ++ take 100 p
	x <- getInputLine  "scelta: "

	n <- case fromJust x of
		[] -> return u
		_ -> case reads $ fromJust x of 
			[] -> return w
			(x,_):_ -> case x  `elem` [1 .. length xs] of
					True -> (: w) <$> (fmap fst . lift . f . snd $ xs !! (x - 1)) 
					False -> return w
	runPasso n	  
	
runPasso  w@(c@(Upload p f) : u) = do
	x <- getInputLine $  "\n** " ++ p ++ "[nome del file da caricare]: "
	n <- case fromJust x of 
		[] -> return u
		fn -> do 	
			k <- liftIO $ tryJust (\(SomeException e) -> Just (show e)) (readFile fn) 
			case k of 
				Left e -> outputStrLn e >> return w
				Right x -> do 
					case reads x of 
						[] -> outputStrLn "valore non valido" >> return w
						(x,_):_ -> (:w) <$> lift (fst <$> f x) 
	runPasso n

runPasso w@(c@(Download x y f):u) = do
		k <- liftIO $ tryJust (\(SomeException e) -> Just (show e)) (writeFile x (show y)) 
		n <- case k of 
			Left e -> outputStrLn e >> return w
			Right () -> outputStrLn "salvato." >> (:u) <$> fst <$> lift f
		runPasso n

interazione :: (MonadException m, Functor m) => Costruzione m b b -> m b
interazione f = svolgi f >>= runInputT defaultSettings . runPasso . return . fst 

