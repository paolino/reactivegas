
{-# LANGUAGE ScopedTypeVariables #-}
-- | una funzione di elaborazione di Lib.Costruzione.Passo a con interazione in console.
module Lib.Interazione.HaskelineM (interazione) where

import Data.Maybe
import Control.Monad (forM)
import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO, MonadIO, lift)
import Lib.Interazione.PassoM
import Lib.Interazione
import Lib.Response 
import Control.Exception
import System.Console.Haskeline

-- runPasso :: (MonadException m, Functor m) => PassoM m b -> InputT m b
runPasso (Costruito x) = return x
{-
runPasso (Output x l:u) = do
	outputStrLn $ "\n" ++ (show x)
	getInputLine "continua .."
	lift l >>= runPasso . (:u) . fst

runPasso (Errore x l:u) = do
	outputStrLn $ "\n" ++ (show x)
	getInputLine "continua .."
	lift l >>= runPasso . (:u) . fst
-}

runPasso  w@(Libero p h f) = do
	x <- getInputLine $ "\n** " ++ p ++ ": "
	n <-  case fromJust x of 
		[] -> h
		x -> case reads x of 
			[] -> case reads $ "\"" ++ x ++ "\"" of
				[] -> outputStrLn "valore non valido" >> return w
				xs -> let (x,"") = last xs in  f x
			xs -> let (x,"") = last xs in  f x
	runPasso n
{-
runPasso (Password p f :r) = runPasso (Libero p f : r)
-}

runPasso w@(Scelta p xs h f) = do
	outputStrLn ("\n** " ++ p) 
	forM (zip [1..] xs) $ \(n,(p,_)) -> outputStrLn $ "\t" ++ show n ++ ") " ++ take 100 p
	x <- getInputLine  "scelta: "
	n <- case fromJust x of
		[] -> h
		x -> case reads x of 
			[] -> return w
			ys -> let (x,"") = last ys in case x  `elem` [1 .. length xs] of
					True -> f . snd $ xs !! (x - 1)
					False -> return w
	runPasso n	  
{-	
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
-}

-- | la funzione svolge la Costruzione nella monade della libreria haskeline
interazione :: (MonadException m, Functor m) => b -> PDescription b b -> m b
interazione d = runInputT defaultSettings $ evalDescriptionM (Costruito d) Costruito p >>= runPassoM
