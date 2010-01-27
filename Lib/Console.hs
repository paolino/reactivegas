{-# LANGUAGE ScopedTypeVariables #-}

import Lib.Costruzione
import Control.Monad.Cont

runPasso :: Passo IO b -> IO b

runPasso (Costruito x) = return x 

runPasso  c@(Libero p u f) = do
	putStr p
	x <- getLine 
	n <- case x of 
		[] -> return u 
		_ -> case reads x of 
			[] -> return c
			(x,_):_ -> f x 
	runPasso n

runPasso c@(Scelta p xs u f) = do
	putStrLn p 
	forM (zip [1..] xs) $ \(n,(p,_)) -> putStrLn $ "\t" ++ show n ++ ") " ++ p
	putStr "scelta: "
	x <- getLine 
	n <- case x of
		[] -> return u 
		_ -> case reads x of 
			[] -> return c
			(x,_):_ -> case x  `elem` [1 .. length xs] of
					True -> f . snd $ xs !! (x - 1)
					False -> return c
	runPasso n	  
	
f :: Monad m => Costruzione m Int
f = do
	x <- libero "primo:" 
	y <- libero "secondo:" 
	z <- scelte [("somma",(+)),("differenza",(-))] "operazione:" 
	return $ z x y


