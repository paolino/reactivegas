{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving, FlexibleContexts, ScopedTypeVariables, NoMonomorphismRestriction,ViewPatterns #-}
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import Data.Maybe
data SceltaOLibero a = Scelta String [(String,a)] | Libero String

type Continuazione m s c a = a -> Costruzione m s c
data Costruzione m s c  
	= forall a . (Show a , Read a) 
		=> Costruzione (SceltaOLibero a) (Continuazione m s c a)
	| Chiuso (m (Costruzione m s c))

costr x = lift . ContT $ Costruzione x 
make m = \s ->  runContT (runReaderT m s) Chiuso

 
runCostruzioneIO c =  
	let zeta c@(Costruzione l f) = do 
		let riprova s  = nl >> msg s >> zeta c
		r <- runErrorT $ nl >> case l of
			Libero z -> do	x <- pgl z
					backifnull (errorOrAhead  ( lift . f) . stringOrNot) x
			Scelta t as -> do 	let bs = ("* fine programma *",undefined) : as
						liftIO $ mapM_ putStrLn  [show n ++ ". " ++ a | (n,a) <- zip [0..] (map fst bs)]
						nl
						let 	q y = do 	when (y < 0 || y > length as) $ throwError "scelta impossibile"
									lift .  f . snd $ (as !! (y - 1))	
						pgl t >>= backifnull (errorOrAhead q . toMaybe . reads)
		either riprova return r 
		where
			backifnull f x = if null x then return Nothing else f x
			errorOrAhead q =  maybe (throwError "errore di lettura") 
				(\x -> q x >>= lift . zeta >>= maybe (lift $ zeta c) (return . Just)) 
			stringOrNot s = toMaybe $ case reads s of
				[] -> reads ("\"" ++ s ++ "\"")
				x -> x
			nl 	= liftIO (putStrLn "")
			prompt 	= liftIO . putStr . (++ ": ")
			pgl s 	= prompt s >> gl
			msg s 	=  nl >> (liftIO . putStrLn . (++ " ****").("**** " ++) $ s)
			gl 	= liftIO getLine
			toMaybe =  fmap fst . listToMaybe
	in zeta c	
{-
main = do
	callCC fine where
		fine k = forever (callCC $ base k) 
		base k k1 =  
-}
