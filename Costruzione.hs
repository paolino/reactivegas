{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving, FlexibleContexts, ScopedTypeVariables, NoMonomorphismRestriction,ViewPatterns #-}

module Costruzione where

import Aspetti
import Control.Monad.Cont
import Control.Monad.Reader

import Data.Maybe
import Control.Monad.Error
import Codec.Binary.UTF8.String

data SceltaOLibero a = Scelta String [(String,a)] | Libero String

type Continuazione m r b a = a -> m (Costruzione m r b) -- una continuazione monadica
data Monad m => Costruzione m r b 
	= forall a . (Show a,Read a) => 
		Costruzione (SceltaOLibero a) (Continuazione m r b a)

newtype Monad m => Svolgimento r b m a = Svolgimento (ContT (Costruzione m r b) (ReaderT r m) a)  deriving 
	(Functor, Monad, MonadReader r, MonadCont)

instance MonadTrans (Svolgimento r b) where
	lift k = Svolgimento (lift . lift $  k) 
instance Show (Svolgimento r b m a)
instance Read (Svolgimento r b m a)

parametro :: (Monad m, Show a,Read a) => SceltaOLibero a -> Svolgimento r b m a 
parametro scelte = Svolgimento (ContT $ \k -> ask >>= \r -> return (Costruzione scelte (\a -> runReaderT (k a) r)))

svolgi :: Monad m => Svolgimento r b m b  -> r -> m (Costruzione m r b)
svolgi (Svolgimento c) r = runReaderT (runContT c undefined) r

----------------------  un driver per utilizzare una Costruzione ----------------------------------------						

runCostruzioneIO :: (Monad m, MonadIO m) =>  Costruzione m r a -> m (Maybe a)
runCostruzioneIO  c = flip runContT return . callCC $ \k -> 
	let zeta c@(Costruzione l f) = do 
		let riprova s  = nl >> msg s >> zeta c
		r <- runErrorT $ nl >> case l of
			Libero z -> do	(encodeString -> x) <- pgl z
					backifnull (errorOrAhead (lift . lift . f) . stringOrNot) x
			Scelta t as -> do 	let bs = ("* fine programma *",undefined) : as
						liftIO $ mapM_ putStrLn  [show n ++ ". " ++ decodeString a | (n,a) <- zip [0..] (map fst bs)]
						nl
						let 	q y = do 	when (y < 0 || y > length as) $ throwError "scelta impossibile"
									when (y == 0) $ lift (k Nothing)
									lift . lift .  f . snd $ (as !! (y - 1))	
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
			msg s 	=  nl >> (liftIO . putStrLn . (++ " ****").("**** " ++). decodeString $ s)
			gl 	= liftIO getLine
			toMaybe =  fmap fst . listToMaybe
	in zeta c			
			

