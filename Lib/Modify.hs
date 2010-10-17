module Lib.Modify where

import Control.Concurrent.STM
import Control.Monad.Trans (MonadIO , MonadTrans, liftIO)

data PeekPoke a = PeekPoke {peek :: IO a , poke :: a -> IO (), modify :: (a -> IO a) -> IO ()}

modifyT :: MonadIO t => PeekPoke a -> (a -> t (Maybe a)) -> t ()
modifyT pp f = 	do 
	y <- liftIO (peek pp) 
	x <- f y
	case x of 
		Just x -> liftIO . poke pp $ x
		Nothing -> return ()
	
mkPeekPoke x s = do
	tt <- atomically $ newTVar x
	return $ PeekPoke (atomically (readTVar tt)) (\x -> atomically (writeTVar tt x) >> s) 
		(\f -> atomically (readTVar tt) >>= f >>= atomically . writeTVar tt  >> s)

type Modify m a =  (a -> m (Maybe a)) -> m ()

writeModify f x = f $ \_ -> return $ Just x

readModify  :: Monad m => Modify m a -> (a -> m ()) -> m ()
readModify f g = f $ \x -> g x >> return Nothing




mkModify x s = do
	tt <- atomically $ newTVar x
	return $ \f -> do  
		mt <- (liftIO . atomically $ readTVar tt) >>= f 
		case mt of 
			Nothing -> return ()
			Just t -> do 
				liftIO . atomically $ writeTVar tt t
				s

mkModifyAssocList mkA xs s = do 
	gs <- atomically $ newTVar  xs
	return $ \f -> do
		xs <- liftIO . atomically $ readTVar gs 
		mxs' <- f $ map fst xs 
		case mxs' of
			Nothing -> return ()
			Just xs' -> do 
				let 	ns = filter (not . (`elem` map fst xs)) xs'
				as <- mapM mkA ns
				liftIO . atomically $ writeTVar gs (xs ++ zip ns as)
				s

