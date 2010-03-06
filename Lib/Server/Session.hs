module Lib.Server.Session where


import Control.Applicative ((<$>))
import Control.Monad.Trans (lift)
import Control.Concurrent.STM (atomically, newTVar, readTVar,writeTVar)
import System.Random (randomIO)
import Network.SCGI (CGI,getCookie,newCookie,setCookie)


import Lib.Server.Core (limitedDB,set,query)

sessioning :: Int -> IO a -> IO (CGI a)
sessioning l rs = do
	sex <-  show <$> (randomIO :: IO Int)
	tcs <- atomically . newTVar $ limitedDB l
	return $ do
		mc <- getCookie sex
		c <- case mc of 
			Just c -> do	lift $ print c 
					return c
			Nothing -> do 
				cn <- lift $ show <$> (randomIO :: IO Int)
				setCookie $ newCookie sex cn
				return cn
		lift $ do
			s0 <- rs 
			atomically $  do
				cs <- readTVar tcs
				case query cs c of
					Just s -> return s
					Nothing -> writeTVar tcs (set cs (c,s0)) >> return s0
