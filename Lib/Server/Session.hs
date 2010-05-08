module Lib.Server.Session where


import Control.Applicative ((<$>))
import Control.Monad.Trans (lift)
import Control.Concurrent.STM (atomically, newTVar, readTVar,writeTVar)
import System.Random (randomIO)
import Network.SCGI (CGI,getCookie,newCookie,setCookie)


import Lib.Database (limitedDB,set,query,forget)
import Debug.Trace

type Droppable a = CGI (a, IO ())

-- | we should treat the unwilling cookie browser (reject for now, at least), those now just trash our cookie db !
sessioning 	:: Int  		-- ^ limit for remebering sessions
		-> IO a 		-- ^ creation of default value
		-> IO (Droppable a,Droppable a)	-- ^ (recall last value for cookie if present, reset anyway)
sessioning l rs = do
	sex <-  show <$> (randomIO :: IO Int)
	tcs <- atomically . newTVar $ limitedDB l
	let 	new c = do
			s <- rs 
			atomically $ do
				cs <- readTVar tcs
		 		writeTVar tcs (set cs (c,s))
			return s
		gc = do 
			mc <- getCookie sex
			case mc of 
				Just c -> return c
				Nothing -> do 
					cn <- lift $ show <$> (randomIO :: IO Int)
					setCookie $ newCookie sex cn
					return cn
		yes = do
			c <- gc 
			r <- lift $ do  
				ms <- atomically $ flip query c <$> readTVar tcs
				maybe (new c) return ms 
			return (r,droppa c)
		no = do 
			c <- gc 
			r <- lift . new $ c
			return (r, droppa c)
		droppa c = atomically $ readTVar tcs >>= writeTVar tcs . flip forget c 
			 	
	return (yes,no)
