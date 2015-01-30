{-# LANGUAGE ScopedTypeVariables #-}
module Lib.Server.Session where

import Control.Exception (catch,IOException)

import Control.Applicative ((<$>))
import Control.Monad (forever)
import Control.Monad.Trans (lift)
import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar,writeTVar, readTChan, newTChan, writeTChan)
import System.Random (randomIO)
import Network.SCGI (CGI,getCookie,newCookie,setCookie, Cookie(cookieExpires))
import Control.Concurrent (forkIO, threadDelay)
import System.FilePath ((</>))
import System.Time (getClockTime, addToClockTime, noTimeDiff, tdMonth, toCalendarTime)
import Lib.Missing (secondM)
import Lib.Database (limitedDB,set,query,forget,dump,restoreDB, DB)
import Debug.Trace

type Droppable a = CGI (a, IO ())

-- | we should treat the unwilling cookie browser (reject for now, at least), those now just trash our cookie db !
sessioning 	:: forall a b . (Read b, Show b) 
		=> FilePath		-- ^ cartella di lavoro  
		-> Int  		-- ^ limit for remebering sessions
		-> STM ()		-- ^ segnale per persistere
		-> (Maybe b -> IO (a,IO b))		-- ^ creation of default value or restoring
		-> IO (Droppable a,Droppable a)	-- ^ (recall last value for cookie if present, reset anyway)
sessioning path l signal rs = do
	os <- catch (readFile (path </> "sessioni") >>= \os -> putStrLn "rilevata persistenza delle sessioni" >> return os) (\(_::IOException) -> return "[]")  
	qs <- seq (last os) .  mapM (secondM $ rs . Just) $ case reads os of
		[] -> []
		[(x,_)] -> x
	tcs <- atomically . newTVar $ restoreDB l qs
	forkIO . forever $ do
		ss <- atomically (signal >> (dump <$> readTVar tcs)) >>= mapM (\(c,(_,ios)) -> fmap ((,) c) ios)
		writeFile (path </> "sessioni") $ show ss
	let sex = "reactivegas_sessione"
	let 	new c = do
			sbk@(s,_) <- rs Nothing 
			atomically $ do
				cs <- readTVar tcs
		 		writeTVar tcs (set cs (c,sbk))
			return s
		gc = do 
			mc <- getCookie sex
			case mc of 
				Just c -> return c
				Nothing -> do 
					cn <- lift $ show <$> (randomIO :: IO Int)
					t <- lift $ getClockTime >>= return . (addToClockTime (noTimeDiff {tdMonth = 1})) >>= toCalendarTime
					let c = newCookie sex cn
					setCookie $ c{cookieExpires = Just t}
					return cn
		yes = do
			c <- gc 
			r <- lift $ do  
				ms <- atomically $ flip query c <$> readTVar tcs
				maybe (new c) (return . fst) ms 
			return (r,droppa c)
		no = do 
			c <- gc 
			r <- lift . new $ c
			return (r, droppa c)
		droppa c = atomically $ readTVar tcs >>= writeTVar tcs . flip forget c  
			 	
	return (yes,no)
