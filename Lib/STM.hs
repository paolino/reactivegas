module Lib.STM where

import Control.Concurrent.STM

condSignal :: TChan a -> IO (STM Bool)
condSignal triggers = do
	reload	<- atomically $ dupTChan triggers
	let r = do
		_ <- readTChan reload
		t <-  isEmptyTChan reload
		if t then return True else r
	return $ r `orElse` return False

condSignalEq :: TChan a -> IO ((a -> Bool) -> STM Bool)
condSignalEq x = do 
	reload	<- atomically $ dupTChan x
	let r z = do
		y <- readTChan reload
		if z y then r (const True) `orElse` return True
			else r z `orElse` return False
	return (\z -> r z `orElse` return False)
