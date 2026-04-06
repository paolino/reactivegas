{- |
Module      : Lib.STM
Description : STM utilities for conditional signaling
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Utilities for STM-based conditional signaling using channels.
-}
module Lib.STM
    ( condSignal
    , condSignalEq
    ) where

import Control.Concurrent.STM
    ( STM
    , TChan
    , atomically
    , dupTChan
    , isEmptyTChan
    , orElse
    , readTChan
    )

-- | Create a conditional signal that returns 'True' when the channel
-- has been written to since the signal was created
condSignal
    :: TChan a
    -- ^ trigger channel
    -> IO (STM Bool)
    -- ^ action returning 'True' if signal received
condSignal triggers = do
    reload <- atomically $ dupTChan triggers
    let drain = do
            _ <- readTChan reload
            isEmpty <- isEmptyTChan reload
            if isEmpty then return True else drain
    return $ drain `orElse` return False

-- | Create a conditional signal with equality predicate
-- Returns 'True' when a matching value is received
condSignalEq
    :: TChan a
    -- ^ trigger channel
    -> IO ((a -> Bool) -> STM Bool)
    -- ^ action taking predicate, returning 'True' if match found
condSignalEq channel = do
    reload <- atomically $ dupTChan channel
    let check predicate = do
            value <- readTChan reload
            if predicate value
                then check (const True) `orElse` return True
                else check predicate `orElse` return False
    return $ \predicate -> check predicate `orElse` return False
