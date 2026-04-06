{-# LANGUAGE MonoLocalBinds #-}

{- |
Module      : Lib.Filesystem
Description : Filesystem utilities for group data persistence
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Provides utilities for reading and writing group-related data
to the filesystem with error handling.
-}
module Lib.Filesystem
    ( onFS
    , groupWrite
    , groupUnwrite
    , groupUnwriteF
    ) where

import Control.Exception (SomeException (..), tryJust)
import System.FilePath ((</>))

-- | Execute an IO action with exception handling
-- Takes an IO action, an error handler, and a success handler
onFS
    :: IO a
    -- ^ IO action to execute
    -> (String -> IO b)
    -- ^ error handler (receives error message)
    -> (a -> IO b)
    -- ^ success handler
    -> IO b
onFS action onError onSuccess =
    tryJust (\(SomeException x) -> Just $ show x) action
        >>= either onError onSuccess

-- | Write group-related data to a file
groupWrite
    :: (Show a)
    => FilePath
    -- ^ group directory
    -> String
    -- ^ filename
    -> Int
    -- ^ version number
    -> a
    -- ^ data to write
    -> IO ()
groupWrite dir filename version value =
    onFS
        (writeFile (dir </> filename) (show (version, value)))
        error
        return

-- | Read group-related data from a file using standard Read
groupUnwrite
    :: (Read a)
    => FilePath
    -- ^ group directory
    -> String
    -- ^ filename
    -> IO (Maybe (Int, a))
    -- ^ 'Just' (version, value) if successful, 'Nothing' on error
groupUnwrite dir filename =
    onFS (readFile (dir </> filename)) (const $ return Nothing) (return . parse)
  where
    parse content = case reads content of
        [(result, _)] -> Just result
        _ -> Nothing

-- | Read group-related data from a file using a custom parser
groupUnwriteF
    :: ReadS (Int, a)
    -- ^ custom parser
    -> FilePath
    -- ^ group directory
    -> String
    -- ^ filename
    -> IO (Maybe (Int, a))
    -- ^ 'Just' (version, value) if successful, 'Nothing' on error
groupUnwriteF customParser dir filename =
    onFS (readFile (dir </> filename)) (const $ return Nothing) (return . parse)
  where
    parse content = case customParser content of
        [(result, _)] -> last content `seq` Just result
        _ -> Nothing
