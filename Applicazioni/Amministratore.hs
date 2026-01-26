{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Applicazioni.Amministratore
Description : Group administrator management
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Provides functionality for managing multiple purchasing groups,
including group creation, validation, and state access.
-}
module Applicazioni.Amministratore
    ( -- * Types
      Group
    , Administrator (..)

      -- * Administrator creation
    , mkAdministrator
    ) where

import Control.Concurrent.STM
    ( STM
    , TChan
    , TVar
    , atomically
    , newTChan
    , newTVarIO
    , readTVar
    , readTVarIO
    , writeTChan
    , writeTVar
    )
import Control.Monad (liftM2, when)
import Control.Monad.Trans (liftIO)
import Data.Maybe (isNothing)
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , renameDirectory
    )
import System.FilePath (addExtension, takeFileName, (</>))
import System.FilePath.Find (FileType (..), depth, fileName, fileType, find)

import Core.Types (Responsabile)
import Lib.STM (condSignalEq)
import Lib.Tokens (Token)

-- | Group name identifier
type Group = String

-- | Administrator interface for managing multiple groups
data Administrator a = Administrator
    { adminReloadCondition :: IO (Group -> IO Bool)
    -- ^ Returns a function that waits for a group reload signal
    , checkGroupName :: Group -> IO Bool
    -- ^ Check if a group name is available (not already used)
    , checkPassword :: Token -> Bool
    -- ^ Validate administrator password
    , bootNewGroup :: (Group, Responsabile) -> IO Bool
    -- ^ Create and initialize a new group
    , listGroups :: IO [Group]
    -- ^ List all available groups
    , getGroupValue :: Group -> STM (Maybe a)
    -- ^ Get the current value for a group
    }

-- | Create an administrator for managing groups
mkAdministrator
    :: Token
    -- ^ administrator password
    -> ((FilePath, Group, Maybe Responsabile, STM ()) -> IO a)
    -- ^ group creation or reading function
    -> FilePath
    -- ^ working directory
    -> IO (Administrator a)
    -- ^ the resulting administrator
mkAdministrator password readGroup dir = do
    -- Analyze directories
    directories <-
        tail
            `fmap` find
                ((== 0) `fmap` depth)
                ( liftM2
                    (&&)
                    ((== Directory) `fmap` fileType)
                    ((/= "static") `fmap` fileName)
                )
                dir
    -- Groups and values
    let names = map takeFileName directories
    reloadChan <- atomically newTChan :: IO (TChan (Either () Group))
    let notifyGroup = writeTChan reloadChan . Right
    groups <-
        zip names
            `fmap` mapM
                readGroup
                ( zipWith
                    (\path name -> (path, name, Nothing, notifyGroup name))
                    directories
                    names
                )

    putStrLn "** Recovered the following groups:"
    mapM_ (\(x, _) -> putStrLn $ "\t" ++ x) groups

    -- condSignalEq :: TChan a -> IO ((a -> Bool) -> STM Bool)
    let matchReload _y (Left ()) = True
        matchReload y (Right x) = x == y
        reloadCondition = do
            t <- condSignalEq reloadChan
            return $ \n -> atomically $ t (matchReload n)

    -- Variable mapping
    groupsVar <- newTVarIO groups
    let createNewGroup groupName responsible = do
            groupValue <-
                readGroup
                    ( dir </> groupName
                    , groupName
                    , Just responsible
                    , notifyGroup groupName
                    )
            liftIO
                . atomically
                $ readTVar groupsVar
                    >>= writeTVar groupsVar . ((groupName, groupValue) :)

        createDirectory groupName = do
            let groupDir = dir </> groupName
            exists <- doesDirectoryExist groupDir
            when exists $
                renameDirectory
                    groupDir
                    (groupDir </> addExtension groupName "copia")
            createDirectoryIfMissing True (groupDir </> "static")
            putStrLn $ "created " ++ groupDir

        checkGroupName' x =
            (isNothing . lookup x) `fmap` readTVarIO groupsVar

        checkPassword' = (== password)

        bootNewGroup' (groupName, responsible) = do
            canCreate <- atomically $ do
                writeTChan reloadChan $ Left ()
                currentGroups <- readTVar groupsVar
                case lookup groupName currentGroups of
                    Nothing -> return (groupName /= "static")
                    Just _ -> return False
            when canCreate $ do
                createDirectory groupName
                createNewGroup groupName responsible
            return canCreate

        listGroups' = map fst `fmap` readTVarIO groupsVar

        getGroupValue' groupName =
            lookup groupName `fmap` readTVar groupsVar

    putStrLn "** Administration active"
    return $
        Administrator
            reloadCondition
            checkGroupName'
            checkPassword'
            bootNewGroup'
            listGroups'
            getGroupValue'
