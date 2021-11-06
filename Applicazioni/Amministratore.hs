{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Applicazioni.Amministratore where

import Control.Concurrent.STM
import Control.Monad (liftM2, when)
import Control.Monad.Trans (liftIO)
import Core.Types
import Data.Maybe (isNothing)
import Lib.STM (condSignalEq)
import Lib.Tokens
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , renameDirectory
  )
import System.FilePath
import System.FilePath.Find

type Gruppo = String

data Amministratore a = Amministratore
  { ammReloadCond :: IO (Gruppo -> IO Bool)
  , controlla_nome :: Gruppo -> IO Bool
  , controlla_password :: Token -> Bool
  , boot_nuovo_gruppo :: (Gruppo, Responsabile) -> IO Bool
  , elenco_gruppi :: IO [Gruppo]
  , valore_di_gruppo :: Gruppo -> STM (Maybe a)
  }

mkAmministratore
  :: Token -- ^ password di amministratore
  -> ((FilePath, Gruppo, Maybe Responsabile, STM ()) -> IO a) -- ^ creazione o lettura di gruppo
  -> FilePath -- ^ directory di lavoro
  -> IO (Amministratore a) -- ^ l'amministratore risultante
mkAmministratore pass readA dir = do
  -- analisi directories
  ms <-
    tail
      `fmap` find
        ((== 0) `fmap` depth)
        (liftM2 (&&) ((== Directory) `fmap` fileType) ((/= "static") `fmap` fileName))
        dir
  -- gruppi e valori
  let ns = map takeFileName ms
  tr <- atomically newTChan :: IO (TChan (Either () Gruppo))
  let pokeGruppo = writeTChan tr . Right
  gs <- zip ns `fmap` mapM readA (zipWith (\m n -> (m, n, Nothing, pokeGruppo n)) ms ns)

  putStrLn "** Recuperati i seguenti gruppi:"
  mapM_ (\(x, _) -> putStrLn $ "\t" ++ x) gs
  --------- condSignalEq :: TChan a -> IO ((a -> Bool) -> STM Bool)
  let trw _y (Left ()) = True
      trw y (Right x) = x == y
      amR = do
        t <- condSignalEq tr
        return $ \n -> atomically $ t (trw n)
  -- mappatura variabile
  pes <- newTVarIO gs
  let new g r0 = do
        pe <- readA (dir </> g, g, Just r0, pokeGruppo g)
        liftIO . atomically $ readTVar pes >>= writeTVar pes . ((g, pe) :)
      create g = do
        let dg = dir </> g
        t <- doesDirectoryExist dg
        when t $ renameDirectory dg (dg </> addExtension g "copia")
        createDirectoryIfMissing True (dg </> "static")
        putStrLn $ "created " ++ dg
      controlla_nome' x = (isNothing . lookup x) `fmap` readTVarIO pes
      controlla_password' = (== pass)
      boot_nuovo_gruppo' (g, r) = do
        t <- atomically $ do
          writeTChan tr $ Left ()
          gs' <- readTVar pes
          case lookup g gs' of
            Nothing -> return (g /= "static")
            Just _ -> return False
        when t $ do
          create g
          new g r
        return t
      elenco_gruppi' = map fst `fmap` readTVarIO pes
      valore_di_gruppo' g = lookup g `fmap` readTVar pes
  putStrLn "** Amministrazione attiva"
  return $ Amministratore amR controlla_nome' controlla_password' boot_nuovo_gruppo' elenco_gruppi' valore_di_gruppo'
