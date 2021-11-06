module Applicazioni.Database.GPatch (GPatches (..), mkGPatches) where

import Control.Monad (forM_, when, void)
import Core.Patch
import Data.Function (on)
import Data.List (groupBy)
import Data.Maybe (fromJust)
import Database.HDBC
import Database.HDBC.Sqlite3
import Lib.Missing (catchRead)
import System.FilePath
import Data.Typeable (Typeable)

read' :: (Read a, Typeable a) => String -> a
read' = catchRead "on module GPatch"

data GPatches = GPatches
  { nuovaGPatch :: Integer -> Group -> IO ()
  , vecchiaGPatch :: Integer -> IO (Maybe Group)
  , abbandonaGPatch :: IO ()
  , ultimaGPatch :: IO Integer
  }

nuoveTabelle :: [[Char]]
nuoveTabelle =
  [ "create table gpatches (id integer primary key, firma varchar(1000))"
  , "create table patches (id integer primary key, gpatch integer , firma varchar(1000), "
      ++ " FOREIGN KEY(gpatch) REFERENCES gpatch(id) on delete cascade)"
  , "create table dichiarazioni (upatch integer, dichiarazione varchar(1000), "
      ++ " foreign key (upatch) references patches(id) on delete cascade)"
  ]

lastRow :: Connection -> String -> String -> IO Integer
lastRow db col table = do
  stmt <- prepare db $ "select " ++ col ++ " from " ++ table ++ " order by id desc limit 1"
  void $ execute stmt []
  o <- sFetchAllRows' stmt
  return $ case o of
    [] -> 0
    [[Just pis]] -> read' pis
    _ -> error ""

insertUPatch :: Connection -> Integer -> Patch -> IO ()
insertUPatch db gi (c, f, es) = do
  void $ run db "insert into patches (gpatch,firma) values (?,?)" [toSql gi, toSql (show (c, f))]
  commit db
  pis <- lastRow db "id" "patches"
  stmt <- prepare db "insert into dichiarazioni (upatch,dichiarazione) values (?,?)"
  executeMany stmt $ map (\e -> [toSql pis, toSql e]) es
  commit db

insertGPatch :: Connection -> Integer -> Group -> IO ()
insertGPatch db i (c, f, us) = do
  pis <- lastRow db "id" "gpatches"

  when (pis == (i - 1)) $ do
    void $ run db "insert into gpatches (firma) values (?)" [toSql (show (c, f))]
    commit db
    pis' <- lastRow db "id" "gpatches"
    forM_ us $ insertUPatch db pis'

getGPatch :: Connection -> Integer -> IO (Maybe Group)
getGPatch db i = do
  stmt <-
    prepare db $
      "select gpatches.firma, patches.firma, dichiarazione from gpatches, patches, "
        ++ "dichiarazioni  where gpatches.id = patches.gpatch and patches.id = dichiarazioni.upatch and gpatches.id = "
        ++ show i
  void $ execute stmt []
  xs <- sFetchAllRows' stmt
  if null xs
    then return Nothing
    else do
      let (cg, fg) = read' . fromJust . head $ head xs
          xs' = map tail xs
          xs'' = groupBy ((==) `on` head) xs'
          ys =
            map
              (\x'' ->
                 let (cu, fu) = read' . fromJust . head $ head x''
                     ds = map (fromJust . head . tail) x''
                  in (cu, fu, ds))
              xs''
      return $ Just (cg, fg, ys)

dropGPatch :: Connection -> IO ()
dropGPatch db = do
  pis <- lastRow db "id" "gpatches"
  stmt <-
    prepare db $
      "delete from gpatches, patches, dichiarazioni where upatch = patches.id and "
        ++ "gpatch = gpatches.id and gpatches.id = "
        ++ show pis
  void $ execute stmt []
  commit db

mkGPatches :: FilePath -> IO GPatches
mkGPatches wd = do
  let mdb = connectSqlite3 $ wd </> "aggiornamenti.sql"
  db <- mdb
  handleSql (\_ -> return ()) $ do
    mapM_ (flip (run db) []) nuoveTabelle
    commit db
  --disconnect db
  return $
    GPatches
      { nuovaGPatch = insertGPatch db
      , vecchiaGPatch = \n -> getGPatch db (n + 1)
      , abbandonaGPatch = dropGPatch db
      , ultimaGPatch = lastRow db "id" "gpatches"
      }
