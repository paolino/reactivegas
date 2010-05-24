
module Applicazioni.Database.GPatch (GPatches (..),mkGPatches) where
import Data.Maybe (fromJust)
import Data.List (groupBy)
import Data.Function (on)
import Control.Applicative ((<$>))
import Control.Monad (forM_, when)
import System.FilePath
import Debug.Trace

import Database.HDBC 
import Database.HDBC.Sqlite3 
import Core.Types (Utente)
import Core.Patch 
import Lib.Euro


data GPatches = GPatches
	{	nuovaGPatch :: Integer -> Group -> IO ()
	,	vecchiaGPatch :: Integer -> IO (Maybe Group)
	}

nuoveTabelle = 
	[	"create table dichiarazioni (upatch integer, dichiarazione varchar(1000))"
	,	"create table patches (id integer primary key, gpatch integer, firma varchar(1000))"
	,	"create table gpatches (id integer primary key, firma varchar(1000))"
	]

lastRow :: Connection -> String -> String -> IO (Integer)
lastRow db col table = do
	stmt <- prepare db $ "select " ++ col ++ " from " ++ table ++ " order by id desc limit 1"
	execute stmt []
	o <- sFetchAllRows' stmt
	return $ case o of
		[] -> 0
		[[Just pis]] -> read pis

insertUPatch :: Connection -> Integer -> Patch -> IO ()
insertUPatch db gi (c,f,es) = do
	run db ("insert into patches (gpatch,firma) values (?,?)") [toSql gi, toSql (show (c,f))]
	commit db
	pis <- lastRow db "id" "patches"
	stmt <- prepare db $ "insert into dichiarazioni (upatch,dichiarazione) values (?,?)"
	executeMany stmt $ map (\e -> [toSql pis, toSql e]) es
	commit db

insertGPatch :: Connection -> Integer -> Group -> IO ()
insertGPatch db i (c,f,us) = do
	pis <- lastRow db "id" "gpatches"
	
	when (pis == (i - 1)) $ do 
		run db ("insert into gpatches (firma) values (?)") [toSql (show (c,f))]
		commit db
		pis <- lastRow db "id" "gpatches"
		forM_ us $ insertUPatch db pis


getGPatch :: Connection -> Integer -> IO (Maybe Group)
getGPatch db i = do
	stmt <- prepare db $ "select gpatches.firma, patches.firma, dichiarazione from gpatches, patches, " ++
		"dichiarazioni  where gpatches.id = patches.gpatch and patches.id = dichiarazioni.upatch and gpatches.id = " 
		++ show i
	execute stmt []
	xs <- sFetchAllRows' stmt
	if null xs then return Nothing else do  
		let 	(cg,fg) = read . fromJust . head $ head $ xs
			xs' = map tail xs
			xs'' = groupBy ((==) `on` head) xs'
			ys = map (\x'' -> let 	(cu,fu) =   read . fromJust . head $ head $ x''
						ds = map (fromJust . head . tail) x''
						in (cu,fu,ds)) xs''
		return $ Just (cg,fg,ys)

 
mkGPatches :: FilePath -> IO GPatches
mkGPatches wd = do
	db <- connectSqlite3 $ wd </> "aggiornamenti.sql"
	handleSql (\_ -> return ()) $ do
		mapM (flip (run db) []) $ nuoveTabelle 
		commit db
	return $ GPatches {
		nuovaGPatch = insertGPatch db,
		vecchiaGPatch = getGPatch db . (+1)
		}
	

