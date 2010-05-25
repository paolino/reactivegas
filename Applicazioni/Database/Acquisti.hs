
module Applicazioni.Database.Acquisti (mkAcquisti, Acquisti (..)) where

import Control.Applicative ((<$>))
import System.FilePath ((</>))
import System.Directory (removeFile)
import Control.Exception (handle, SomeException (..))
import Data.Maybe (fromJust)
import Data.List (groupBy)
import Data.Function (on)
import Control.Monad (forM_, when)
import Debug.Trace

import Database.HDBC 
import Database.HDBC.Sqlite3 
import Core.Types (Utente)
import Eventi.Acquisto (FineAcquisto (..))
import Lib.Euro

data Acquisti = Acquisti
	{	nuovoAcquisto :: FineAcquisto -> IO ()
	,	ultimiAcquisti :: Int -> IO [FineAcquisto]
	}

nuoveTabelle = 
	[	"create table impegni (acquisto integer, utente varchar(100), impegno varchar(100))"
	,	"create table acquisti (id integer primary key, nome varchar(100))"
	]

lastRow :: Connection -> String -> String -> IO (Integer)
lastRow db col table = do
	stmt <- prepare db $ "select " ++ col ++ " from " ++ table ++ " order by id desc limit 1"
	execute stmt []
	o <- sFetchAllRows' stmt
	return $ case o of
		[] -> 0
		[[Just pis]] -> read pis

insertAcquisto :: Connection -> FineAcquisto -> IO ()
insertAcquisto db (FineAcquisto b us) = do
	run db ("insert into acquisti (nome) values (?)") [toSql b]
	pis <- lastRow db "id" "acquisti"
	forM_ us $ \(u,v) -> do
		run db ("insert into impegni (acquisto,utente,impegno) values (?,?,?)") [toSql pis, toSql u, toSql $ show v]
	commit db

getAcquisti :: Connection -> Int -> IO [FineAcquisto]
getAcquisti db n = do
	pis <- lastRow db "id" "acquisti"
	stmt <- prepare db ("select nome,utente,impegno from acquisti,impegni where acquisto = id and id > " ++
		show (pis - fromIntegral n))
	execute stmt []
	xs <- sFetchAllRows' stmt
	let 	ys = groupBy ((==) `on` head) xs
		ys' = map (\y -> let 	nome =  fromJust . head $ head $ y
					uvs = map (map fromJust . tail) y
					in FineAcquisto nome (map (\[u,v] -> (u,read v)) uvs) 
				) ys
	return ys'

mkAcquisti 	:: FilePath 	-- ^ workin directory
		-> IO Acquisti
mkAcquisti wd  = do
	handle (\(SomeException _) -> return ()) (removeFile $ wd </> "acquisti.sql")
	db <- connectSqlite3 $ wd </> "acquisti.sql"
	handleSql (\_ -> return ()) $ do
		mapM (flip (run db) []) $ nuoveTabelle 
		commit db
	return $ Acquisti {
		nuovoAcquisto = insertAcquisto db,
		ultimiAcquisti = getAcquisti db 
		}
	
