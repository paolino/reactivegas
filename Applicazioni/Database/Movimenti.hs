module Applicazioni.Database.Movimenti (mkMovimenti, Movimenti (..)) where

import Control.Applicative ((<$>))
import System.FilePath ((</>))
import System.Directory (removeFile)
import Control.Exception (handle, SomeException (..))
import Database.HDBC 
import Database.HDBC.Sqlite3  (connectSqlite3)
import Data.Time

import Core.Types (Utente)
import Eventi.Accredito (Movimento (..))
import Lib.Missing (catchRead)

data Movimenti = Movimenti
	{	nuoviMovimenti :: Integer -> [Movimento] -> IO ()
	,	ultimiMovimentiConto :: Utente -> Int -> IO [Movimento]
	,	ultimiMovimentiCassa :: Utente -> Int -> IO [Movimento]
	}

movimentoRow d (MovimentoU u e s) = [toSql "conto", toSql u , toSql (show e), toSql (s ++ " (" ++ d ++ ")")]
movimentoRow d (MovimentoR u e s) = [toSql "cassa", toSql u , toSql (show e), toSql (s ++ " (" ++ d ++ ")")]

read' x = catchRead "on module Movimenti" x

rowMovimento [Just "conto",Just u,Just e, Just s] = MovimentoU u (read' e) s
rowMovimento [Just "cassa",Just u,Just e, Just s] = MovimentoR u (read' e) s

mkMovimenti 	:: FilePath 	-- ^ workin directory
		-> IO Movimenti
mkMovimenti wd  = do
	handle (\(SomeException _) -> return ()) (removeFile $ wd </> "movimenti.sql")
	let mdb = connectSqlite3 $ wd </> "movimenti.sql"
	db <- mdb
	handleSql (\_ -> return ()) $ do
		run db ("CREATE TABLE movimenti (id INTEGER Primary Key, aggiornamento , integer, tipo char(5)" ++
			", utente varchar(50), movimento varchar(50), descrizione varchar(50))") []
		commit db
	let 	iM db i = prepare db $ "INSERT INTO movimenti (aggiornamento,tipo,utente,movimento,descrizione) VALUES (" ++
			show i ++ ",?,?,?,?)" 
	 	uM db u n = prepare db $ "select tipo,utente,movimento,descrizione from movimenti where utente = '" ++ u 
			++ "' and tipo = 'conto'  order by id desc limit " ++ show n
		rM db u n = prepare db $ "select tipo,utente,movimento,descrizione from movimenti where utente = '" ++ u 
			++ "' and tipo = 'cassa'  order by id desc limit " ++ show n

	return $ Movimenti
		{	nuoviMovimenti = \ i ms -> do
				iS <- iM db i
				d <- (show . utctDay) `fmap` getCurrentTime
				executeMany iS . map (movimentoRow d) $ ms
				commit db
		,	ultimiMovimentiConto =  \u n -> do
				uS <- uM db u n
				execute uS [] 
				r <- map rowMovimento <$> sFetchAllRows' uS
				return r
		,	ultimiMovimentiCassa =  \u n -> do
				uS <- rM db u n
				execute uS [] 
				r <- map rowMovimento <$> sFetchAllRows' uS
				return r
		} 
	
