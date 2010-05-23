module Applicazioni.Movimenti (mkMovimenti, Movimenti (..)) where

import Control.Applicative ((<$>))
import System.FilePath

import Database.HDBC 
import Database.HDBC.Sqlite3 
import Core.Types (Utente)
import Eventi.Accredito (Movimento (..))
import Lib.Euro

data Movimenti = Movimenti
	{	nuoviMovimenti :: Integer -> [Movimento] -> IO ()
	,	ultimiMovimentiConto :: Utente -> Int -> IO [Movimento]
	,	ultimiMovimentiCassa :: Utente -> Int -> IO [Movimento]
	}

movimentoRow (MovimentoU u e s) = [toSql "conto", toSql u , toSql (show e), toSql s]
movimentoRow (MovimentoR u e s) = [toSql "cassa", toSql u , toSql (show e), toSql s]

rowMovimento [Just "conto",Just u,Just e, Just s] = MovimentoU u (read e) s
rowMovimento [Just "cassa",Just u,Just e, Just s] = MovimentoR u (read e) s

mkMovimenti 	:: FilePath 	-- ^ workin directory
		-> Int 		-- ^ ultimo aggiornamento valido
		-> IO Movimenti
mkMovimenti wd v = do
	db <- connectSqlite3 $ wd </> "movimenti.db"
	handleSql (\_ -> return ()) $ do
		run db ("CREATE TABLE movimenti (id INTEGER Primary Key, aggiornamento , integer, tipo char(5)" ++
			", utente varchar(50), movimento varchar(50), descrizione varchar(50))") []
		commit db
	run db ("delete from movimenti where aggiornamento > " ++ show v) []
	let 	iM i = prepare db $ "INSERT INTO movimenti (aggiornamento,tipo,utente,movimento,descrizione) VALUES (" ++
			show i ++ ",?,?,?,?)" 
	 	uM u n = prepare db $ "select tipo,utente,movimento,descrizione from movimenti where utente = '" ++ u 
			++ "' and tipo = 'conto'  order by id desc limit " ++ show n
		rM u n = prepare db $ "select tipo,utente,movimento,descrizione from movimenti where utente = '" ++ u 
			++ "' and tipo = 'cassa'  order by id desc limit " ++ show n

	return $ Movimenti
		{	nuoviMovimenti = \ i ms -> do
				iS <- iM i
				executeMany iS . map movimentoRow $ ms
				commit db
		,	ultimiMovimentiConto =  \u n -> do
				uS <- uM u n
				execute uS [] 
				map rowMovimento <$> sFetchAllRows' uS
		,	ultimiMovimentiCassa =  \u n -> do
				uS <- rM u n
				execute uS [] 
				map rowMovimento <$> sFetchAllRows' uS
		} 
	
