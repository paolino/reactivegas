{-# LANGUAGE StandaloneDeriving #-}
import System.Random
import System.Console.GetOpt
import System.Environment
import System.Time
import Data.Maybe 
import Control.Arrow
import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either

import Codec.Crypto.RSA

import Componenti 
import Eventi
import Fields
import UI

------------------------------------------------
------------------------------------------------
type Signing  = Reader (Tempo,Utente)
contestualmente :: (Tempo,Utente) -> Signing a -> a
contestualmente = flip runReader

firmo :: Evento ->  Signing Evento
firmo ev  =  do	(t,Utente u pu (Just pr)) <- ask 
		let 	ev' = ev{tempo = t} 
			ev'' = ev'{responsabile = u}
		return $ ev''{firma = Firma (sign pr (image ev''))}


---- un set di creatori di eventi sotto l'ambiente reale, un utente e un tempo attuale.
uResponsabile m pk 	= firmo (Responsabile undefined undefined m pk undefined)
uMembro m 		= firmo (Membro undefined undefined m undefined)
uAccredito m v 		= firmo (Accredito undefined undefined m v undefined)
uSaldo m v 		= firmo (Saldo undefined undefined m v undefined)
uApertura o t 		= firmo (Apertura undefined undefined o t undefined)
uChiusura o 		= firmo (Chiusura undefined undefined o undefined)
uFallimento o 		= firmo (Fallimento undefined undefined o undefined)
uRichiesta m o v 	= firmo (Richiesta undefined undefined m o v undefined)
uBootstrap m pk 	= return $ Bootstrap m pk
----------------------------------------------------------------------------
-------------------------------------------------------------------------------------
deriving instance Read PrivateKey
loadPrivata :: String -> IO PrivateKey
loadPrivata s = read <$> readFile s

loadConoscenza :: String -> [Componente] -> IO Estratto
loadConoscenza s cs = (fst . valida (validaEvento cs) . map read . lines) <$> readFile s

loadTempo :: IO Tempo
loadTempo = fromClockTime <$> (getClockTime >>= toCalendarTime)
	
data Utente = Utente {user :: User, pu :: PublicKey, pr :: Maybe PrivateKey}

addEvent :: Estratto -> ReaderT (Tempo,Utente,[Componente]) (WriterT [Evento] IO) Estratto
addEvent est = do
	evf <- liftIO $ do 
		n <- untilIn "Nuovo evento" ["Accredito","Richiesta","Saldo","Apertura","Chiusura","Fallimento"]
		case n of 
			"Accredito" -> do
				m <- untilIn "Accredito per " (S.elems $ membri est)
				z <- untilParse $ "Accredito per " ++ show m ++ " di euro "
				return $ uAccredito m z
			"Richiesta" -> do
				m <- untilIn "Richiesta d'ordine per " (S.elems $ membri est)
				o <- untilIn ("Richiesta d'ordine per " ++ show m ++ " sul bene ") (M.keys $ aperti est)
				z <- untilParse $ "Richiesta d'ordine per " 
					++ show m ++ " sul bene " ++ show o ++ " del valore di "
				return $ uRichiesta m o z
			"Saldo" -> do
				r <- untilIn "Saldo ricevuto da " (M.keys $ responsabili est)
				v <- untilParse $ "Saldo ricevuto da " ++ show r ++ " di euro "
				return $ uSaldo r v
			"Membro" -> do 
				r <- untilParse $ "Nuovo membro di nome "
				return $ uMembro r
			"Responsabile" -> do
				r <- untilIn "Nuovo responsabile di nome " (M.keys $ responsabili est)
				pu <- untilParse $ "Nuovo responsabile di nome " ++ show r ++ " con chiave "
				return $ uResponsabile r pu
			"Apertura" -> do
				o <- untilNotIn "Nuovo ordine per il bene " (S.elems $ tuttibeni est)
				t <- untilParse $ "Nuovo ordine per il bene " ++ show o ++ " con data ultima "
				return $ uApertura o t
			"Chiusura" -> do
				o <- untilIn "Chiusura ordine per il bene " (M.keys $ aperti est)
				return $ uChiusura o
			"Fallimento" -> do
				o <- untilIn "Fallimento ordine per il bene " (M.keys $ aperti est)
				return $ uFallimento o

			_	-> error "non implementato"
			
	(t,u,cs) <- ask			
	let 	(est',rs) = runWriter $ validaEvento cs (runReader evf (t,u)) est  
	case rs of 
		[Left (e,t)] -> liftIO $ print t
		[Right e] -> tell [e]
	return est'


main = do
	t <- loadTempo
	pr <- loadPrivata "paolino.priv"
	let cs = [vTempoCorretto t, vResponsabile, vMembro, vSaldo]
	e <- loadConoscenza "prova.csv" cs
	runWriterT (runReaderT (addEvent e) (t,Utente (User "paolino") undefined (Just pr) ,cs))  >>= print
