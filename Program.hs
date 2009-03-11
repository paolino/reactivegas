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
import Data.Maybe

import Codec.Crypto.RSA

import Componenti (validaEvento)
import Eventi 
import Bugs
import UI
import Control.Parallel.Strategies


load :: Read a => String -> IO a
load s = read <$> (readFile s >>= \e -> rnf e `seq` return e)


loadEstratto :: String -> IO Estratto
loadEstratto s = load s 

newtype WEvento = WEvento String
instance Show WEvento where
	show (WEvento s) = s

addEvent :: Estratto -> ReaderT PublicKey IO (Estratto,Maybe (Evento))
addEvent est = do
	evf <- liftIO $ do 
		n <- untilIn "Nuovo evento" $ map WEvento
			["Accredito","Richiesta","Saldo","Novizio","Nick","Apertura","Chiusura","Fallimento"]
		case n of 
			WEvento "Accredito" -> do
				m <- untilIn "Accredito per " (S.elems $ membri est)
				z <- untilParse $ "Accredito per " ++ show m ++ " di euro "
				return $ Accredito m z
			WEvento "Richiesta" -> do
				m <- untilIn "Richiesta d'ordine per " (S.elems $ membri est)
				o <- untilIn ("Richiesta d'ordine per " ++ show m ++ " sul bene ") (M.keys $ aperti est)
				z <- untilParse $ "Richiesta d'ordine per " 
					++ show m ++ " sul bene " ++ show o ++ " del valore di "
				return $ Richiesta m o z
			WEvento "Saldo" -> do
				r <- untilIn "Saldo ricevuto da " (M.keys $ responsabili est)
				v <- untilParse $ "Saldo ricevuto da " ++ show r ++ " di euro "
				return $ Saldo r v
			WEvento "Novizio" -> do 
				r <- untilParse $ "Nuovo membro di nome "
				return $ Novizio r
			WEvento "Nick" -> do
				r <- untilParse $ "Il mio nome e' "
				return $ Nick r 
			WEvento "Apertura" -> do
				o <- untilNotIn "Nuovo ordine per il bene " (S.elems $ tuttibeni est)
				return $ Apertura o 
			WEvento "Chiusura" -> do
				o <- untilIn "Chiusura ordine per il bene " (M.keys $ aperti est)
				return $ Chiusura o
			WEvento "Fallimento" -> do
				o <- untilIn "Fallimento ordine per il bene " (M.keys $ aperti est)
				return $ Fallimento o

			_	-> error "non implementato"
	pu <- ask			
	let 	(est',rs) = runWriter $ validaEvento (pu,evf) est  
	case rs of 
		[Left (_,t)] -> liftIO $ print t >> return (est, Nothing)
		[Right e] -> return (est', Just e)

command xs est = do 	d <- liftIO $ untilIn "Comando" [WEvento "Fine", WEvento "Evento", WEvento "Resoconto", WEvento "Patch"]
			case d of 
				WEvento "Fine" -> return (xs,est)
				WEvento "Evento" -> do
					(est',x) <- addEvent est
					case x of 
						Just e -> command (x:xs) est'
						Nothing -> command xs est
				WEvento "Resoconto" -> liftIO (mapM_  print $ xs) >> command xs est


main = do
	--pr <- loadPrivata "paolino.priv"
	pu <- load "paolino.publ"
	est <- loadEstratto "estratto"
	runReaderT (command [] est) pu >>= writeFile "estratto" . show . snd
