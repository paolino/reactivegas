{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}
module Costruzione where

import Prelude hiding (catch)
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Reader
import Eventi
import Componenti
import System.IO.Error
import Text.JSON
import Network.Shed.Httpd
-- import Control.Exception

data Free = Intero | Stringa deriving Show
instance JSON Free where
	showJSON Intero = JSString $ toJSString "campo intero"
	showJSON Stringa = JSString $ toJSString "campo testuale"
	readJSON _ = error "parsing di un Free in JSON"
data Parziale a = Fine Logico | Continua [String] (Reader Estratto (Either Free [a])) 


jcrl e = Just . Continua e . return . Left
jcrr e w h = Just . Continua e $ w <$> ask >>= (return . Right . map show . h)
jf = Just . Fine
parse' :: [String] -> Maybe (Parziale String)
parse' e@["Apertura"] = jcrl e Stringa
parse' ["Apertura",s] = jf $ Apertura (read s)
parse' e@["Accredito"] = jcrr e membri S.toList
parse' e@["Accredito",m] = jcrl e Intero
parse' ["Accredito",m,v] = jf $ Accredito (read m) (read v)
parse' e@["Richiesta"] =  jcrr e membri S.toList
parse' e@["Richiesta",m] = jcrr e aperti M.keys
parse' e@["Richiesta",m,b] = jcrl e Intero
parse' ["Richiesta",m,b,v] = jf $ Richiesta (read m) (read b) (read v)
parse' e@["Chiusura"] = jcrr e aperti M.keys
parse' ["Chiusura",b] = jf $ Chiusura (read b)
parse' e@["Fallimento"] = jcrr e aperti M.keys
parse' ["Fallimento",b] = jf $ Fallimento (read b)
parse' e@["Novizio"] = jcrl e Stringa
parse' ["Novizio",n] = jf $ Novizio (read n)
parse' e@["Saldo"] = jcrr e  responsabili M.assocs
parse' e@["Saldo",r] = jcrl e Intero
parse' ["Saldo",r,v] = jf $ Saldo (read r) (read v)
parse' [] = Just . Continua [] . return . Right $ ["Apertura","Accredito","Richiesta","Chiusura","Fallimento","Novizio","Saldo"]
parse' _ = Nothing
parse x = parse' . read $ "[" ++ x ++ "]"


provaValida xs = do 
	e <- read <$> readFile "estratto" :: IO Estratto
	pu <- read <$> readFile "paolino.publ"
	let 	r xs e = case parse xs of 
			Nothing -> return .  Left $ "parsing fallito dell'evento " ++ show xs
			Just (Fine ev) -> case valido e (pu,ev) of 
				Left err -> print err >> r [] e 
				Right e' -> r [] e' 
			Just (Continua ss f) -> do
				print ss
				print $ runReader f e
				let t = do 	l <- getLine 
						r (xs ++ [l]) e
				t  `catch` (\(err::IOError) -> return $ if isEOFError err then Right e else error (show err)) 
	r [] e
				
http :: Reuie -> IO Response
http xs = do
	e <- read <$> readFile "estratto" :: IO Estratto
	evs <- read <$> readFile "logici" :: IO [Logico]
	pu <- read <$> readFile "utente.publ"
	case parse xs of 
		Nothing -> return (Response 404  [] (encode $ "parsing fallito dell'evento " ++ show xs),e) 
		Just (Fine ev) -> case valido e (pu,ev) of 
			Left err -> return (Response 404  [] (encode err), e) 
			Right e' -> return (Response 200 [] (encode "evento creato"),e')
		Just (Continua ss f) -> return (Response 200 [] (encode $ runReader f e),e)
				


	
