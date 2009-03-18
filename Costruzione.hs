{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}
module Costruzione where

import Prelude hiding (catch)
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Reader
import Text.ParserCombinators.ReadP
import Data.Char
import Eventi
import Componenti
import System.IO.Error
import Text.JSON
import Network.Shed.Httpd
import Debug.Trace
-- import Control.Exception

data Free = Intero | Stringa deriving Show
instance JSON Free where
	showJSON Intero = JSString $ toJSString "campo intero"
	showJSON Stringa = JSString $ toJSString "campo testuale"
	readJSON _ = error "parsing di un Free in JSON"
data Parziale a = Fine Logico | Continua [String] (Reader Estratto (Either Free [a])) 
instance Show (Parziale a) where
	show (Fine l) = show l
	show (Continua ss _) = "?" ++ show ss

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

newtype SString = SString {unSString :: [String]} deriving Show
instance Read SString where
	readsPrec _ s = readP_to_S (SString <$> sepBy (munch1 isAlphaNum) (char ',')) s

parse x = (\y -> trace (show y) y) . parse' . unSString .read $ (trace x x)

				
				


	
