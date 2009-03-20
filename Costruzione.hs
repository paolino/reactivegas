{-# LANGUAGE NoMonomorphismRestriction, ExistentialQuantification, ScopedTypeVariables #-}
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
import Codec.Binary.UTF8.String
import Codec.Crypto.RSA
-- import Control.Exception
instance JSON Bene where
	readJSON (JSString s) = Ok (Bene $ fromJSString s)
	readJSON _ 		= Text.JSON.Error "failed parsing a Membro"
	showJSON (Bene s) = JSString $ toJSString (encodeString s)

instance JSON Membro where
	readJSON (JSString s) = Ok (Membro $ fromJSString s)
	readJSON _ 		= Text.JSON.Error "failed parsing a Membro"
	showJSON (Membro s) = JSString $ toJSString (encodeString s)
instance JSON PublicKey where
	readJSON _ = Error "no way to parse to a public key"
	showJSON = JSString . toJSString . take 6 . show . public_n

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
jcrr e w h = Just . Continua e $ w <$> ask >>= (return . Right . map Box . h)
jf = Just . Fine

data Box = forall a . JSON a => Box a
instance JSON Box where
	readJSON _ = Error "problem"
	showJSON (Box a) = showJSON a
parse' :: [String] -> Maybe (Parziale Box)
parse' e@["Apertura"] = jcrl e Stringa
parse' ["Apertura",s] = jf $ Apertura (Bene s)
parse' e@["Accredito"] = jcrr e membri S.toList
parse' e@["Accredito",m] = jcrl e Intero
parse' ["Accredito",m,v] = jf $ Accredito (Membro m) (read v)
parse' e@["Richiesta"] =  jcrr e membri S.toList
parse' e@["Richiesta",m] = jcrr e aperti M.keys
parse' e@["Richiesta",m,b] = jcrl e Intero
parse' ["Richiesta",m,b,v] = jf $ Richiesta (Membro m) (Bene b) (read v)
parse' e@["Chiusura"] = jcrr e aperti M.keys
parse' ["Chiusura",b] = jf $ Chiusura (Bene b)
parse' e@["Fallimento"] = jcrr e aperti M.keys
parse' ["Fallimento",b] = jf $ Fallimento (Bene b)
parse' e@["Novizio"] = jcrl e Stringa
parse' ["Novizio",n] = jf $ Novizio (Membro n)
parse' e@["Saldo"] = jcrr e  responsabili M.assocs
parse' e@["Saldo",r] = jcrl e Intero
parse' ["Saldo",r,v] = jf $ Saldo (read r) (read v)
parse' [] = Just . Continua [] . return . Right $ map Box ["Apertura","Accredito","Richiesta","Chiusura","Fallimento","Novizio","Saldo"]
parse' _ = Nothing

newtype SString = SString {unSString :: [String]} deriving Show
instance Read SString where
	readsPrec _ s = readP_to_S (SString <$> sepBy (munch1 (/= ',')) (char ',')) (s)

parse x = (\y -> trace ("parsed " ++ show y) y) . parse' . unSString .read $ (trace ("parsing " ++ x ++ " " ++ show (map ord x) ) x)

				
				


	
