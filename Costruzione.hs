{-# LANGUAGE NoMonomorphismRestriction, ExistentialQuantification, ScopedTypeVariables #-}
module Costruzione (parse, Parziale (..)) where

import Control.Monad.Reader (Reader,ask)
import Control.Applicative ((<$>))
import qualified Data.Map as M (keys,assocs)
import qualified Data.Set as S (toList)
import Text.ParserCombinators.ReadP (readP_to_S,sepBy,munch1,char)

import Codec.Binary.UTF8.String (encodeString)
import Codec.Crypto.RSA (PublicKey, public_n)

import Text.JSON.Types -- (JSString (..))
import Text.JSON (JSON (..), toJSString)

import Eventi
import Componenti

instance JSON Bene where
	readJSON = undefined
	showJSON (Bene s) = JSString $ toJSString (encodeString s)

instance JSON Membro where
	readJSON = undefined
	showJSON (Membro s) = JSString $ toJSString (encodeString s)

instance JSON PublicKey where
	readJSON = undefined
	showJSON = JSString . toJSString . take 6 . show . public_n

data Free = Intero | Stringa deriving Show

instance JSON Free where
	readJSON = undefined
	showJSON Intero = JSString $ toJSString "campo intero"
	showJSON Stringa = JSString $ toJSString "campo testuale"

data Box = forall a . JSON a => Box a
instance JSON Box where
	readJSON = undefined
	showJSON (Box a) = showJSON a

data Parziale = Fine Logico | Continua [String] (Reader Estratto (Either Free [Box])) 

parse :: String -> Maybe Parziale
parse  = parse' . fst . last . readP_to_S (sepBy (munch1 $ (/=) ',') $ char ',') where
	jcrl e = Just . Continua e . return . Left
	jcrr e w h = Just . Continua e $ w <$> ask >>= (return . Right . map Box . h)
	jf = Just . Fine
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
	parse' e@["Nick"] = jcrl e Stringa
	parse' ["Nick", n] = jf $ Nick (Membro n)
	parse' [] = Just . Continua [] . return . Right 
		$ map Box ["Apertura","Accredito","Richiesta","Chiusura","Fallimento","Novizio","Nick","Saldo"]
	parse' _ = Nothing


				
				


	
