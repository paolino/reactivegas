{-# LANGUAGE ViewPatterns #-}

module Lib.Server.CGI (cgiFromServer) where

import Control.Applicative ((<$>))

import Data.List.Split (splitOneOf)
import Data.Maybe (listToMaybe)
import qualified Data.ByteString.Lazy.Char8 as B (readFile)
import Control.Monad.Error (mplus,Error , ErrorT,runErrorT,throwError,lift)
import Network.SCGI
import Codec.Binary.UTF8.String (decodeString)
import Lib.Server.Core
import Text.XHtml
import Lib.HTTP


import Debug.Trace

readHKey :: ErrorT String (CGIT IO) Enk
readHKey = lift (getInput "hkey") >>= onNothing "form corrotta, manca la chiave di punto"

readFKey :: ErrorT String (CGIT IO) Fok
readFKey =  do 
	fks <- lift (getInput "fkey") >>= onNothing "form corrotta, manca la chiave di interazione"
	(i,_) <- onNothing "form corrotta, la chiave di form non è un numero" . listToMaybe $ reads fks 
	return i

readValore :: ErrorT String (CGIT IO) Value
readValore = fmap id <$> lift (getInput "valore") >>= onNothing "form corrotta, manca la risposta"

pagina :: [(Html,Int)] -> Html
pagina xs =  thediv << map (\(h,i) -> thediv ! [theclass ("dimensione" ++ show i)] << h) xs

-- | eleva gli errori nella monade del server in quella di CGI 
liftServer :: Error e => ErrorT e IO a -> ErrorT e (CGIT IO) a
liftServer ma = do
	r <- lift . lift $ runErrorT ma 
	case r of 	Left x -> throwError x	
			Right y -> return y

	

type HServer e = Server e Html Link

-- | map a Server reactor to a CGI action 
cgiFromServer :: (Html -> CGI CGIResult) -> (Server e Html Link,IO ()) -> CGI CGIResult
cgiFromServer resp ((fsM,(liftServer .) -> s),droppa) = do 
	vs <- getVars 
	is <- getInputs
	r <- runErrorT $ case lookup "REQUEST_URI"  vs of 
		Just x -> let xs =  tail $ splitOneOf "/?" x in
			case xs of
				[""] -> lift $ lift fsM >>= resp . pagina 
				["style.css"] -> do
					css <- liftIO $ readFile "style.css"   
					lift $ do 	setHeader "Content-type" "text/css"
							output css
				["favicon.ico"] -> do
					css <- liftIO $ B.readFile "favicon.ico"   
					lift $ do 	setHeader "Content-type" "image/ico"
							outputFPS css

				("interazione":_) -> do 
					hk <- readHKey
					fk <- readFKey
					v <- readValore
					ehl <- 	s (hk,fk,Continua v) `mplus` 
						s (hk,fk,Continua $ decodeString v)	
					case ehl of
						Right hs -> lift . resp $ pagina hs
						Left _ -> throwError "l'interazione continua con un download"
				("download":_) -> do
					hk <- readHKey
					fk <- readFKey
					ec <- s (hk,fk,Scarica)
					case ec of
						Left (Link n x m) -> lift $ do
							setHeader "Content-type" m
							setHeader "Content-Disposition" "attachment"
							setHeader "Content-Disposition" $ "filename=" ++ show n
							output x 
						Right _ -> throwError "l'interazione continua con una interazione"
				("menu":_) -> do
					hk <- readHKey
					fk <- readFKey
					v <- readValore
					ehl <- s (hk,fk,case v of 
						"chiudi" -> Chiudi
						"clona" -> Clona
						"affonda" -> Affonda
						"allarga" -> Allarga
						"restringi" -> Restringi
						_ -> Chiudi)
					case ehl of
						Right hs -> lift . resp . pagina $ hs
						Left _ -> throwError "l'interazione continua con un download"
				("reset":_) -> lift $ lift fsM >>= resp . pagina
					
				_ -> lift (lift droppa) >> throwError "boh"
			
	either (\s -> outputNotFound s) return r


