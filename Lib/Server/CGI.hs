{-# LANGUAGE ViewPatterns #-}

module Lib.Server.CGI (cgiFromServer) where

import Data.Maybe (listToMaybe)

import Control.Monad.Error (Error , ErrorT,runErrorT,throwError,lift)
import Network.SCGI
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
readValore = lift (getInput "valore") >>= onNothing "form corrotta, manca la risposta"

pagina :: [Html] -> CGI CGIResult
pagina xs = output . prettyHtml $  
		header << (thelink ! [rel "stylesheet", href "/style.css", thetype "text/css"] << noHtml 
			-- +++ script ! [strAttr "language" "Javascript", src "/function.js"] << noHtml
				)
				+++ (body {-! [strAttr "onload" "rewrite()"]-} << 
					map (\h -> thediv ! [theclass "interazione"] << h) xs)

-- | eleva gli errori nella monade del server in quella di CGI 
liftServer :: Error e => ErrorT e IO a -> ErrorT e (CGIT IO) a
liftServer ma = do
	r <- lift . lift $ runErrorT ma 
	case r of 	Left x -> throwError x	
			Right y -> return y

-- | map a Server reactor to a CGI action 
cgiFromServer :: Server e Html Link -> CGI CGIResult
cgiFromServer (f,(liftServer .) -> s) = do 
	vs <- getVars 
	is <- getInputs
	r <- runErrorT $ do
		case lookup "REQUEST_URI"  vs of 
			Just "/style.css" -> do
				css <- liftIO $ readFile "style.css"   
				lift $ do 	setHeader "Content-type" "text/css"
				 		output css
			Just "/function.js" -> do
				js <- liftIO $ readFile "function.js"   
				lift $ do 	setHeader "Content-type" "text/js"
				 		output js
			Just "/interazione" -> do 
				hk <- readHKey
				fk <- readFKey
				v <- readValore
				ehl <- s (hk,fk,Continua v)
				case ehl of
					Right hs -> lift $ pagina hs
					Left _ -> throwError "l'interazione continua con un download"
			Just "/download" -> do
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
			Just "/menu" -> do
				hk <- readHKey
				fk <- readFKey
				v <- readValore
				ehl <- s (hk,fk,case v of 
					"chiudi" -> Chiudi
					"clona" -> Clona
					"inchioda" -> Inchioda)
				case ehl of
					Right hs -> lift $ pagina $ hs
					Left _ -> throwError "l'interazione continua con un download"
			Just "/" -> lift $ pagina [f]
			_ -> throwError "richiesta non intercettata"
	either (\s -> outputNotFound s) return r

