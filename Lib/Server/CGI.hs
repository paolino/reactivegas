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
import Lib.Missing (onNothing)


import Debug.Trace

readHKey :: ErrorT String (CGIT IO) TimeKey
readHKey = do 
	r <- lift (getInput "hkey") >>= onNothing "manca la chiave di punto"
	case reads r of
		[(k,_)] -> return k
		_ -> throwError "campo hkey illeggibile"

readFKey :: ErrorT String (CGIT IO) FormKey
readFKey =  do 
	r <- lift (getInput "fkey") >>= onNothing "manca la chiave di form"
	case reads r of
		[(k,_)] -> return k
		_ -> throwError "campo fkey illeggibile"

readValore :: ErrorT String (CGIT IO) Value
readValore = fmap id <$> lift (getInput "valore") >>= onNothing "manca la risposta"

pagina :: [(Html,Int)] -> [Html]
pagina xs =  map (\(h,i) -> thediv ! [theclass ("boxes interazione dimensione" ++ show i)] << h) xs

-- | eleva gli errori nella monade del server in quella di CGI 
liftServer :: Error e => ErrorT e IO a -> ErrorT e (CGIT IO) a
liftServer ma = do
	r <- lift . lift $ runErrorT ma 
	case r of 	Left x -> throwError x	
			Right y -> return y

	

type HServer e = Server e Html Link

-- | map a Server reactor to a CGI action 
cgiFromServer :: ([Html] -> CGI CGIResult) -> (Server e Html Link,IO ()) -> CGI CGIResult
cgiFromServer resp (Server apertura servizio,droppa) = do 
	let s = liftServer . servizio 
	vs <- getVars
	-- lift $ print $ lookup "REQUEST_URI"  vs
	is <- getInputs

	--lift $ print is
	r <- runErrorT $ case lookup "REQUEST_URI"  vs of 
		Just x -> let xs =  tail $ splitOneOf "/?" x in
			case xs of
				[""] -> lift .  resp . pagina $ apertura

				("unaform":_) -> do 
					hk <- readHKey
					fk <- readFKey
					v <- readValore
					ehl <- 	s (hk,fk,ContinuaS v) `mplus` 
						s (hk,fk,ContinuaS $ decodeString v)	
					case ehl of
						Right hs -> lift . output . prettyHtml . fst . head $ hs
						Left _ -> throwError "unaform fallita"

				("ricarica":_) -> do 
					hk <- readHKey
					fk <- readFKey
					ehl <- 	s (hk,fk,RicaricaS)
					case ehl of
						Right hs -> lift . output . prettyHtml . fst . head $ hs
						Left _ -> throwError "ricarica fallita"
				("interazione":_) -> do 
					hk <- readHKey
					fk <- readFKey
					v <- readValore
					ehl <- 	s (hk,fk,ContinuaT v) `mplus` 
						s (hk,fk,ContinuaT $ decodeString v)	
					case ehl of
						Right hs -> lift . resp $ pagina hs
						Left _ -> throwError "l'interazione continua con un download"
				("download":_) -> do
					hk <- readHKey
					fk <- readFKey
					ec <- s (hk,fk,ScaricaD)
					case ec of
						Left (Link n x m) -> lift $ do
							setHeader "Content-type" m
							setHeader "Content-Disposition" "attachment"
							setHeader "Content-Disposition" $ "filename=" ++ show n
							output x 
						Right _ -> throwError "l'interazione continua con una interazione"
				_ -> lift (lift droppa) >> throwError "boh"
			
	either output return r


