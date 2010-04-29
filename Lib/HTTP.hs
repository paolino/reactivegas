{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving #-}
module Lib.HTTP where

import Control.Arrow ((***))
import Control.Applicative
import Text.XHtml
import Data.Maybe
import qualified Lib.Passo as P 
import Lib.Response
import Data.List
import Data.Typeable

import Network.URL
import Debug.Trace
import Codec.Binary.UTF8.String



mkLink :: String -> [(String,String)] -> String
mkLink x = exportURL . foldl (\s  -> add_param s ) (fromJust $ importURL x)

data Link = Link 	{nomelink :: String
			,valore :: String
			,mime :: String
			}

{-
internalmenu y z  = let 
	in	thediv ! [theclass "menu"] << ulist << (	
				li ! [theclass "menu"] 
					<< anchor ! [ theclass "menu", href $ mkLink "/menu" [("hkey",y),("fkey",z),("valore","clona")]]
					<< "clona" 
 				+++ li ! [theclass "menu"] 
					<< anchor ! [theclass "menu", href $ mkLink "/menu" [("hkey",y),("fkey",z),("valore","affonda")]] 
					<< "accoda"
				+++ li ! [theclass "menu"] 
					<< anchor ! [theclass "menu", href $ mkLink "/menu" [("hkey",y),("fkey",z),("valore","allarga")]]
					<< "allarga" 
				+++ li ! [theclass "menu"] 
					<< anchor ! [theclass "menu", href $ mkLink "/menu" [("hkey",y),("fkey",z),("valore","restringi")]]
					<< "restringi" 

				+++ li ! [theclass "menu"] 
					<< anchor ! [theclass "menu", href $ mkLink "/menu" [("hkey",y),("fkey",z),("valore","chiudi")]] 
					<< "chiudi" 
				)	 
-}
internalmenu _ _ = noHtml 
renderResponse k x = thediv ! [theclass k] << renderResponse' x where
	renderResponse' x@(ResponseOne y) =  case typeOf y == typeOf noHtml of
			False -> thediv << (show x)
			True -> thediv << (fromJust (cast y) :: Html)
	renderResponse' (ResponseMany xs) =  ulist ! [theclass k] << concatHtml (map  ((li ! [theclass k] <<) .   show ) xs) 
	renderResponse' (ResponseAL xs) =  dlist ! [theclass k]
			<< concatHtml (map  (\(x,y) -> dterm ! [theclass k] << x +++ ddef ! [theclass k] << ( show $ y)) xs) 
	renderResponse' (Response xs) =  dlist  ! [theclass k] << 
			concatHtml (map  (\(x,y) -> dterm ! [theclass k] << x +++ ddef ! [theclass k] << renderResponse' y) xs)

  
runPasso :: (Monad m ) 
	=> P.Passo m b 
	-> 	( String -> String ->  Html
		, Maybe Link
		, String -> Maybe (m (P.HPasso m b))
		)

runPasso (P.Output x c) = let
	k y z = thediv ! [theclass "passobox"] << 
			(	renderResponse "responso" x 
				+++ form ! [method "post", action "/interazione"] 
					<< [	hidden "hkey" y, hidden "valore" "undefined", 
						hidden "fkey" z, submit "" "Continua .." ! [theclass "continua"]]
			) +++ internalmenu y z
	in (k, Nothing,\_ -> Just c )

runPasso (P.Errore x c) = let
	k y z = thediv ! [theclass "passobox"] << 
			(	renderResponse "errore" x 
				+++ form ! [method "post", action ("/interazione")] 
					<< [	hidden "hkey" y, hidden "valore" "undefined", 
						hidden "fkey" z, submit "" "Continua .." ! [theclass "continua"]]
			) +++ internalmenu y z
	in (k, Nothing,\_ -> Just c )

runPasso (P.Costruito _) = runPasso . P.Errore 
	(ResponseOne "vicolo cieco dell'interfaccia utente") $ return (P.Costruito undefined,[])

runPasso (P.Libero q c ) = let 
	k y z = thediv ! [theclass "passobox"] << 
			(	thediv ! [theclass "response"] << q +++ 
				form ! [method "post", action ("/interazione")] 
					<< [	hidden "hkey" y, textfield "valore", 
						hidden "fkey" z, submit "" "Continua .." ! [theclass "continua"]]
			) +++ internalmenu  y z
	parse x = case reads x of
		[] -> case reads $ "\"" ++ x ++ "\"" of 
			[] -> Nothing 
			[(x',_)] -> Just $ c x'
		[(x',_)] -> Just $ c x'
	in (k, Nothing,parse)
runPasso (P.Scelta q xs c) = let 
	k y z =  thediv ! [theclass "passobox"] << 
		(thediv ! [theclass "response"] << q +++ 
				 ulist << (map (\(x,_) -> li ! [theclass "scelta"]
					<< anchor ! [theclass "passobox", 
						href $ mkLink "/interazione" [("hkey",y),("fkey",z),("valore",x)]] 
						<< x) xs ))  
			+++ internalmenu y z
	resp x = lookup x xs >>= return . c
	in (k, Nothing,resp)
	
runPasso (P.Upload q c ) = let
	k y z = thediv ! [theclass "passobox"] << 
		(thediv ! [theclass "response"] << q +++ 
			form ! [method "post", action "/interazione", enctype "multipart/form-data"] << 
				( 	[afile "valore", hidden "hkey" y,  
					hidden "fkey" z, submit "" "Continua .." ! [theclass "continua"]]
				) 
		) +++ internalmenu y z

	parse x = case reads x of
		[] -> Nothing  
		[(x',_)] -> Just $ c x'
	in (k, Nothing ,parse)


runPasso (P.Download q x c) = let
	k y z = thediv ! [theclass "passobox"] << 
		(thediv ! [theclass "download"] <<  
				form ! [method "post", action "/download"] 
					<< [	hidden "hkey" y,  hidden "fkey" z
						, hidden "valore" "undefined" , submit "" "Download .." ! [theclass "continua"]]
				+++
				form ! [method "post", action "/interazione"] 
					<< [	hidden "hkey" y,  hidden "fkey" z
						, hidden "valore" "undefined" , submit "" "Continua .." ! [theclass "continua"]]
		) +++ internalmenu y z

	in (k,Just $ Link q (show x) "application/chiavi",\_ -> Just c)
-------------------------------------------------------------------------------------------------------
radio'  = tag "option" 
--------------------------------------------------------------------------------------------------



