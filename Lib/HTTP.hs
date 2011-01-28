{-# LANGUAGE ScopedTypeVariables, ViewPatterns, StandaloneDeriving, DeriveDataTypeable #-}
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


deriving instance Typeable Html

mkLink :: String -> [(String,String)] -> String
mkLink x = exportURL . foldl (\s  -> add_param s ) (fromJust $ importURL x)

data Link = Link 	{nomelink :: String
			,valore :: String
			,mime :: String
			}
renderResponse k x = (thediv ! [theclass k] << renderResponse' x) where
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
	-> 	( String -> String -> Maybe String -> Maybe String -> Html
		, Maybe Link
		, String -> Maybe (m (P.HPasso m b))
		)

timeB s _ Nothing = noHtml
timeB s y (Just z) = anchor ! [theclass "back", href $ mkLink "/interazione" [("hkey",z),("fkey",y)]] << s
			
indietro = timeB "⇦"
avanti = timeB "⇨"

runPasso (P.Output x mc) = let
	k y z mb ma = thediv ! [theclass "passobox",strAttr "hkey" y, strAttr "fkey" z] << 
			(	avanti z mb +++ indietro z ma +++
				renderResponse "output" x 
					
				+++ form ! [theclass "quiet", method "post", action "/interazione"] 
					<< case mc of 
						Nothing -> []
						Just c -> [	hidden "hkey" y, hidden "valore" "undefined", 
							hidden "fkey" z, submit "" "Continua" ! [theclass "continua"]]
			) 
	in (k, Nothing,\_ -> mc )

runPasso (P.Errore x mc) = let
	k y z mb ma = thediv ! [theclass "passobox", strAttr "hkey" y, strAttr "fkey" z] << 
			(	indietro z mb +++ avanti z ma +++ renderResponse "errore" x 
				
				+++ form ! [theclass "quiet",method "post", action ("/interazione")] 
					<< case mc of 
						Nothing -> []
						Just c -> [	
							hidden "hkey" y, hidden "valore" "undefined", 
							hidden "fkey" z, submit "" "Continua" ! [theclass "continua"]
							]
			) 
	in (k, Nothing,\_ -> mc)

runPasso (P.Costruito _) = runPasso . P.Errore 
	(ResponseOne "vicolo cieco dell'interfaccia utente") $ Nothing

runPasso (P.Libero po q c) = let 
	k y z mb ma = thediv ! [theclass $ "passobox", strAttr "hkey" y, strAttr "fkey" z] << 
			(thediv ! [theclass "responso"] << indietro z mb +++ avanti z ma +++
				renderResponse "output" q +++ 
				
				form ! [theclass "quiet",method "post", action "/interazione", strAttr "accept-charset" "utf8"] 							<< [	hidden "hkey" y, textfield "valore", 
						hidden "fkey" z, submit "" "Inserisci" ! [theclass "continua"]]
			) 
	parse x = case reads (decodeString x) of
		[] -> case reads $ "\"" ++ decodeString x ++ "\"" of 
			[] -> Nothing 
			(last -> (x',_)) -> Just $ c x'
		(last -> (x',_)) -> Just $ c x'
	in (k, Nothing,parse)

runPasso (P.Password po q c ) = let 
	k y z mb ma = thediv ! [theclass $ "passobox" , strAttr "hkey" y, strAttr "fkey" z] << 
			(	thediv ! [theclass "responso"] <<indietro z mb +++ avanti z ma +++ q +++ 
				form ! [theclass "quiet", method "post", action "/interazione",
					strAttr "accept-charset" "utf8"] 
					<< [	hidden "hkey" y, password "valore", 
						hidden "fkey" z, submit "" "Inserisci" ! [theclass "continua"]]
			) 
	parse x = case reads x of
		[] -> case reads $ "\"" ++ x ++ "\"" of 
			[] -> Nothing 
			(last -> (x',_)) -> Just $ c x'
		(last -> (x',_)) -> Just $ c x'
	in (k, Nothing,parse)

runPasso (P.Scelta po q xs c) = let 
	k y z mb ma =  thediv ! [theclass $ "passobox" , strAttr "hkey" y, strAttr "fkey" z] << 
			(	thediv ! [theclass "responso"] << indietro z mb +++ avanti z ma +++ 
					renderResponse "output" q  +++
					  
					ulist << (map (\(x,_) -> li ! [theclass "scelta"]
					<< anchor ! [theclass "quietL", href $ mkLink "/interazione" [("hkey",y),("fkey",z),("valore",x)]] 
						<< x) xs )
			)  
	resp x = lookup x xs >>= return . c
	in (k, Nothing,resp)
	
runPasso (P.Upload po q c ) = let
	k y z mb ma = thediv ! [theclass $ "passobox" , strAttr "hkey" y, strAttr "fkey" z] << 
		(	thediv ! [theclass "responso"] <<indietro z mb +++ avanti z ma +++ q +++
			form ! [theclass "quiet", method "post", action "/interazione", enctype "multipart/form-data"] << 
				 	[afile "valore", hidden "hkey" y,  
					hidden "fkey" z, submit "" "Carica" ! [theclass "continua"]]
		) 

	parse x = case reads x of
		[] -> Nothing  
		[(x',_)] -> Just $ c x'
	in (k, Nothing ,parse)


runPasso (P.Download f q x c) = let
	k y z mb ma = thediv ! [theclass "passobox", strAttr "hkey" y, strAttr "fkey" z] << 
		(thediv ! [theclass "responso"] <<indietro z mb +++ avanti z ma +++ q +++
		(thediv ! [theclass "download"] <<  
				form ! [theclass "quiet", method "post", action "/download"] 
					<< [	hidden "hkey" y,  hidden "fkey" z
						, hidden "valore" "undefined" , submit "" "Scarica" ! [theclass "continua"]]
		)
		)

	in (k,Just $ Link f (show x) "application/any",\_ -> Just c)
-------------------------------------------------------------------------------------------------------
radio'  = tag "option" 
--------------------------------------------------------------------------------------------------



