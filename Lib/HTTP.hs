{-# LANGUAGE ScopedTypeVariables #-}
module Lib.HTTP where

import Control.Applicative
import Text.XHtml
import qualified Lib.Passo as P 
import Lib.Response
import Data.List
import Debug.Trace

data Link = Link 	{nomelink :: String
			,valore :: String
			,mime :: String
			}


internalmenu y z  =  	thediv ! [theclass "menu"] << (form ! [identifier $ "b" ++ tail y, method "post", action "/menu"] << 
				([hidden "hkey" y,hidden "fkey" z] +++ 
					( map (\x -> tag "input" ! [thetype "radio", theclass "menu" , name "valore" , value x] << x) ["clona","chiudi","inchioda"])
					+++ submit "" "Aspetto"))
					 

renderResponse k x = thediv ! [theclass k] << renderResponse' x
renderResponse' x@(ResponseOne _) =  thediv << show x
renderResponse' (ResponseMany xs) =  ulist << concatHtml (map  ((li <<) .  show ) xs) 
renderResponse' (ResponseAL xs) =  dlist 
		<< concatHtml (map  (\(x,y) -> dterm << x +++ ddef << show y) xs) 
renderResponse' (Response xs) =  dlist << 
		concatHtml (map  (\(x,y) -> dterm << x +++ ddef << renderResponse' y) xs)

  
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

runPasso (P.Costruito x) =
	(\_ _ -> thediv ! [theclass "passobox"] << 
			(	thediv ! [theclass "response"] 
				<< (anchor ! [href "/interazione"] << "riparti")
			)
		, 	Nothing
		,	\_ -> Just $ return (P.Costruito x,[])
		) -- brutta roba, infatti non va!

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
			form ! [identifier $ "a" ++ tail y
				, method "post"
				, action "/interazione"
				]<< (( map (\(x,_) -> tag "input" ! [theclass "passobox",thetype "radio", value x, name "valore"] << x +++ br) xs) +++  
					[	hidden "hkey" y,  
						hidden "fkey" z, submit "" "Continua .." ! [theclass "continua"]
					]) 
		) +++ internalmenu y z
	resp x = trace x $ lookup x xs >>= return . c
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



