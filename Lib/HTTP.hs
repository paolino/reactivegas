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

renderResponse k x@(ResponseOne _) =   thediv ! [theclass k] << show x
renderResponse k (ResponseMany xs) = thediv ! [theclass k] <<  ulist << concatHtml (map  ((li <<) .  show ) xs) 
renderResponse k (ResponseAL xs) = thediv ! [theclass k] << dlist 
		<< concatHtml (map  (\(x,y) -> dterm << x +++ ddef << show y) xs) 
renderResponse k (Response xs) = thediv ! [theclass k] << dlist << 
		concatHtml (map  (\(x,y) -> dterm << x +++ ddef << renderResponse k y) xs)

  
runPasso :: (Monad m , Show b) 
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
						hidden "fkey" z, submit "" "Continua .."]
			)
	in (k, Nothing,\_ -> Just c )

runPasso (P.Errore x c) = let
	k y z = thediv ! [theclass "passobox"] << 
			(	renderResponse "errore" x 
				+++ form ! [method "post", action ("/interazione")] 
					<< [	hidden "hkey" y, hidden "valore" "undefined", 
						hidden "fkey" z, submit "" "Continua .."]
			)
	in (k, Nothing,\_ -> Just c )

runPasso (P.Costruito x) =
	(\_ _ -> thediv ! [theclass "passobox"] << 
			(	thediv ! [theclass "response"] 
				<< (show x +++ anchor ! [href "/interazione"] << "riparti")
			)
		, 	Nothing
		,	\_ -> Just $ return (P.Costruito x,0)
		) -- brutta roba

runPasso (P.Libero q c ) = let 
	k y z = thediv ! [theclass "passobox"] << 
			(	thediv ! [theclass "response"] << q +++ 
				form ! [method "post", action ("/interazione")] 
					<< [	hidden "hkey" y, textfield "valore", 
						hidden "fkey" z, submit "" "Continua .."]
			)
	parse x = case reads x of
		[] -> case reads $ "\"" ++ x ++ "\"" of 
			[] -> Nothing 
			[(x',_)] -> Just $ c x'
		[(x',_)] -> Just $ c x'
	in (k, Nothing,parse)
runPasso (P.Scelta q xs c) = let 
	k y z = thediv ! [theclass "passobox"] << 
		(thediv ! [theclass "response"] << q +++ 
			form ! [method "post", action ("/interazione")] 
					<< (( map (\(x,_) -> radio' "valore" x << x +++ br) xs) +++  
					[	hidden "hkey" y,  
						hidden "fkey" z, submit "" "Continua .."
					])
		)
	resp x = trace x $ lookup x xs >>= return . c
	in (k, Nothing,resp)
	
runPasso (P.Upload q c ) = let
	k y z = thediv ! [theclass "passobox"] << 
		(thediv ! [theclass "response"] << q +++ 
			form ! [method "post", action "/interazione", enctype "multipart/form-data"] << 
				( 	[afile "valore", hidden "hkey" y,  
					hidden "fkey" z, submit "" "Continua .."]
				) 
		)
	parse x = case reads x of
		[] -> Nothing  
		[(x',_)] -> Just $ c x'
	in (k, Nothing ,parse)


runPasso (P.Download q x c) = let
	k y z = thediv ! [theclass "passobox"] << 
		(thediv ! [theclass "download"] <<  
				form ! [method "post", action "/download"] 
					<< [	hidden "hkey" y,  hidden "fkey" z
						, hidden "valore" "undefined" , submit "" "Download .."]
				+++
				form ! [method "post", action "/interazione"] 
					<< [	hidden "hkey" y,  hidden "fkey" z
						, hidden "valore" "undefined" , submit "" "Continua .."]
		) 
	in (k,Just $ Link q (show x) "application/chiavi",\_ -> Just c)
-------------------------------------------------------------------------------------------------------
radio' n v = tag "input" ! [thetype "radio",value v,identifier n,name n]
--------------------------------------------------------------------------------------------------



