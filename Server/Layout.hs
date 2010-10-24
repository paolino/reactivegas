-- | impostazioni della pagina di interazione http
module Server.Layout (pagina, layout) where

import Text.XHtml

paypalV = "-----BEGIN PKCS7-----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-----END PKCS7-----"
-- | le cinque finestre , path e nome
layout :: [([String],Int)]
layout = 	[(["gestione dichiarazioni"],2)
		,(["descrizione della sessione"],1)
		,(["amministrazione"],4)
		,(["interrogazione sullo stato del gruppo"],5)
		,(["effetto delle ultime dichiarazioni"],3)
		,(["gruppo di acquisto"],6)
		,(["responsabile autore"],7)
		]


metadata = header << 	(
				(thelink ! [rel "stylesheet", href "/static/style.css", thetype "text/css"] << noHtml)
			+++ 	(thelink ! [rel "icon", href "/static/favicon.ico"] << noHtml)
			+++ 	(thetitle << "Amministrazione G.A.S.") 
			+++ 	(meta ! [httpequiv "Content-Type", content "text/html;charset=utf8;"])
			)  
piede = ulist << 	[
			li << ("Documentazione: " +++ anchor ! 
				[href "http://github.com/paolino/reactivegas/wiki"] << "github"),
			li << ("Codice sorgente: " +++ anchor ! 
				[href "http://github.com/paolino/reactivegas"] << "github"),
			li << ("Donazioni per lo sviluppo: " +++ form ! [
				action "https://www.paypal.com/cgi-bin/webscr",
				method "post"
				] << 	[	hidden "cmd" "_s-xclick",
						hidden "encrypted" paypalV,
						input ! [name "submit" , src "/static/donazioni.gif" ,
							thetype "image",theclass "paypal", alt "paypal"]
						])
			]  

testata = 	thediv ! [theclass "titolo"] << ( 
	thediv ! [theclass "project"] << "ReactiveGAS" +++
	thediv ! [theclass "synopsis"] << "Cooperazione economica nei gruppi d'acquisto")
 
	+++	thediv ! [theclass "abort"] << thediv ! [theclass "reset"] << anchor ! [href "/"] << "annullamento dell'interazione"

pagina 	:: Html 	-- ^ corpo della pagina
	-> String	-- ^ pagina servita
pagina b = prettyHtml $ 
		header << metadata  
	+++ 	body << (		(thediv ! [theclass "testata"] << testata)
				+++ 	(thediv ! [theclass "utente"] << b)
				+++ 	(thediv ! [theclass "pedata"] << piede)  
			)

	

