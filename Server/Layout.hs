-- | impostazioni della pagina di interazione http
module Server.Layout (pagina, layout) where

import Text.XHtml

-- | le cinque finestre , path e nome
layout :: [([String],Int)]
layout = 	[(["gestione dichiarazioni"],2)
		,(["descrizione della sessione"],3)
		,(["amministrazione"],1)
		,(["interrogazione sullo stato del gruppo"],4)
		]


metadata = header << 	(
				(thelink ! [rel "stylesheet", href "/static/style.css", thetype "text/css"] << noHtml)
			+++ 	(thelink ! [rel "icon", href "/static/favicon.ico"] << noHtml)
			+++ 	(thetitle << "Amministrazione G.A.S.") 
			+++ 	(meta ! [httpequiv "Content-Type", content "text/html;charset=utf8;"])
			)  
piede = ulist << 	[
			li << ("Servizio sviluppato da " +++ anchor ! [href "mailto:paolo.veronelli@gmail.com"] 
					<< "paolino" +++ "e amici"),
			li << ("Codice disponibile sotto licenza BSD presso " +++ anchor ! 
				[href "http://github.com/paolino/reactivegas"] << "github.com")
			]  

testata = 	thediv ! [theclass "titolo"] << ("Amministrazione economica di alcuni gruppo d'acquisto") 
	+++	(thediv ! [theclass "reset"] << anchor ! [href "/"] << "annulla la sessione")

pagina 	:: Html 	-- ^ corpo della pagina
	-> String	-- ^ pagina servita
pagina b = prettyHtml $ 
		header << metadata  
	+++ 	body << (		(thediv ! [theclass "testata"] << testata)
				+++ 	(thediv ! [theclass "utente"] << b)
				+++ 	(thediv ! [theclass "pedata"] << piede)  
			)

	

