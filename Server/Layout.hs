-- | impostazioni della pagina di interazione http
module Server.Layout (pagina, layout) where

import Text.XHtml

layout :: [([String],Int)]
layout = 	
		[(["gruppo di acquisto"],6)
		,(["gestione dichiarazioni"],2)
		,(["descrizione della sessione"],1)
		,(["responsabile autore"],7)
		,(["effetto delle ultime dichiarazioni"],3)
		,(["amministrazione"],4)
		,(["interrogazione sullo stato del gruppo"],5)
		]


metadata = header << 	[
		thelink ! [rel "stylesheet", href "/static/portale.css", thetype "text/css"] << noHtml,
		thelink ! [rel "icon", href "/static/favicon.ico"] << noHtml,
		thetitle << "ReactiveGAS (Amministrazione)",
		meta ! [httpequiv "Content-Type", content "text/html;charset=utf8;"],
		thelink ! [thetype "text/css", href "/static/jquery-ui.css", rel "Stylesheet"]
			<<noHtml,
		script ! [thetype "text/javascript", src "/static/jquery.js"]
			<<noHtml,
		script ! [thetype "text/javascript", src "/static/jquery-ui.js"] << noHtml,
		script ! [thetype "text/javascript", src "/static/help.js"] << noHtml
	] 
 

testata = 
	(thediv ! [title "annulla sessione", theclass "project"] << anchor ! [ href "/"] << "ReactiveGAS") +++
	thediv ! [theclass "synopsis"] << "Servizio gestione prepagato per i gruppi di acquisto"
pagina 	:: [Html] 	-- ^ corpo della pagina
	-> String	-- ^ pagina servita
pagina b = prettyHtml $ 
		header << metadata  
	+++ 	body << [
		thediv ! [theclass "titolo"] << testata,
		thediv ! [identifier "help", theclass "boxes"] << noHtml,
		thediv ! [theclass "utente"] << b,
		thediv ! [theclass "contatti"] << ulist << [
					li << anchor ! [href "http://github.com/paolino/reactivegas/wiki"] << "documentazione",
					li << anchor ! [href "http://github.com/paolino/reactivegas"] << "codice sorgente",
					li << anchor ! [href "mailto:paolo.veronelli@gmail.com"] << "contatti",
					li << "donazioni: postepay 4023600431903923"
					]
		]


