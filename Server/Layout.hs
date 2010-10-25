-- | impostazioni della pagina di interazione http
module Server.Layout (pagina, layout) where

import Text.XHtml

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
piede = thediv ! [theclass "pedata"] << ulist << 	[
			li << anchor ! [href "http://github.com/paolino/reactivegas/wiki"] << "documentazione",
			li << anchor ! [href "http://github.com/paolino/reactivegas"] << "sorgenti"
			]  

testata = 
	thediv ! [theclass "project"] << "ReactiveGAS" +++
	thediv ! [theclass "synopsis"] << "Cooperazione economica per i gruppi di acquisto"
 

pagina 	:: Html 	-- ^ corpo della pagina
	-> String	-- ^ pagina servita
pagina b = prettyHtml $ 
		header << metadata  
	+++ 	body << [
		thediv ! [theclass "titolo boxes"] << (testata +++ piede),
		thediv ! [theclass "abort boxes"] << thediv ! [theclass "reset"] 
			<< anchor ! [href "/"] << "annullamento dell'interazione",
		thediv ! [theclass "utente"] << b,
		thediv ! [theclass "contatti boxes"] << ulist << [
					li << "Contatto sviluppatore:" 
						+++ anchor ! [href "mailto:paolo.veronelli@gmail.com"]<< "e-mail",
					li << "Donazioni: postepay n° 4023600431903923 intestata a Paolo Veronelli"
					]
		]

	

