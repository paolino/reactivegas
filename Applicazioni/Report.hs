-- pubblica un file di report HTML

module Applicazioni.Report where

import Control.Monad (forever)
import Control.Concurrent
import Text.XHtml

import Eventi.Accredito
import Eventi.Impegno
import Applicazioni.Reactivegas

metadata = header << 	(
				(thelink ! [rel "stylesheet", href "/report.css", thetype "text/css"] << noHtml)
			+++ 	(thetitle << "Report G.A.S.") 
			+++ 	(meta ! [httpequiv "Content-Type", content "text/html;charset=utf8;"])
			)  


 

reporter :: TS -> Html
reporter s = (thediv ! [theclass "crediti"] << table ! [border 1] << 
	(caption << "crediti dei membri"
	+++
	 map (\(u,e) -> tr << (td << u +++ td << show e)) (reportCrediti s)
	))
	+++ (thediv ! [theclass "casse"] << table ! [border 1] << 
	(caption << "casse dei responsabili"
	+++
	 map (\(u,e) -> tr << (td << u +++ td << show e)) (reportCasse s)
	))
	+++ (thediv ! [theclass "impegni"] << table ! [border 1] << 
	(caption << "acquisti (raccolte di impegni) aperti"
	+++
	 map (\(s,b,u,is,as) -> tr << (td << s +++ td << u +++ td << richieste is +++ td << accettate as)) 
		(reportImpegni s) 
		
	))
	
richieste is = thediv ! [theclass "richieste"] << table ! [border 1] <<
	(caption << "richieste in attesa"
	+++ map (\(u,e) -> tr << (td << u +++ td << show e)) is)
accettate is = thediv ! [theclass "richieste"] << table ! [border 1] <<
	(caption << "richieste accettate"
	+++ map (\(u,e) -> tr << (td << u +++ td << show e)) is)



report d Nothing  = writeFile d $ prettyHtml $ metadata +++ (body << "gruppo in costruzione") 
report d (Just s) = writeFile d $ prettyHtml $ metadata +++ (body << reporter s)

