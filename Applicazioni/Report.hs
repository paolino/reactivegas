-- pubblica un file di report HTML

module Applicazioni.Report where

import Control.Monad (forever)
import System.Time
import Text.XHtml

import Eventi.Accredito
import Eventi.Impegno
import Applicazioni.Reactivegas

metadata = header << 	(
				(thelink ! [rel "stylesheet", href "/static/report.css", thetype "text/css"] << noHtml)
			+++ 	(thetitle << "Report G.A.S.") 
			+++ 	(meta ! [httpequiv "Content-Type", content "text/html;charset=utf8;"])
			)  


 

altrs f  = map (\(i,x) -> tr ! [theclass $ if odd i then "odd" else "even"] << f x) . zip [0..]
reporter :: TS -> Html
reporter s = (table ! [identifier "crediti"] << 
	(caption << "crediti dei membri" +++ tr << (th << "nickname"  +++ th << "credito")
	+++ altrs (\(u,e) -> td << u +++ td << show e) (reportCrediti s)
	))
	+++ (table ! [identifier "casse"] << 
	(caption << "casse dei responsabili"  +++ tr << (th << "nickname"  +++ th << "cassa")

	+++ altrs (\(u,e) -> td << u +++ td << show e) (reportCasse s)
	))
	+++ (table  ! [identifier "impegni"]  << 
	(caption << "acquisti (raccolte di impegni) aperti"  +++ tr << (th << "acquisto"  +++ th << "responsabile" +++
		th << "richieste in attesa" +++ th << "richieste accolte")

	+++ altrs (\(s,b,u,is,as) -> td << s +++ td << u +++ td ! [identifier "richieste"] << richieste as +++ td ! [identifier "accettate"] << accettate is) (reportImpegni s) 
		
	))
	
richieste is =  table <<
	altrs (\(u,e) -> td << u +++ td << show e) is
accettate is =  table <<
	altrs (\(u,e) -> td << u +++ td << show e) is



report d x = do 
	t <- getClockTime >>= toCalendarTime
	let h = h3 << ("Ultimo aggiornamento: " ++ calendarTimeToString t)
	writeFile d $ prettyHtml $ metadata +++ (body << (h +++ maybe (h3 << "Gruppo in costruzione") reporter x))

