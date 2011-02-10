-- pubblica un file di report HTML

module Applicazioni.Report where

import Data.Maybe (catMaybes)
import Control.Monad (forever)
import System.Time
import Text.XHtml

import Lib.Aspetti (see)
import Lib.Euro
import Core.Types (Utente)
import Core.Programmazione (estrai)
import Eventi.Accredito
import Eventi.Impegno
import Eventi.Acquisto
import Applicazioni.Reactivegas
import Applicazioni.Database.Movimenti
import Applicazioni.Database.Acquisti
import Eventi.Accredito (Movimento (..))

metadata = header << 	(	
			script ! [thetype "text/javascript", src "/static/report.js"] << noHtml
			+++	(thelink ! [rel "stylesheet", href "/static/report.css", thetype "text/css"] << noHtml)	
			+++ 	(thelink ! [rel "icon", href "/static/favicon.ico"] << noHtml)
			+++ 	(thetitle << "Report G.A.S.") 
			+++ 	(meta ! [httpequiv "Content-Type", content "text/html;charset=utf8;"])
			)  

movimenti ::  Movimento -> Html
movimenti (MovimentoU u de s) = tr << (td << show de +++ td << s)
movimenti (MovimentoR u de s) = tr << (td << show de +++ td << s)

altrsM xs f  = mapM (\(i,x) -> f x >>= \y -> return $ tr ! [theclass $ if odd i then "odd" else "even"] << y) . zip [0..] $ xs
altrs xs f  = map (\(i,x) -> tr ! [theclass $ if odd i then "odd" else "even"] << f x) . zip [0..] $ xs
reporter :: Int -> Movimenti -> Acquisti -> TS  -> IO Html
reporter n (Movimenti _ conti casse) (Acquisti _ acquisti) s  = do

	conti' <- altrsM (reportCrediti s) $ \(u,e) -> do
		ms <- conti u n
		return (td << u +++ td << show e +++ td ! [theclass "movimentiU"] << table << map movimenti ms)
	casse'  <- altrsM (reportCasse s) $ \(u,e) -> do
		ms <- casse u n
		return (td << u +++ td << show e +++ td ! [theclass "movimentiR"] << table << map movimenti ms)
	acquisti' <- acquisti n
	return $ (table ! [identifier "crediti"] << 
			(caption << "crediti dei membri" +++ tr << (
				th << "nickname"  +++ 
				th << "credito" +++ 
				th ! [theclass "colMovimenti"] << "ultimi movimenti")
			+++ conti' ))
		+++ (table ! [identifier "casse"] << 
			(caption << "casse dei responsabili"  +++ tr << (
				th << "nickname"  +++ 
				th << "cassa" +++ 
				th ! [theclass "colMovimenti"] << "ultimi movimenti")
			+++ casse' ))
		+++ (table  ! [identifier "impegni"]  << 
			(caption << "acquisti aperti"  
			+++ tr << (th << "acquisto"  +++ th << "responsabile" +++
			th ! [theclass "colRichieste"] << "richieste in attesa" +++ th ! [theclass "colAccolte"] << "richieste accolte")
			+++ altrs (reportImpegni s)  (\(s,b,u,is,as) -> td << s +++ td << u +++ td ! [theclass "richieste"] << richieste as +++ td ! [theclass "accettate"] << richieste is) 
			))
	
		+++ (table  ! [identifier "acquisti"]  << 
			(caption << "acquisti chiusi"  
			+++ tr << (th << "acquisto"  +++ th << "quote")
			+++ altrs acquisti'  (\(FineAcquisto b uvs) -> 
				td << b +++ td ! [theclass "quote"] << table << altrs uvs (\(u,v) -> 
					td << u +++ td << (show v)))
			))

richieste is =  table <<
	altrs is (\(u,e) -> td << u +++ td << show e) 


mkReporter :: FilePath -> FilePath -> Int -> Bool -> IO ((Effetti,Maybe QS) -> IO ())
mkReporter wd d l t = do 
	m@(Movimenti i _ _) <- mkMovimenti wd t 
	a@(Acquisti ia _) <- mkAcquisti wd t 
	return $ \(ls,x) -> do 
		t <- getClockTime >>= toCalendarTime
		let 	h = h3 << ("Ultimo aggiornamento: " ++ calendarTimeToString t)
		b <- case x of
			Nothing -> return $ h3 << "Gruppo in costruzione"
			Just (QS (x,_)) -> do 
				i (see x) . fst . estrai $ ls
				mapM_ ia  . fst . estrai $ ls
				reporter l m a x
		writeFile d $ prettyHtml $ metadata +++ (body << (h +++ b))


