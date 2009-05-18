{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction #-}
module Controllo where

import Control.Monad.RWS
import Control.Applicative
import Control.Arrow
import Data.List
import Data.Maybe
import Data.Either
import Debug.Trace
import Data.Function

import Core (Nodo, runInserzione , inserimentoCompleto, Reazione, mkNodi, reattore)
import Serializzazione (serializza, deserializza)
import Prioriti
import Text.PrettyPrint

type Log d = [([(d,String)],String)]
-- ---------------------------------------------
-- | monade di caricamento eventi
type Programma m s c d = RWST 
	() -- spazio libero per le configurazioni
	(Log d) -- il log del caricamento zippato con gli eventi causanti
	(s,[Nodo s c d]) -- lo stato applicativo e le basi dei nodi reattori
	m -- una monade interna
	

caricaEvento :: (Show d, Monad m) => (d,String) -> Programma m s c d ()
caricaEvento x = do
	(s,rs) <- get
	let (rs',s',ws) = runInserzione (inserimentoCompleto x rs) [] s
	tell ws
	put (s',rs')

caricaEventi :: (Show d, Monad m) => [R] -> [(d,String)] -> Programma m s c d ()
caricaEventi rs = mapM_ caricaEvento . sortP rs snd

caricaStato :: (Read s, Show s, Read d, Monad m) => String -> Programma m s c d ()
caricaStato s = do
	(_,map (fromJust . reattore) -> rs) <- get -- si ritiene che i reattori siano li :)
	put . second (map (uncurry deserializza) . (flip zip rs)) . read $ s

scaricaStato :: (Read s, Show s, Show d,  Monad m) =>  Programma m s c d String
scaricaStato = show <$> second (map serializza) <$> get

-- esegue una computazione Programma all'interno di una modifica dello stato serializzato
conStato :: (Read s, Show s, Read d, Show d, Monad m) => String -> Programma m s c d b ->  Programma m s c d (b,String)
conStato s f = caricaStato s >> liftM2 (,) f scaricaStato 

runProgramma :: (Read s, Show s, Read d, Show d, Monad m) => [Reazione s c d] -> String -> Programma m s c d b -> m (b,String,Log d)
runProgramma rs s p = do 
	((y,s'),_,ws) <- runRWST (conStato s p) () (undefined, mkNodi rs) 
	return (y,s',ws)
{-	
-- | il metodo normale per caricare un set di eventi su uno stato serializzato e ottenere lo stato serializzato risultante
-- insieme al log del caricamento inchiavato con gli eventi
-- attenzione che uno stato serializzato puó essere deserializzato senza errori e fidato 
-- solamente se la lista di reattori passata é funzionalente equivalente a quella con cui é stato costruito
-- ovvero revisioni delle reazioni rendono errata e/o catastrofica la deserializzazione, in tal caso ricaricare tutti gli eventi
-- partendo da uno stato fresco
cicloStandard :: (Read s, Show s, Read d, Show d, Monad m) => 
	[Reazione s c d] -- albero reazioni
	-> String 
	-> [(d,String)] 
	-> m (String,Log d)
cicloStandard reazioni stato eventi = do
	(_,stato',logs) <- runProgramma reazioni stato (mapM_ caricaEvento eventi)
	return (stato',logs)
-}
--------------------- programma di stampa ------------------------------------------
data Show a => Tree a = Node (a,[Tree a]) | Leaf 

showTrees =  render . vcat . map renderTree . passa  where
	renderTree Leaf = Text.PrettyPrint.empty
	renderTree (Node (x,ts)) = text x $$ nest 3 (vcat (map renderTree ts))

passa :: (Show a ,Eq a) => [[a]] -> [Tree a]
passa xs = let 	h = map ((head . head) &&& map tail) . groupBy ((==) `on` head) 
		in do 	y <- groupBy ((==) `on` null) xs  -- distinguiamo tra liste vuote e piene
			if null . head $ y then const Leaf <$> y -- una lista di liste vuote é una lista di Leaf
				else Node . second passa <$>  h y -- ogni sottosequenza di una lista  di liste piene che ha 
					-- la stessa testa é un nodo. ricorsivamente analizziamo il resto
stampaLogs = 	putStrLn . showTrees . map (\(as,b) -> (map (\(d,e) -> show d ++ ":" ++ e) as) ++ ["------> " ++ b])
----------------------------------------

