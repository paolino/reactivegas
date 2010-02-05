{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, NoMonomorphismRestriction #-}

import System.Random
import Codec.Crypto.RSA
import Control.Arrow
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import System.Exit
import Data.IORef
import System.FilePath

import System.Console.Haskeline (runInputT, defaultSettings)


import Data.Maybe
import Lib.Response
import Core.Types
import Core.Controllo
import Core.Contesto
import Core.Inserimento
import Core.Programmazione
import Core.Parsing
import Core.Patch
import Core.File
import Core.Costruzione
import Core.Aggiornamento

import Lib.Console
import Lib.Passo (Costruzione,menu,svolgi) 
import qualified Lib.Passo as P
import Lib.TreeLogs
import Lib.Firmabile

import Eventi.Amministrazione
import Eventi.Anagrafe
import Eventi.Accredito
import Eventi.Impegno
import Eventi.Ordine
import Eventi.Sincronizzatore

----------------- hacking for a boot state ---------------
nuovoStato :: [Responsabile] -> Responsabile -> (TS,[SNodo TS Utente])
nuovoStato rs s = (bootAnagrafe rs  . bootAccredito . bootImpegni . bootOrdini . bootSincronizzatore s $ (),replicate (length rs) $ SNodo True [])
------------------------------------------

-- | il tipo dello stato generale
type TS = TyAnagrafe (TyAccredito (TyImpegni (TyOrdini (TySincronizzatore ()))))

type TSN = ((Int,FilePath),(TS,[SNodo TS Utente]))
-- | lista di prioritizzatori
ps = [priorityAnagrafe, priorityAnagrafeI, priorityAccredito, priorityImpegnoI, priorityImpegno, priorityOrdine, priorityAssenso] 

-- | lista di reattori
rs = [reazioneAnagrafe, reazioneAccredito, reazioneOrdine, reazioneSincronizzatore] :: [Reazione TS ParserConRead Utente ]

caricamento :: MonadIO m => [Esterno Utente] -> (TS,[SNodo TS Utente]) -> m (TS,[SNodo TS Utente])
caricamento es s = do  
	let (q,logs) = caricaEventi ps rs es s
	liftIO $ do 	putStrLn "\n********** Caricamento **************"
			putStrLn $ eccoILogs $ map (first flatten) logs
			putStrLn "*************************************\n"
	return q


type Eventi = [String]
type Logs = String

data Patching
	= forall a. Show a => Evento a 
	| Bocciato String 
	| Sfronda Eventi
	| FinePatch

type Env m = ReaderT TSN (StateT (Maybe Utente,[String]) m)
type Interface m a = Costruzione (Env m) () a

caricamentoT :: MonadIO m => Costruzione (Env m) () (TS,[SNodo TS Utente])
caricamentoT = do 
	(_,evs) <- get
	if (not $ null evs) then do 
		t <- forzaUtente
		if t then do 
			(Just u,evs) <- get 	
			s <- asks snd 
			caricamento (map ((,) u) evs)  s
			else asks snd
		else asks snd
bocciato :: MonadIO m => Interface m a -> String -> Interface m a
bocciato f x =  liftIO (putStrLn ("*** Incoerenza: " ++ x) >> getLine) >> f

forzaUtente :: MonadIO m => Interface m Bool
forzaUtente = do 
	(u,_) <- get
	(s',_) <- asks snd
	case u of 
		Nothing -> runSupporto s' (bocciato (return False)) (\u -> (modify . first . const . Just $ u) >> return True) $ do
				(us :: [Utente]) <- map fst <$> costrResponsabili
				scelte (zip us us) "il nome dell'autore degli eventi"
		Just _ -> return True


patching  :: MonadIO m => Interface m () 
patching = do
	let	nuovoevento x = modify (second $ (++ [show x]) )  >> patching	
		sfronda xs = modify (second $ const xs) >> patching
	(s,_) <- caricamentoT
	(_,evs) <- get
	menu "produzione eventi" . (("fine produzione eventi",return ()):) . concatMap ($bocciato patching) $ [
		costrEventiAnagrafe s nuovoevento ,
		costrEventiAssenso s nuovoevento ,
		costrEventiAccredito s nuovoevento ,
		costrEventiImpegno s nuovoevento ,
		costrEventiOrdine s nuovoevento ,
		costrEventiSincronizzatore s nuovoevento ,
		costrGestionePatch evs () sfronda 
		]

interrogazioni :: MonadIO m => Interface m ()
interrogazioni = do
	(_,evs) <- get
	(s,_) <- if null evs then asks snd else caricamentoT
	let 	response x = liftIO (print x >> getLine) >> interrogazioni
	menu "interrogazione stato del gruppo" . (("fine interrogazione",return ()):) . concatMap ($ bocciato interrogazioni) $ [
		costrQueryAccredito s response,
		costrQueryAnagrafe s response,
		costrQueryOrdine s response,
		costrQueryAssenso s response,
		costrQueryImpegni s response
		]
salvataggio = do
	(Just u,evs) <- get
	((i,_),(s,_)) <- ask
	mpa <- runSupporto s (bocciato $ salvaPatch >> return Nothing) (return . Just) $ mkPatch u evs
	case mpa of 
		Nothing -> return ()
		Just pa -> liftIO $ writeFile ("aggiornamento." ++ u ++ "." ++ show (i + 1)) (show pa) 

salvaPatch = do
	(_,evs) <- get
	when (not $ null evs) $ do 
		t <- forzaUtente
		when t $ do 
			when (not $ null evs) $ menu "vuoi firmare l'aggiornamento che hai prodotto?" $ [("si",salvataggio),("no",return ())]
 	
salvaChiavi c@(u,v) = do
	(_,path) <- asks fst
	liftIO $ writeFile (path </> "chiavi." ++ u) $ show v
	
salvaGruppo g = do
	(i,path) <- asks fst
	liftIO . writeFile (path </> "aggiornamento." ++ show (i + 1)) . show $ g

sincronizza = do
	((i,path),(s,_)) <- ask
	ps <- map value . filter ((==) (i + 1) . index) <$> liftIO (getVerfiles "aggiornamento" path)
	if null ps then bocciato (return ()) $ "nessun aggiornamento per lo stato " ++ show i
		else runSupporto s  (bocciato $ return ()) salvaGruppo   $ mkGroup ps

chiavi = do
	(s,_) <- caricamentoT
	runSupporto s (bocciato $ return ()) salvaChiavi nuoveChiavi

amministrazione = menu "amministrazione" $ [
			("creazione chiavi per un nuovo responsabile" ,chiavi),
			("creazione aggiornamento di gruppo",sincronizza)
			]

bootChiavi = do
	u <- P.libero "scegli il tuo nome di utente"
	p <- P.libero "immetti una password, una frase , lunga almeno 12 caratteri"
	return $ (u,cryptobox p)

bootStato = do
	(_,path) <- asks fst
	cs <- liftIO (getBasfiles "chiavi" path)
	s <- P.scelte (map (bext &&& id) cs) "scegli il sincronizzatore"
	let 	si:rs = map (\(Basfile nome _ chiave) -> (nome,chiave)) $ s:cs
	liftIO $ writeFile "stato.0" . show $ nuovoStato rs si
	
	
boot = menu "amministrazione" $ [
			("creazione chiavi per un nuovo responsabile" ,bootChiavi >>= salvaChiavi),
			("creazione nuovo stato di gruppo", bootStato)
			]

mainmenu :: MonadIO m => Interface m ()
mainmenu = do
	menu "reactivegas (2010)" $ [
		("fine programma",salvaPatch),
		("interrogazione dello stato",interrogazioni >> mainmenu),
		("produzione eventi",patching >> mainmenu),
		("amministrazione",amministrazione >> mainmenu)
		]

loader :: MonadIO m => (Int,(TS,[SNodo TS Utente])) -> Group -> ErrorT String m (TS,[SNodo TS Utente])
loader (_,ts@(s,_)) g = do 
	es <- runReaderT (fromGroup g) s
	caricamento es ts

main :: IO ()
main = do
	mq <- runErrorT $ aggiornamento Nothing loader
	case mq of
		Left s -> error s
		Right (wd,Nothing) -> evalStateT (runReaderT (interazione () boot) ((undefined , wd), error "impossible happened")) $ error "impossible happened"
		Right (wd,Just (i,ts)) -> evalStateT (runReaderT (interazione () mainmenu) ((i,wd),ts)) (Nothing,[])

{-
ts0 = s0 [("paolino",cryptobox "gaien")] ("sincronizzatore",cryptobox "desinc")
-}
