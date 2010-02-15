
module Core.Aggiornamento (Aggiornamento (..), aggiornamento) where


import Data.List (sort)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))

import System.Directory (getCurrentDirectory)
import System.FilePath (takeExtension,replaceExtension, (</>))


import Lib.Valuedfiles  (Valuedfile (..), ext, maybeParse, getValuedfiles)

import Core.Patch (Patch,Group)
import Core.Types (Evento)

import Eventi.Anagrafe (Responsabile, Utente)
-----------------------------------------------------------------------------------------

consumaM :: (Show a) => (a -> b -> IO (Utente,a)) -> Valuedfile a Int -> [Valuedfile b Int] -> IO (Valuedfile a Int)
consumaM _ y [] = return y
consumaM agg y@(Valuedfile n p s) (Valuedfile m q d :xs) 
	| m <= n = do
		putStrLn $ "Warn: sono presenti file di stato vecchi"++ q
		consumaM agg y xs
	| m == n + 1 = do
		(u,s') <- agg s d
		let 	np = replaceExtension p (takeExtension q)
		s' `seq` do 	writeFile np (show s') 
				putStrLn $ " Caricato l'aggiornamento " ++ show m ++ " firmato da " ++ u 
		consumaM agg (Valuedfile m np s') xs
	| m > n + 1 = do
			putStrLn $ "Warn: Rilevato l'aggiornamento " ++ show m 
				++ ", manca l'aggiornamento " ++ show (n + 1)
			return y
consumaM _ (Valuedfile _ _ _) ((Valuedfile _ _ _) : _) = error "broken consumaM"


data Aggiornamento a 	
	= Boot 	{ publishStato :: ([Responsabile] -> a) -> IO ()	
		, publishChiavi :: Responsabile -> IO ()
		}
	| Flow 	{ getStato :: a 
		, getEventi :: [Evento]
		, publishEventi :: [Evento] -> IO ()
		, publishUPatch :: Utente -> Patch -> IO ()
		, publishChiavi :: Responsabile -> IO ()
		, publishGPatch :: Maybe (([Patch] -> Group) -> IO ())
		}
aggiornamento :: (Show a, Read a) => Maybe FilePath -> (a -> Group -> IO (Utente,a)) -> IO (Aggiornamento a)
aggiornamento mf aggiorna = do
	putStrLn "\n\n *************** Inizio aggiornamento ***********"
	wd <- maybe getCurrentDirectory return mf 
	let writeChiavi (r,c) = writeFile (wd </> "chiavi." ++ r) . show $ c
	putStrLn $ " Cartella di lavoro: " ++ wd
	stati <- reverse . sort <$> getValuedfiles maybeParse "stato" wd
	cs <- map (ext &&& value) <$> (getValuedfiles return "chiavi" wd)
	putStrLn $ " Rilevate chiavi responsabile " ++ show (map fst cs)
	if  not $ null stati then do 
		let stato = head stati
		putStrLn $ " Rilevato il file di stato " ++ show (ext stato)
		(_,aggiornamenti) <- break ((> ext stato) . ext) <$> sort 
			<$> getValuedfiles maybeParse "aggiornamento" wd
		putStrLn $ " Rilevati gli aggiornamenti di gruppo " ++ show (map ext aggiornamenti)	
		Valuedfile n _ s <- consumaM aggiorna stato aggiornamenti
		as <- map value . filter ((==) (n + 1) . ext) <$> liftIO (getValuedfiles maybeParse "aggiornamento" wd)
		putStrLn $ " Rilevati " ++ show (length as) ++ " aggiornamenti individuali "
		rs <- map value . filter ((==) (n + 1) . ext) <$> liftIO (getValuedfiles maybeParse "eventi" wd)
		putStrLn $ " ********* Fine aggiornamento (stato " ++ show n ++ ") ********\n\n"
		return $ Flow 
			s
			(if null rs then [] else head rs)
			(writeFile (wd </> "eventi." ++ show (n + 1)) . show)
			(\u -> writeFile (wd </> "aggiornamento." ++ u ++ "." ++ show (n + 1)) . show)
			writeChiavi $
			if null as then Nothing else
				Just $ writeFile (wd </> "aggiornamento." ++ show (n + 1)) . show . ($as)
		else do
			putStrLn $ " ********* Fine aggiornamento (inizializzazione) ********\n\n"
			return $ Boot 
				(writeFile  (wd </> "stato.0") . show . ($cs)) 
				writeChiavi
aggiornamentoWeb :: (Show a, Read a) => Maybe FilePath -> (a -> Group -> IO (Utente,a)) -> IO (Aggiornamento a)
aggiornamentoWeb mf aggiorna = do
	putStrLn "\n\n *************** Inizio aggiornamento ***********"
	wd <- maybe getCurrentDirectory return mf 
	let writeChiavi (r,c) = writeFile (wd </> "chiavi." ++ r) . show $ c
	putStrLn $ " Cartella di lavoro: " ++ wd
	stati <- reverse . sort <$> getValuedfiles maybeParse "stato" wd
	cs <- map (ext &&& value) <$> (getValuedfiles return "chiavi" wd)
	putStrLn $ " Rilevate chiavi responsabile " ++ show (map fst cs)
	if  not $ null stati then do 
		let stato = head stati
		putStrLn $ " Rilevato il file di stato " ++ show (ext stato)
		(_,aggiornamenti) <- break ((> ext stato) . ext) <$> sort 
			<$> getValuedfiles maybeParse "aggiornamento" wd
		putStrLn $ " Rilevati gli aggiornamenti di gruppo " ++ show (map ext aggiornamenti)	
		Valuedfile n _ s <- consumaM aggiorna stato aggiornamenti
		as <- map value . filter ((==) (n + 1) . ext) <$> liftIO (getValuedfiles maybeParse "aggiornamento" wd)
		putStrLn $ " Rilevati " ++ show (length as) ++ " aggiornamenti individuali "
		rs <- map value . filter ((==) (n + 1) . ext) <$> liftIO (getValuedfiles maybeParse "eventi" wd)
		putStrLn $ " ********* Fine aggiornamento (stato " ++ show n ++ ") ********\n\n"
		return $ Flow 
			s
			(if null rs then [] else head rs)
			(writeFile (wd </> "eventi." ++ show (n + 1)) . show)
			(\u -> writeFile (wd </> "aggiornamento." ++ u ++ "." ++ show (n + 1)) . show)
			writeChiavi $
			if null as then Nothing else
				Just $ writeFile (wd </> "aggiornamento." ++ show (n + 1)) . show . ($as)
		else do
			putStrLn $ " ********* Fine aggiornamento (inizializzazione) ********\n\n"
			return $ Boot 
				(writeFile  (wd </> "stato.0") . show . ($cs)) 
				writeChiavi

data Aggiornamento a =
	Boot 	{ 	writeStato :: ([Responsabile] -> a) -> IO ()	
		, 	writeChiavi :: Responsabile -> IO ()
		}
	| Flow 	{ 	readStato :: IO a,
			readEventi :: Utente -> IO [Eventi],
			writeEventi :: Utente -> Eventi -> IO (),
			writeUPatch :: Utente -> Patch -> IO (),
			readUPatches :: IO [Patch],
		 	writeChiavi :: Responsabile -> IO (),
			writeGPatch :: Group -> IO ()
			readLog :: IO [String]
		}
			 
data Board a = Board 
	{	tstato :: TVar a
	,	teventi :: TVar [(Utente,TVar [Evento])]
	,	tupatch :: TChan (Utente,Patch)
	,	tchiavi :: TChan Resposabile
	,	tgpatch :: TChan Group
	,	tlog 	:: TChan String
	}

		
	
