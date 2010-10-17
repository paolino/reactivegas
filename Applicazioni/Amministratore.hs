
{-# LANGUAGE Rank2Types, ScopedTypeVariables, RecursiveDo #-}

module Applicazioni.Amministratore where
import Control.Monad (when)
import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Arrow (first,(&&&))
import Lib.Passo

import Data.List (lookup)
import Lib.Tokens 
import Control.Monad.Trans (liftIO, MonadIO)
import Core.Types
import Lib.Modify (PeekPoke)
import Lib.Response
import System.FilePath
import System.FilePath.Find
import System.Directory
import Lib.Filesystem
import Lib.Modify

import Applicazioni.Persistenza

type Gruppo = String


uiNuovoGruppo :: (MonadIO m, Functor m) 
	-- | procedura di inserimento gruppo
	=> ((FilePath, Gruppo) -> Token -> IO ())
	-> (Token,Gruppo)
	-- | interfaccia
	-> Costruzione m b ()
uiNuovoGruppo new (tok,g) = do
	let dir = "gruppi" </> g
	t <- liftIO $ doesDirectoryExist dir
	let go = liftIO $ do
		when t $ renameDirectory dir ("gruppi" </> addExtension g "copia")
		createDirectoryIfMissing True (dir </> "static")
		print $ "created " ++ dir </> "static"
		new (dir,g) tok

	if t then menu "gestione conflitto nuovo gruppo" [
			("eliminare il vecchio gruppo con lo stesso nome e proseguire", go),
			("abbandonare la procedura di creazione nuovo gruppo",return ())
			]
		else go

uiAmministratore :: (MonadIO m, Monad m , Functor m) => PeekPoke (Tokenizer () Gruppo) -> ((Token,Gruppo) -> Costruzione m b ()) -> [(String,Costruzione m b ())]
uiAmministratore pp c = uiTokenizer  pp "elenco dei gruppi operativi"
		(libero "nome del nuovo gruppo da associare al token")
		($ c )
		(\_ -> errore $ ResponseOne "la chiusura del servizio non passa di qua")

mkAmministratore ::(MonadIO m, Functor m) 
	=> ((FilePath, String) -> Token -> IO (Persistenza a b d)) 
	-- | il menu di amministrazione e la query da nome del gruppo a persistenza
	-> Token 			-- ^ password di amministrazione
	-> IO ([(String, Costruzione m q ())], Gruppo -> STM (Maybe (Persistenza a b d)),IO [Gruppo])
mkAmministratore runGruppo pass = mdo
	tok0 <- onFS  (readFile "amministratore") (\_ -> return $ mkTokenizer (pass,[],[])) (return . mkTokenizer . read) 
	
	mtok <- mkPeekPoke tok0 (atomically $ writeTChan commit ())
	commit <- atomically newTChan  
	forkIO $ do 	atomically $ readTChan commit
			tok <- peek mtok
			onFS  (writeFile "amministratore" $ show $ serialize tok) error return
	ms <- tail `fmap` find 
		((== 0) `fmap` depth) 
		((== Directory) `fmap` fileType) 
		"gruppi"
	pes <- atomically $ newTVar []
	let new (dir,g) t = do 	
		pe <- runGruppo (dir,g) t
		liftIO . atomically $ readTVar pes >>= writeTVar pes . ((g,pe):)
	mapM_ (flip new $ error "token di super responsabile perso") (map (id &&& takeFileName) ms)
	return $ (uiAmministratore mtok (uiNuovoGruppo new), \g -> lookup g `fmap` readTVar pes, 
		map fst `fmap` atomically (readTVar pes))


