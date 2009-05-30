{-# LANGUAGE TypeOperators, ViewPatterns, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
import System.Environment
import Control.Applicative ((<$>))
import System.IO
import Codec.Crypto.RSA
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad.State
import Control.Monad.Cont
import Control.Monad.Identity
import Data.List

import Data.Maybe
import Control.Monad.Error
import Codec.Binary.UTF8.String
import System.Directory

import Anagrafe
import MakePatch 
import Costruzione
import Rete
import Controllo 
import Applicazione
import Prioriti

main =  do	
	s  <- readFile "stato"
	let h = showDigest $ sha512 $ B.pack s
	g <- read <$> readFile "sincronizzatore.publ"
	[x,z] <- getArgs
	ps <- query x (read z) (show $ (g,Aggiornamento h))
	s' <- aggiornaStato g s ps 
	vs <- query x (read z) (show $ (g,Validi))
	when (responsabiliQ s' /= vs) $ error "Il sicronizzatore sta truffando sui responsabili validi"
	writeFile "stato" s'
	hSetBuffering stdout NoBuffering 
	runStateT (svolgi (interfaccia (x,read z) g s') >>= runCostruzioneIO) (Nothing,[])

-- interfaccia :: (String,Int) -> PublicKey -> String -> MakePatch r m ()
interfaccia hp puk s = let c = correggiStato (liftIO . stampaLogs) reattori priorities s in 
	nodo (liftIO . logerrore) [
		autenticazione (c, liftIO . cercaChiave), 
		commit (liftIO . send hp puk s), 
		const $ trattamentoEvento (liftIO . logerrore, c) makers
		]


logerrore  =  putStrLn .( ++ "****" ) . ("****" ++)

--correggiStato :: MonadState Patch m => (Log Utente -> m ()) -> [Reazione r ParserConRead Utente] -> [R] ->  String -> m r
correggiStato pl rs bs s = do
	(uprk,xs) <- get 
	let ys = case uprk of 
		Just (u,prk) -> zip (repeat u) xs
		Nothing -> []
	case null ys of
		False -> let (r,_,log) = runIdentity $ runProgramma rs s (caricaEventi bs ys >> (fst <$> get)) in
			pl log >> return r
		True -> let (r,_,log) = runIdentity $ runProgramma rs s (fst <$> get) in return r

-- supporto IO 
send :: (String,Int) -> PublicKey -> String -> (Utente,PrivateKey,[String]) -> IO String
send (x,z) g s (u, prk, reverse -> es) = do
	pu <- read <$> (readFile $ u ++ ".publ")
	let h = showDigest $ sha512 $ B.pack s
	let r = (pu,sign prk (B.pack $ h ++ concat es),es)
	query x z $ show (g,Patch r) 

cercaChiave s = do
	ls <- getDirectoryContents "."
	case find ((==) $ s ++ ".priv") ls of
		Nothing -> print ("file chiave privata di " ++ decodeString s ++ " non trovato") >> return Nothing
		Just x -> Just . read <$> readFile x

runCostruzioneIO :: (Monad m, MonadIO m) =>  Costruzione m  a -> m (Maybe a)
runCostruzioneIO  c = flip runContT return . callCC $ \k -> 
	let zeta c@(Costruzione l f) = do 
		let riprova s  = nl >> msg s >> zeta c
		r <- runErrorT $ nl >> case l of
			Libero z -> do	(encodeString -> x) <- pgl z
					backifnull (errorOrAhead (lift . lift . f) . stringOrNot) x
			Scelta t as -> do 	let bs = ("fine",undefined) : as
						liftIO $ mapM_ putStrLn  [show n ++ ". " ++ decodeString a | (n,a) <- zip [0..] (map fst bs)]
						nl
						let 	q y = do 	when (y < 0 || y > length as) $ throwError "scelta impossibile"
									when (y == 0) $ lift (k Nothing)
									lift . lift .  f . snd $ (as !! (y - 1))	
						pgl t >>= backifnull (errorOrAhead q . toMaybe . reads)
		either riprova return r 
		where
			backifnull f x = if null x then return Nothing else f x
			errorOrAhead q =  maybe (throwError "errore di lettura") 
				(\x -> q x >>= lift . zeta >>= maybe (lift $ zeta c) (return . Just)) 
			stringOrNot s = toMaybe $ case reads s of
				[] -> reads ("\"" ++ s ++ "\"")
				x -> x
			nl 	= liftIO (putStrLn "")
			prompt 	= liftIO . putStr . (++ ": ")
			pgl s 	= prompt s >> gl
			msg s 	=  nl >> (liftIO . putStrLn . (++ " ****").("**** " ++). decodeString $ s)
			gl 	= liftIO getLine
			toMaybe =  fmap fst . listToMaybe
	in zeta c			
			

	
