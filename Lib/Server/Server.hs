{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Lib.Server.Server (server) where


import Data.List  (tails)
import Data.List.Split (splitOneOf)
import Control.Applicative ((<$>))
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Concurrent.STM (STM)
import Text.XHtml (Html)
import Network.SCGI(CGI, CGIResult, runSCGI, handleErrors,getVars)
import Network (PortID (PortNumber))


import Lib.Response
import Lib.Server.Core
import Lib.Server.CGI
import Lib.Server.Session
import Lib.Passo
import qualified Lib.Passo as P
import Lib.HTTP
import Lib.Missing ((>$>))

-- | la monade interna all'interazione che ammettiamo.
type Running e = ReaderT e IO

type RPasso e =  Passo (Running e) ()

type HRPasso e = HPasso (Running e) ()

rzip :: [a] -> [b] -> [(a,b)]
rzip xs ys = reverse $ zip (reverse xs) (reverse ys)

-- | istanza di Form computata da un HPasso con environment. L'elaborazione prevede il passaggio del passo di radice, quindi, un passo senza storia.
fromHPasso :: forall e . HRPasso e -> e -> Form e Html Link
fromHPasso (p,[]) e0 = fromHPasso' ((p,e0),[]) where
	fromHPasso' :: ((RPasso e,e),[(Value,Running e (RPasso e))]) -> Form e Html Link
	fromHPasso' ((p,e),qs) = let 
			(h,ml,cont) = runPasso p -- trasformata html
			vs = map fst qs
			pass v = cont v >>= \mhp -> return $ do 
				((p',ps),e') <- runReaderT (liftM2 (,) mhp ask) e 
				return . fromHPasso' $ ((p',e'), rzip (v : vs) $ ps) -- postulato sul comportamento di Passo.cont
			reload = let
				check k (e,_) (vmps@((_,mp):_)) = do
					(p,e') <- lift $ runReaderT (liftM2 (,) mp ask)  e
					let result = ((p,e'),vmps)
					case p of	Errore _ _  -> k result
							_ -> return (e',result)
				in fromHPasso' >$> flip runContT return .callCC $ \k -> 
					fmap snd . foldM (check k) (e0,((p,e0),[])) . tail . reverse . tails $ qs
			in Form pass reload (map fst qs) (\enk -> h enk . show) ml
fromHPasso _ _ = error "inizializzazione con contesto non implementata"	
	
checkReset :: CGI a -> CGI a -> CGI a
checkReset reset k = do
	vs <- getVars
	
	case lookup "REQUEST_URI" vs of
		Just x -> let xs = tail $ splitOneOf "/?" x in
			case xs of
				[""] -> reset
				_ -> k
		_ -> k

server 	:: forall e b . (Read b,Show b) 
	=>  FilePath		-- ^ cartella di lavoro
	-> Int 			-- ^ porta del server scgi
	-> Int					-- ^ numero massimo di ricordi per sessione
	-> Int					-- ^ numero massimo di sessioni simultanee 	
	-> Costruzione (Running e) () () 	-- ^ applicazione
	-> CGI (Maybe CGIResult)				-- ^ preserver
	-> ([Html] -> CGI CGIResult) 		-- ^ gestore del response
	-> [([Value],Int)] 			-- ^ serializzazione delle form di default
	-> (STM () -> Maybe b -> IO (e, IO b))  		-- ^ produzione e restore di evironment per sessione
	-> IO () 				-- ^ aloa
server path (PortNumber . fromIntegral -> port) limitR limitS applicazione preServer 
		responseHandler defaultForms newEnvironment = do
	-- definizione di nuova sessione
	let 	newSession signal s = do 
			-- ogni sessione ha la possibilità di avere il suo environment
			(en, ben) <-  newEnvironment signal s
			-- esplicitazione della definizione di applicazione (runContT)
			(hp :: HRPasso e) <- runReaderT (svolgi applicazione) en
			-- computazione delle forms
			(fs :: [IdedForm e Html Link]) <- forM defaultForms $ \(vs,i) -> flip (,) i <$> restore (fromHPasso hp en) vs
			-- boot di un nuovo servizio 
			s <- mkServer limitR fs
			return (s,ben)
			
			
	(run,reset) <- sessioning path limitS newSession
	putStrLn "** Server attivo"
	runSCGI port . handleErrors $ do 
		b <- preServer
		case b of
			Nothing -> checkReset reset run >>= cgiFromServer responseHandler 
			Just result -> return result
