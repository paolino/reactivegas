{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Lib.Server.Server (server) where


import Data.List  (tails)
import Data.List.Split (splitOneOf)
import Control.Applicative ((<$>))
import Control.Monad.Reader
import Control.Monad.Cont
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


type Running e = ReaderT e IO

type RPasso e =  Passo (Running e) ()

type HRPasso e = HPasso (Running e) ()

rzip :: [a] -> [b] -> [(a,b)]
rzip xs ys = reverse $ zip (reverse xs) (reverse ys)

-- | istanza di Form computata da un HPasso con environment
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

server 	:: forall e . Int 			-- ^ porta del server scgi
	-> Int					-- ^ numero massimo di ricordi per sessione
	-> Int					-- ^ numero massimo di sessioni simultanee 	
	-> Costruzione (Running e) () () 	-- ^ applicazione
	-> (Html -> CGI CGIResult) 		-- ^ gestore del response
	-> [([Value],Int)] 			-- ^ serializzazione delle form di default
	-> IO e  				-- ^ produzione di evironment per sessione 
	-> IO () 				-- ^ aloa
server (PortNumber . fromIntegral -> port) limitR limitS applicazione responseHandler defaultForms newEnvironment = do
	-- definizione di nuova sessione
	let newSession = do 
		-- ogni sessione ha la possibilità di avere il suo environment
		en <-  newEnvironment
		-- esplicitazione della definizione di applicazione (runContT)
		(hp :: HRPasso e) <- runReaderT (svolgi applicazione) en
		-- computazione delle forms
		(fs :: [IdedForm e Html Link]) <- forM defaultForms $ \(vs,i) -> flip (,) i <$> restore (fromHPasso hp en) vs
		mkServer limitR fs  
	(run,reset) <- sessioning limitS newSession 
	runSCGI port $ handleErrors (checkReset reset run >>= cgiFromServer responseHandler)
