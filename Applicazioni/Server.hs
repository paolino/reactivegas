{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Applicazioni.Server where


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


rzip :: [a] -> [b] -> [(a,b)]
rzip xs ys = reverse $ zip (reverse xs) (reverse ys)
-- | istanza di Form computata da un HPasso con environment
fromHPasso :: forall e . HPasso (ReaderT e IO) () -> e -> Form e Html Link
fromHPasso (p,[]) e0 = fromHPasso' ((p,e0),[]) where
	fromHPasso' :: ((Passo (ReaderT e IO) (),e),[(Value,ReaderT e IO (Passo (ReaderT e IO) ()))]) -> Form e Html Link
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

sessionFromHPasso 	:: Int 
			-> e 
			-> [[Value]]
			-> HPasso (ReaderT e IO) () 
			-> IO (Server e Html Link)
sessionFromHPasso l e vss hp = do
	fs <- forM vss $ \vs -> (,) (Just $ length vs) <$> restore (fromHPasso hp e) vs
	mkServer l fs  (const $ return ())
	
		
	


interazione :: Costruzione (ReaderT () IO) () () 
interazione = rotonda $ \k -> do
		P.output (ResponseOne "benvenuto")
		(x :: Int) <- upload "un file contenente lo show di un numero" 
		P.output (ResponseOne ("bel numero :" ++ show x))
		download "il numero uno.txt" x
		t <- scelte [("si",True),("no",False)] "fine?"
		if t then k () else return ()

-- sessionCgi :: Int -> Costruzione (ReaderT e IO) () () -> IO e -> IO (Server e Html Link) 
sessionCgi l x vss me = do 	e <- me
 				runReaderT (svolgi x) e >>= sessionFromHPasso l e vss
	
checkReset :: CGI a -> CGI a -> CGI a 
checkReset reset k = do	
	vs <- getVars 
	case lookup "REQUEST_URI"  vs of 
		Just x -> let xs =  tail $ splitOneOf "/?" x in
			case xs of
				("reset":_) -> reset
				_ -> k
		_ -> k

sessionServer 	:: forall e . Int 	 -- ^ porta del server scgi
			-> Int 	 -- ^ numero massimo di ricordi per sessione
			-> Costruzione (ReaderT e IO) () () -- ^ interfaccia utente
			-> (Html -> CGI CGIResult) -- ^ gestore del response
			-> [[Value]] -- ^ serializzazione delle foem di default
			-> IO e  -- ^ produzione di evironment per sessione 
			-> IO () -- ^ aloa
sessionServer (PortNumber . fromIntegral -> port) limit interface responseHandler defaultForms newEnvironment = do
	(server :: CGI (Server e Html Link,IO ()),reset) <- sessioning 100 (sessionCgi limit interface defaultForms newEnvironment) 
	runSCGI port $ handleErrors (checkReset reset server >>= cgiFromServer responseHandler)
