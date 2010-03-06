{-# LANGUAGE ScopedTypeVariables #-}

module Applicazioni.Server where

import Data.List  (tails)
import Control.Applicative ((<$>))
import Control.Monad.Reader
import Control.Monad.Cont
import Text.XHtml (Html)
import Network.SCGI(CGI, CGIResult, runSCGI, handleErrors)
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
			-> HPasso (ReaderT e IO) () 
			-> IO (Server e Html Link)
sessionFromHPasso l e hp = mkServer l (fromHPasso hp e) print


interazione :: Costruzione (ReaderT () IO) () () 
interazione = rotonda $ \k -> do
		P.output (ResponseOne "benvenuto")
		(x :: Int) <- upload "un file contenente lo show di un numero" 
		P.output (ResponseOne ("bel numero :" ++ show x))
		download "il numero uno.txt" x
		t <- scelte [("si",True),("no",False)] "fine?"
		if t then k () else return ()

-- sessionCgi :: Int -> Costruzione (ReaderT e IO) () () -> IO e -> IO (Server e Html Link) 
sessionCgi l x me = do 	e <- me
 			runReaderT (svolgi x) e >>= sessionFromHPasso l e
	 
-- singleSessionServer :: Int -> Int -> Costruzione (ReaderT e IO) () () -> IO e -> IO ()
singleSessionServer p l x me = do
	s <- sessioning 100 (sessionCgi l x me)	
	runSCGI  (PortNumber $ fromIntegral p)  $ 
		handleErrors (s >>= cgiFromServer)
--
--	in do	key <- show <$> (randomIO  :: IO Int)
--		return (key,FormPoint pass ctx env (h key) ml)
