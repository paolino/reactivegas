{-# LANGUAGE ScopedTypeVariables #-}

module Applicazioni.Server where

import Data.List  (tails)
import Control.Monad.State
import Control.Monad.Cont
import Text.XHtml (Html)

import Lib.Response
import Lib.Server.Core
import Lib.Server.CGI
import Lib.Passo
import qualified Lib.Passo as P
import Lib.HTTP

-- | istanza di Form computata da un HPasso con environment
fromHPasso :: HPasso (StateT e IO) () -> e -> Form e Html Link
fromHPasso (p,[]) e0 = fromHPasso' ((p,e0),[]) where
	fromHPasso' ((p,e),ps) = let 
			(h,ml,cont) = runPasso p -- trasformata html
			pass = cont >=> \mhp -> return $ do 
				((p',ps),e') <- runStateT mhp e 
				return . fromHPasso' $ ((p',e'),ps)
			reload = let
				check k (e,_) (mp:rps) = do
					(p,e') <- lift $ runStateT mp e
					let result = ((p,e'),ps)
					case p of	Errore _ _  -> k result
							_ -> return (e',result)
				in fromHPasso' >$> flip runContT return .callCC $ \k -> 
					fmap snd . foldM (check k) (e0,undefined) . tail . reverse . tails $ ps
			in Form pass reload (\enk -> h enk . show) ml
fromHPasso _ _ = error "inizializzazione con contesto non implementata"	

sessionFromHPasso 	:: Int 
			-> IO e 
			-> HPasso (StateT e IO) () 
			-> IO (Server e Html Link)
sessionFromHPasso l me hp = me >>= mkServer l . fromHPasso hp 


interazione :: Costruzione (StateT () IO) () () 
interazione = rotonda $ \k -> do
		P.output (ResponseOne "benvenuto")
		(x :: Int) <- upload "un file contenente lo show di un numero" 
		P.output (ResponseOne ("bel numero :" ++ show x))
		download "il numero uno.txt" x
		t <- scelte [("si",True),("no",False)] "fine?"
		if t then k () else return ()
--
--	in do	key <- show <$> (randomIO  :: IO Int)
--		return (key,FormPoint pass ctx env (h key) ml)
