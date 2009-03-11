module Integrity (mkIntegrity, Integrity, mkRPatch , RPatch, mkPatch , Patch, takeIntegrity) where

import Control.Monad.Error ()
import Control.Monad (liftM2)
import Control.Monad.Reader (lift, ReaderT, runReaderT, ask)
import Control.Applicative ((<$>))
import qualified Data.Map as M (singleton, keys)

import qualified Data.ByteString.Lazy.Char8 as B (pack,ByteString)

import Codec.Crypto.RSA (sign, verify, PublicKey, PrivateKey)

import Bugs
import Eventi (Estratto (..), Eventi, vuoto, Logico)

type Firma = B.ByteString 

data Conoscenza 
	= Conoscenza Int Patch Conoscenza 
	| Base PublicKey PublicKey 
	deriving (Show,Read)

unfoldConoscenza (Base _ _) = []
unfoldConoscenza (Conoscenza n p c') = (p,n) : unfoldConoscenza c'

data Integrity = Integrity {
	conoscenza :: Conoscenza, 
	padrone :: PublicKey, 
	estratto :: Estratto 
	}

-- | l'environment per le computazioni di integrita'
type Env = ReaderT Integrity (Either String)

mkIntegrity :: Conoscenza -> [Patch] -> Either String Integrity 
mkIntegrity b@(Base m u) = foldl valida (Right $ Integrity b m start) . zip [0..] where
	start = vuoto {responsabili = M.singleton u Nothing} -- let first respo in 
	valida ei (n, p) = do
		i <- ei
		runReaderT (validazionePatch p) i
		return i{conoscenza = Conoscenza n p (conoscenza i)}
mkIntegrity _ = const $ Left "mkIntegrity: inizializzazione della conoscenza con qualcosa di diverso da una Base"


-- | le patch per aggiornare una conoscenza
takeIntegrity :: Int -> Env [Patch]
takeIntegrity n = reverse . map fst . takeWhile ((>=) n . snd) 
	. unfoldConoscenza . conoscenza <$> ask

data RPatch = RPatch {
	responsabile :: PublicKey,  -- ^ chi ha prodotto gli eventi
	eventi :: [Logico], 	-- ^ gli eventi prodotti 
	firmaR :: Firma		-- ^ la firma degli eventi contro la conoscenza usata
	} deriving (Show,Read)

data Patch = Patch {
	patches :: [RPatch], -- ^ le patch  ricevute dai responsabili
	firma :: Firma -- ^ la controfirma del sincronizzatore sulle rpatch contro la conoscenza
	} deriving (Show,Read)
---------------------------------------------------------------------

hash :: Show a => a -> B.ByteString
hash = B.pack . show

test :: String -> Bool -> Env ()
test s t = if t then return () else lift . Left $ s

validazionePatch :: Patch -> Env ()
validazionePatch (Patch ps f) = do
		mapM_ validazioneRPatch ps 
		Integrity c ks _ <- ask 
		test "firma superutente errata" (verify ks (hash (ps,c)) f)

validazioneRPatch  :: RPatch -> Env ()
validazioneRPatch (RPatch r es f) = do
		Integrity c _ est <- ask
		test "responsabile sconosciuto" (not $ r `elem` (M.keys $ responsabili est))
		test "firma responsabile errata" (verify r (hash (es,c)) f)

mkRPatch :: [Logico] -> PublicKey -> PrivateKey -> Env RPatch
mkRPatch es pu pr = do 	c <- conoscenza <$> ask
			liftM2 (>>) validazioneRPatch return $ 
				RPatch pu es . sign pr $ hash (es,c) 

mkPatch :: [RPatch] -> PrivateKey -> Env Patch
mkPatch ps pr = do 	c <-  conoscenza <$> ask
			liftM2 (>>) validazionePatch return $ 
				Patch ps . sign pr $ hash (ps,c) 

---------------------------------------------------------------------------------------------------------

render :: Conoscenza -> Eventi
render (Base m us) = []
render (Conoscenza _ (Patch ps _) c) = render c ++ foldr1 mix zs where
	zs = map (liftM2 zip (repeat . responsabile) eventi) ps
	mix [] [] = []
	mix [] ys = ys
	mix xs [] = xs
	mix xs@(x@(_,ex):xt) ys@(y@(_,ey):yt) = if ex <= ey then x : mix xt ys else y : mix xs yt

----------------------------------------------------------------------------------------


			
