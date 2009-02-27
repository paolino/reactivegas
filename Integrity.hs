{-# LANGUAGE StandaloneDeriving,TypeSynonymInstances #-}

import Control.Monad.Error
import Data.List ((\\))
--import System.Random

import qualified Data.ByteString.Lazy.Char8 as B (pack,ByteString)

import Codec.Crypto.RSA

type Firma = B.ByteString
type User = PublicKey

deriving instance Read User
-- | un responsabile crea una patch con la sua chiave pubblica , un lista di eventi, e una firma degli eventi, 
-- concatenati con la conoscenza visionata
data RPatch a = RPatch {
	responsabile :: User, 
	eventi :: [a], 
	firmaR :: Firma
	} deriving (Show,Read)

data Patch a = Patch {
	patches :: [RPatch a], 
	nuovi :: [User],
	firma :: Firma 
	} deriving (Show,Read)

data Conoscenza a = Conoscenza (Patch a) (Conoscenza a) | Base User deriving (Show,Read)

data Chiavi = Chiavi {master :: User, responsabili :: [User]} deriving Show

deriving instance Eq User

hash :: Show a => a -> B.ByteString
hash = B.pack . show

test :: String -> Bool -> Either String ()
test s t = if t then Right () else Left s

validazioneS :: Show a => Conoscenza a -> Either String Chiavi
validazioneS (Base u) = return (Chiavi u [])
validazioneS (Conoscenza (Patch ps us f) c) = do
		Chiavi ks us' <- validazioneRs ps c
		test "firma superutente errata" (verify ks f $ hash ((ps,us),c))
		Right $ Chiavi ks (us' ++ us)

validazioneRs :: Show a => [RPatch a] -> Conoscenza a -> Either String Chiavi
validazioneRs ps c = do
		let 	u (RPatch r es f) = (r, verify r f $ hash (es,c))
			(rs,ts) = unzip . map u $ ps 
		Chiavi ks us <- validazioneS c
		test "responsabili sconosciuti" (null $ rs \\ us)
		test "firme responsabili errate" (and ts)
		Right $ Chiavi ks us

mkRPatch :: Show a => [a] -> User -> PrivateKey -> Conoscenza a -> RPatch a
mkRPatch es u ks c = RPatch u es . sign ks $ hash (es,c)

mkPatch :: Show a => [RPatch a] -> [User] -> PrivateKey -> Conoscenza a -> Patch a
mkPatch ps us ks c = Patch ps us . sign ks $ hash ((ps,us),c)

render :: Ord a => Conoscenza a -> [(User,a)]
render (Base _) = []
render (Conoscenza (Patch ps _ _) c) = render c ++ foldr1 mix zs where
	zs = map (liftM2 zip (repeat . responsabile) eventi) ps
	mix [] [] = []
	mix [] ys = ys
	mix xs [] = xs
	mix xs@(x@(_,ex):xt) ys@(y@(_,ey):yt) = if ex <= ey then x : mix xt ys else y : mix xs yt
