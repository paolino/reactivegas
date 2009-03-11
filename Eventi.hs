
{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances #-}
module Eventi where

import Control.Monad.Error
import Control.Monad.Writer
import Control.Arrow
import Data.Maybe
import Data.List
import Control.Applicative
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as B

import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Set as S
import qualified Data.Map as M
import Codec.Crypto.RSA (PublicKey, public_n)
import Lib (injectM)

import Bugs
type Responsabile = PublicKey
prettyResponsabile = take 6 . show . public_n
newtype Membro = Membro {unMembro :: String} deriving (Eq,Ord)
instance Read Membro where
	readsPrec _ = P.readP_to_S  (P.skipSpaces >> Membro <$> P.munch1 (isAlphaNum))

instance Show Membro where
	show (Membro u) = u
newtype Bene = Bene {unBene :: String} deriving (Eq,Ord)
instance Show Bene where
	show (Bene o) = o
instance Read Bene where
	readsPrec _ = P.readP_to_S  (P.skipSpaces >> Bene <$> P.munch1 (isAlphaNum))

type Valore = Double
--------------------------------------------------------------------------------

data Logico
	= Dissenso	{responsabile :: Responsabile} 
	| Assenso	{responsabile :: Responsabile}
	| Nick		{membro :: Membro}
	| Novizio 	{membro :: Membro}
	| Apertura 	{bene :: Bene }
	| Accredito 	{membro :: Membro , valore :: Valore} 
	| Richiesta 	{membro :: Membro, bene :: Bene, valore :: Valore}	
	| Chiusura 	{bene :: Bene}
	| Fallimento 	{bene :: Bene}
	| Saldo 	{responsabile :: Responsabile , valore :: Valore} 
	| Promozione	{responsabile :: Responsabile}
	| Licenziamento	{responsabile :: Responsabile} deriving (Eq,Ord, Show, Read)


type Evento = (Responsabile, Logico)
type Eventi = [Evento]

data Report  = Fallito {beneR :: Bene} | Successo {beneR :: Bene} deriving (Eq,Ord,Show,Read)

tuttibeni :: Estratto -> S.Set Bene
tuttibeni est = S.union (S.fromList $ M.keys (aperti est)) (S.map beneR (chiusi est))

data Estratto = Estratto {
	aperti 			:: M.Map Bene [Logico],
	chiusi 			:: S.Set Report,
	conti_membri 		:: M.Map Membro Valore, 
	conti_responsabili 	:: M.Map Responsabile Valore,
	membri 			:: S.Set Membro, 
	responsabili 		:: M.Map Responsabile (Maybe Membro),
	promuovendi		:: M.Map Responsabile (M.Map Responsabile Bool),
	licenziandi		:: M.Map Responsabile (M.Map Responsabile Bool)
	} deriving (Read,Show)

type Responso = Either (Evento,String) Evento

type Validatore = Evento -> Estratto -> Writer [Responso] Estratto

valida :: Validatore -> Eventi -> (Estratto,[Responso])
valida v xs = runWriter $ injectM vuoto (map v xs) 

vuoto = Estratto M.empty S.empty M.empty M.empty S.empty M.empty M.empty M.empty

