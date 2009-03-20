
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
import Codec.Binary.UTF8.String

import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Set as S
import qualified Data.Map as M
import Codec.Crypto.RSA (PublicKey, public_n)
import Lib (injectM)

import Bugs
type Responsabile = PublicKey

newtype Membro = Membro {unMembro :: String} deriving (Eq,Ord)

instance Read Membro where
	readsPrec k = map (first Membro) . readsPrec k 
instance Show Membro where
	show (Membro b) = "\"" ++ b ++ "\""

newtype Bene = Bene {unBene :: String} deriving (Eq,Ord)
instance Read Bene where
	readsPrec k = map (first Bene) . readsPrec k 
instance Show Bene where
	show (Bene b) =  "\"" ++ b ++ "\""

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
	aperti 			:: M.Map Bene (M.Map Membro Valore),
	chiusi 			:: S.Set Report,
	conti_membri 		:: M.Map Membro Valore, 
	conti_responsabili 	:: M.Map Responsabile Valore,
	membri 			:: S.Set Membro, 
	responsabili 		:: M.Map Responsabile (Maybe Membro)
	--seme			:: B.ByteString
	--sincronizzatore 	:: PublicKey
	--fiducia			:: [(Public
	-- promuovendi		:: M.Map Responsabile (M.Map Responsabile Bool),
	-- licenziandi		:: M.Map Responsabile (M.Map Responsabile Bool)
	} deriving (Read,Show)


type Responso = Either (Evento,String) Evento

type Validatore = Evento -> Estratto -> Writer [Responso] Estratto

valida :: Validatore -> Eventi -> (Estratto,[Responso])
valida v xs = runWriter $ injectM vuoto (map v xs) 

vuoto = Estratto M.empty S.empty M.empty M.empty S.empty M.empty -- M.empty M.empty


