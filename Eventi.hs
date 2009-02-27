{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances #-}
module Eventi where

import Control.Monad.State 
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

import Codec.Crypto.RSA (PublicKey)
import Text.CSV (parseCSV, printCSV)
import Lib (injectM)

import Fields
--------------------------------------------------------------------------------

data Logico
	= Membro 	{membro :: User}
	| Responsabile	{membro :: User}
	| Apertura 	{bene :: Bene }
	| Accredito 	{membro :: User , valore :: Valore} 
	| Richiesta 	{membro :: User, bene :: Bene, valore :: Valore}	
	| Chiusura 	{bene :: Bene}
	| Fallimento 	{bene :: Bene}
	| Saldo 	{responsabile :: Responsabile , valore :: Valore} deriving (Eq,Ord, Show, Read)

type Evento = (Responsabile, Logico)
type Conoscenza = [Evento]

data Report  = Fallito {beneR :: Bene} | Successo {beneR :: Bene} deriving (Eq,Ord,Show,Read)

tuttibeni :: Estratto -> S.Set Bene
tuttibeni est = S.union (S.fromList $ M.keys (aperti est)) (S.map beneR (chiusi est))

data Estratto = Estratto {
	aperti :: M.Map Bene [Logico],
	chiusi :: S.Set Report,
	conti_membri :: M.Map User Valore, 
	conti_responsabili :: M.Map Responsabile Valore,
	membri :: S.Set User, 
	responsabili ::  M.Map Responsabile User
	} deriving (Read,Show)

type Responso = Either (Evento,String) Evento

type Validatore = Evento -> Estratto -> Writer [Responso] Estratto

type Componente = Evento -> Estratto -> Either String Estratto

validaEvento :: [Componente] -> Validatore
validaEvento vs e s = case injectM s (map ($e) vs) of 	
			Left t -> tell [Left (e,t)] >> return s
			Right s' -> tell [Right e] >> return s'

valida :: Validatore -> Conoscenza -> (Estratto,[Responso])
valida v xs = runWriter $ injectM vuoto (map v xs) 

vuoto = Estratto M.empty S.empty M.empty M.empty S.empty M.empty

