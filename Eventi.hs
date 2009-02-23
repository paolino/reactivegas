{-# LANGUAGE StandaloneDeriving #-}
module Eventi where

import Control.Monad.State 
import Control.Monad.Error
import Control.Monad.Writer
import Control.Arrow
import Data.Maybe
import Data.List
import Control.Applicative
import Data.Char

import qualified Text.ParserCombinators.ReadP as P
import qualified Data.Set as S
import qualified Data.Map as M

import Codec.Crypto.RSA (PublicKey)
import Text.CSV (parseCSV, printCSV)
import Lib (injectM)

import Fields
--------------------------------------------------------------------------------
data Evento
	= Accredito 	{tempo :: Tempo, responsabile :: User, membro :: User , valore :: Valore, firma :: Firma} 
	| Richiesta 	{tempo :: Tempo, responsabile :: User, membro :: User, bene :: Bene, valore :: Valore, firma :: Firma}	
	| Apertura 	{tempo :: Tempo, responsabile :: User, bene :: Bene, tempo :: Tempo , firma :: Firma}
	| Membro 	{tempo :: Tempo, responsabile :: User, membro :: User, firma :: Firma}
	| Saldo 	{tempo :: Tempo, responsabile :: User, membro :: User, valore :: Valore, firma :: Firma}
	| Responsabile 	{tempo :: Tempo, responsabile :: User, membro :: User, publicKey :: PublicKey, firma :: Firma}
	| Chiusura 	{tempo :: Tempo, responsabile :: User, bene :: Bene, firma :: Firma}
	| Fallimento 	{tempo :: Tempo, responsabile :: User, bene :: Bene, firma :: Firma}
	| Bootstrap 	{membro :: User, publicKey :: PublicKey}

instance Read Evento where
	readsPrec _ s = case parseCSV "read instance" s of
		Right (("Bootstrap":n:[pk]):_) -> [(Bootstrap (read n) (read pk),"")]
		Right (("Richiesta":t:r:m:b:v:f:[]):_) -> [(Richiesta (read t) (read r) (read m) (read b) (read v) (read f),"")]
		Right (("Responsabile":t:r:m:pk:f:[]):_) -> [(Responsabile (read t) (read r) (read m) (read pk) (read f), "")]
		Right (("Membro":t:r:m:f:[]):_) -> [(Membro (read t) (read r) (read m) (read f), "")]
		Right (("Apertura":t:r:o:q:f:[]):_) -> [(Apertura (read t) (read r) (read o) (read q) (read f), "")]
		Right (("Accredito":t:r:m:v:f:[]):_) -> [(Accredito (read t) (read r) (read m) (read v) (read f), "")]
		Right (("Chiusura":t:r:o:f:[]):_) -> [(Chiusura (read t) (read r) (read o) (read f), "")]
		Right (("Fallimento":t:r:o:f:[]):_) -> [(Fallimento (read t) (read r) (read o) (read f),"")]
		Right (("Saldo":t:r:m:v:f:[]):_) -> [(Saldo (read t) (read r) (read m) (read v) (read f),"")]
		Left t -> error $ show t
instance Show Evento where		
	show (Bootstrap u pk) = (init . printCSV. return) ["Bootstrap", show u, show pk]
	show x =  (init . printCSV. return) (showU x ++ [show . firma $ x])

showU (Membro t r u f) 		=  ["Membro", show t, show r, show u]
showU (Responsabile t r u pk f) =  ["Responsabile", show t, show r, show u, show pk]
showU (Apertura t r o q f) 	=  ["Apertura", show t, show r, show o, show q]
showU (Chiusura t r o f) 	=  ["Chiusura", show t, show r, show o]
showU (Accredito t r u v f) 	=  ["Accredito", show t, show r, show u , show v]
showU (Richiesta t r u v o f) 	=  ["Richiesta", show t, show r ,show u, show v, show o]
showU (Saldo t r u v f) 	=  ["Saldo", show t, show r, show u , show v]
showU (Bootstrap m pk) 		=  ["Bootstrap", show m, show pk]
	
-----------------------------------------------------------------------------------------
	
type Conoscenza = [Evento]

data Report  = Fallito {beneR :: Bene} | Successo {beneR :: Bene} deriving (Eq,Ord,Show,Read)

tuttibeni :: Estratto -> S.Set Bene
tuttibeni (Estratto as cs _ _ _ _) = S.union (S.fromList $ M.keys as) (S.map beneR cs)

data Estratto = Estratto {
	aperti :: M.Map Bene [Evento],
	chiusi :: S.Set Report,
	conti_membri :: M.Map User Valore, 
	conti_responsabili :: M.Map User Valore,
	membri :: S.Set User, 
	responsabili ::  M.Map User PublicKey
	} deriving (Read,Show)

type Responso = Either (Evento,String) Evento

type Validatore = Evento -> Estratto -> Writer [Responso] Estratto

type Componente = Evento -> Estratto -> Either String Estratto

validaEvento :: [Componente] -> Validatore
validaEvento vs e s = case injectM s (map ($e) vs) of 	
			Left t -> tell [Left (e,t)] >> return s
			Right s' -> tell [Right e] >> return s'

valida :: Validatore -> Conoscenza -> (Estratto,[Responso])
valida v xs = runWriter $ injectM vuoto (map v xs) where
	vuoto = Estratto M.empty S.empty M.empty M.empty S.empty M.empty


