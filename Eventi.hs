{-# LANGUAGE StandaloneDeriving #-}
module Eventi where

import Control.Monad.State 
import Codec.Crypto.RSA
import Control.Arrow
import Data.Maybe
import Data.List
import Control.Applicative
import Data.Char
import qualified Text.ParserCombinators.ReadP as P
import Text.CSV
import Data.Set as S
import Data.Map as M
import Data.ByteString.Lazy.Char8
import qualified Text.CSV


import Crypto 
import Fields

data Evento
	= Accredito 	{tempo :: Tempo, resp :: User, membro :: User , valore :: Valore, firma :: Firma} -- ^ il firmante ha ricevuto denaro
	| Richiesta 	{tempo :: Tempo, resp :: User, membro :: User, bene :: Bene, valore :: Valore, firma :: Firma}	-- ^ 
	| Apertura 	{tempo :: Tempo, resp :: User, bene :: Bene, firma :: Firma}
	| Membro 	{tempo :: Tempo, resp :: User, membro :: User, firma :: Firma}
	| Saldo 	{tempo :: Tempo, resp :: User, membro :: User, valore :: Valore, firma :: Firma}
	| Responsabile 	{tempo :: Tempo, resp :: User, membro :: User, publicKey :: PublicKey, firma :: Firma}
	| Chiusura 	{tempo :: Tempo, resp :: User, bene :: Bene, firma :: Firma}
	| Fallimento 	{tempo :: Tempo, resp :: User, bene :: Bene, firma :: Firma}
	| Bootstrap 	{membro :: User, publicKey :: PublicKey}


instance Read Evento where
	readsPrec _ s = case parseCSV "read instance" s of
		Right (("Bootstrap":n:[pk]):_) -> [(Bootstrap (read n) (read pk),"")]
		Right (("Richiesta":t:r:m:b:v:f:[]):_) -> [(Richiesta (read t) (read r) (read m) (read b) (read v) (read f),"")]
		Left t -> error $ show t
instance Show Evento where		
	show (Membro t r u f) = (printCSV. return) ["Membro", show t, show r, show u, show f]
	show (Responsabile t r u pk f) = (printCSV. return) ["Responsabile", show t, show r, show u, show pk, show f]
	show (Apertura t r o f) = (printCSV. return) ["Apertura", show t, show r, show o, show f]
	show (Chiusura t r o f) = (printCSV. return) ["Chiusura", show t, show r, show o, show f]
	show (Accredito t r u v f) = (printCSV. return) ["Accredito", show t, show r, show u , show v, show f]
	show (Richiesta t r u v o f) = (printCSV. return) ["Richiesta", show t, show r ,show u, show v, show o, show f]
	show (Saldo t r u v f) = (printCSV. return) ["Saldo", show t, show r, show u , show v, show f]
	show (Bootstrap u pk) = (printCSV. return) ["Bootstrap", show u, show pk]
	

	

type Conoscenza = [Evento]

type Chiavi = Map User PublicKey

data Report  = Fallito {beneR :: Bene} | Successo {beneR :: Bene} deriving (Eq,Ord,Show,Read)

data Beni = Beni {aperti :: Map Bene [Evento] , chiusi 	:: Set Report} deriving (Read,Show)

tuttibeni :: Beni -> Set Bene
tuttibeni (Beni as cs) = S.union (S.fromList $ M.keys as) (S.map beneR cs)

data Conti = Conti {conti_membri, conti_responsabili	:: Map User Valore} deriving (Read,Show)

type Membri = Set User
type Responsabili = Set User

data Extract = Extract {beni :: Beni, conti :: Conti, membri :: Membri, responsabili :: Responsabili} deriving (Read,Show)

emptyExtract :: Extract
emptyExtract = Extract (Beni M.empty S.empty) (Conti M.empty M.empty) S.empty S.empty

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM f [] = return ([],[])
partitionM f (x:xs) = do
	r <- f x 
	br <- partitionM f $ xs
	return . (if r then first (x:) else second (x:)) $ br

type Validator = Evento -> State Extract Bool
type Rigetto = Conoscenza
validate :: Validator -> Conoscenza -> ((Conoscenza,Rigetto),Extract)
validate v xs = runState (partitionM v xs) emptyExtract

verifica :: Validator
verifica = undefined 
