{-# LANGUAGE StandaloneDeriving #-}
module Eventi where

import Control.Monad.State 
import Codec.Crypto.RSA
import Data.Maybe
import Data.List
import Control.Applicative
import Data.Char
import qualified Text.ParserCombinators.ReadP as P
import Text.CSV
import Data.Set as S
import Data.Map as M

type Valore = Double

newtype User = User String deriving (Eq,Ord)
instance Read User where
	readsPrec _ = P.readP_to_S  (P.skipSpaces >> User <$> P.munch1 (isAlphaNum))
instance Show User where
	show (User u) = u
newtype Bene = Bene String deriving (Eq,Ord)
instance Show Bene where
	show (Bene o) = o
instance Read Bene where
	readsPrec _ = P.readP_to_S  (P.skipSpaces >> Bene <$> P.munch1 (isAlphaNum))

deriving instance Read PublicKey

data Evento
	= Accredito User User Valore -- ^ il firmante ha ricevuto denaro
	| Richiesta User User Bene Valore 	-- ^ 
	| Apertura User Bene 
	| Membro User User
	| Saldo User User Valore
	| Responsabile User User PublicKey
	| Chiusura User Bene 
	| Fallimento User Bene
	deriving (Read,Show)

type Storia = [Evento]


data Report  = Fallito {bene :: Bene} | Successo {bene :: Bene} deriving (Eq,Ord,Show,Read)

data Beni = Beni {aperti :: Map Bene [Evento] , chiusi 	:: Set Report} deriving (Read,Show)

tuttibeni :: Beni -> Set Bene
tuttibeni (Beni as cs) = S.union (S.fromList $ M.keys as) (S.map bene cs)

data Conti = Conti {conti_membri, conti_responsabili	:: Map User Valore} deriving (Read,Show)

type Membri = Set User
type Responsabili = Set User

data Knowledge = Knowledge {beni :: Beni, conti :: Conti, membri :: Membri, responsabili :: Responsabili} deriving (Read,Show)

emptyKnowledge :: Knowledge
emptyKnowledge = Knowledge (Beni M.empty S.empty) (Conti M.empty M.empty) S.empty S.empty

validate :: Evento -> State Knowledge Bool
validate m@(Membro _ _) = modify (step m) >> return True
validate r@(Responsabile _ _ _) = modify (step r) >> return True
validate s@(Saldo _ _ _) = modify (step s) >> return True
validate a@(Accredito _ m _) = do
	ms <- membri <$> get
	if m `S.member` ms then do	
		modify $ step a 
		return True
	 else return False
validate a@(Apertura _ o) = do
	bs <- tuttibeni . beni <$> get
	if o `S.member` bs then return False
		else modify (step a) >> return True
validate c@(Chiusura _ o) = do
	as <- aperti . beni <$> get
	if not (o `S.member` as) then return False
		else modify (step c) >> return True

validate f@(Fallimento _ o) = do
	as <- aperti . beni <$> get
	if not (o `S.member` as) then return False
		else modify (step f) >> return True
validate r@(Richiesta _ m o v) = do
	Knowledge bs@(Beni as _) cs@(Conti cms _) ms rs <- get 
	if and [m `S.member` ms, o `S.member` as, M.findWithDefault 0 m cms >= v] then do
		modify (step r) >> return True
		else return False

	
----------------------------- summarizing knowledge from a list of valid events ------------------------------------------------




mkKnowledge :: Storia -> Knowledge
mkKnowledge = foldr step emptyKnowledge

modifica :: (Valore -> Valore) -> User -> Map User Valore -> Map User Valore
modifica dv m = M.insertWith (const dv) m (dv 0)

step :: Evento -> Knowledge -> Knowledge
step (Membro _ m) 		k@(Knowledge _ _ ms _) = k{membri = m `S.insert` ms} 
step (Responsabile _ m _) 	k@(Knowledge _ _ _ rs) = k{responsabili = m `S.insert` rs} 
-- accredita sul conto  del responsabile e sul conto del membro
step (Accredito r m v) 		k@(Knowledge _ (Conti cms crs) _ _) =  k{conti = Conti (modifica (+v) m cms) (modifica (+v) r crs)}
-- accredita sul conto del primo responsabile e scredita sul secondo
step (Saldo r1 r2 v)  		k@(Knowledge _ (Conti cms crs) _ _) =  k{conti = Conti cms (modifica (flip (-) v) r2  . modifica (+v) r1 $ crs)}
step (Fallimento _ o) 		k@(Knowledge (Beni vs as) _ _ _ ) = k{beni = Beni vs (S.insert (Fallito o) as)}
step (Chiusura r o)  		k@(Knowledge (Beni vs as) _ _ _ ) = k{beni = Beni vs (S.insert (Successo o) as)}
step (Apertura r o)  		k@(Knowledge (Beni vs as) _ _ _ ) = 
	if Fallito o `S.member` as || Successo o `S.member` as then k else k{beni = Beni (S.insert o vs) as}
step (Richiesta r m o v) 	k@(Knowledge b@(Beni vs as) (Conti cms crs)  _ _ ) 
	| Fallito o `S.member` as = k
	| Successo o `S.member` as = k{conti = Conti (modifica (flip (-) v) m cms) (modifica (flip (-) v) r crs)}
	| otherwise = k{conti = Conti (modifica (flip (-) v) m cms) crs} -- denaro impegnato ma non ancora speso

