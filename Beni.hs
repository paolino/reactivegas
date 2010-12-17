{-# LANGUAGE NoMonomorphismRestriction, DeriveDataTypeable #-}
import Lib.Metrics
import Lib.Passo
import Data.List
import Data.Maybe
import Lib.Console
import Control.Monad.Trans
import Lib.Response
import Data.Typeable
import Control.Arrow
import Control.Monad.State

data Bene p = Bene {
	categoria :: [String],
	filiera :: [String],
	commercio :: Maybe p
	} deriving (Show,Read)

class Impegnabile p where 
	impegno :: p -> [(String,Dimensions,Dimensioned -> [Dimensioned])]
	
class Descrizione a where
	campi :: a -> (String,[(String,Dimensioned)])

prettyDescrizione :: Descrizione a => a -> Response
prettyDescrizione x = let
	(h,fs) = campi x
	in Response $  [("tipo",ResponseOne h)] ++ map (second (ResponseOne . prettyDimensioned)) fs




data Confezionato = Confezionato {
	prezzo :: Dimensioned, -- Denaro / x || Denaro / y
	confezionamento :: Dimensioned -- y / x
	} deriving (Show, Read, Typeable)

instance Descrizione Confezionato where
	campi (Confezionato x y) = ("confezionato",[("confezionamento",y), ("prezzo",x)])
	
instance Impegnabile Confezionato where
	impegno (Confezionato x y) 
			| denominatore x === denominatore y = 
				let  bj z = [z , z * x, z * y] in 
				[
				("confezioni", denominatore y, bj),
				("denaro", numeratore x, \z ->  let ds = dimensions (z/x) in 
					bj $ Dimensioned (fromIntegral $ floor (z/x)) ds),
				("unità di misura", numeratore y,  \z -> let ds = dimensions (z/y) in 
					bj $ Dimensioned (fromIntegral $ floor (z/y)) ds)
				]
			| denominatore x === numeratore y = 
				let  bj z = [z , z * y * x, z * y] in 
				[
				("confezioni", denominatore y, bj),
				("denaro", numeratore x, \z ->  let ds = dimensions (z/x/y) in 
					bj $ Dimensioned (fromIntegral $ floor (z/x/y)) ds),
				("unità di misura", numeratore y,  \z -> let ds = dimensions (z/y) in
					bj $ Dimensioned (fromIntegral $ floor (z/y)) ds)
				]



uiImpegnabile :: (Monad m , Impegnabile p) => p -> Costruzione m b [Dimensioned]
uiImpegnabile x = let
	qs = map (\(s,m,q) -> (s,(m,q))) $ impegno x in 
	do 
		(m,q) <- scelte qs "tipo di ordine"
		v <- libero $ "valore in " ++ bisex (plurale m)
		return $ q (Dimensioned (toRational v) m)
		 

uiConfezionato :: Monad m => Bene Confezionato -> Costruzione m b (Bene Confezionato)
uiConfezionato (Bene cs fs _) = do
	c <- scelte [
		("scatola", Dimension (1,Singolo Scatola)),
		("pacchetto", Dimension (1,Singolo Pacchetto))
		] "contenitore di tipo"
	q <- scelte [
		("peso", Dimension (1,Chilogrammo)),
		("unità", Dimension (1,Singolo Pezzo)) 
		] "bene nel contenitore misurato in"
	s <- libero $ "contenuto (" ++ bisex (plurale [q,chdim negate c]) ++ ")"
	let tr d = [Dimension (1,Euro), chdim negate d]
	r <- scelte [
		(bisex . plurale $ tr c, tr c),
		(bisex . plurale $ tr q, tr q)
		] "prezzo misurato in"
	p <- libero $ "prezzo (" ++ bisex (singolare r) ++ ")"
	return $ Bene cs fs $ Just $ 
		Confezionato (Dimensioned (toRational p) r) (Dimensioned (toRational s) [q,chdim negate c])



uiCategoria (Bene cs fs comm) = do
	l <- libero "nuova categoria"
	return (Bene (nub $ l:cs) fs comm)	
uiFiliera (Bene cs fs comm) = do
	l <- libero "nuovo attore nella filiera"
	return (Bene cs (nub $ l:fs) comm)
	
uiBene b = do
	f <- scelte [
		("aggiungi categoria", uiCategoria b >>= uiBene),
		("aggiungi filiera",uiFiliera b >>= uiBene),
		("imposta i valori di commercio",uiConfezionato b >>= uiBene),
		("fine descrizione", return b)
		] "operazione" 
	b' <- f
	output True $ mostraBene b'
	return b'
	

query = map (\(s,m,q) -> (s,m)) . impegno . commercio
	
caffe = Bene
	["caffè","solidale","macinato"]
	["Rebelde","Ya Basta","Mappamondo"]
	$ Just $ Confezionato 
		(Dimensioned 3.5 [Dimension (1,Euro),Dimension (-1,Singolo  Pacchetto)]) 
		(Dimensioned 0.25 [Dimension (1,Chilogrammo),Dimension (-1,Singolo Pacchetto)])
	
farina = Bene
	["grano", "tenero", "biologico"]
	["Barilli","Borgo val di Taro"]
	$ Just $ Confezionato 
		(Dimensioned 0.25 [Dimension (1,Euro), Dimension (-1, Chilogrammo)])
		(Dimensioned 25 [Dimension (1,Chilogrammo), Dimension (-1,Singolo Pacchetto)])

data Stato = Stato {
	beni :: [Bene Confezionato],
	ordini :: [(Bene Confezionato, [Dimensioned])]
	}

mostraBene (Bene cs ps Nothing) = ResponseOne $ intercalate ", " (cs ++ ps)
mostraBene (Bene cs ps (Just comm)) = Response [
	("categorie",ResponseOne $ intercalate ", " cs),
	("filiera",ResponseOne $ intercalate ", " ps),
	("commercio", prettyDescrizione comm)
	]

uiMain = join $  scelte [
		("aggiungi bene", do 
			b <- uiBene (Bene [] [] Nothing) 
			lift $ modify (\(Stato bs os) -> Stato (b:bs) os)
			uiMain
			),
			
		("aggiungi ordine", do 
			bs <- filter (\(Bene _ _ x) -> isJust x) `fmap` lift (gets beni)
			b <- scelte (map (\b@(Bene cs ps comm) -> (intercalate ", " (cs ++ ps),b)) bs) 
				"bene da ordinare"
			output True $ prettyDescrizione (fromJust $ commercio b)
			rs <- uiImpegnabile (fromJust $ commercio b)
			lift $ modify (\(Stato bs os) -> Stato bs ((b,rs):os))
			uiMain
			),
		("mostra ordini", do
			Stato bs os <- lift get
			output True $ Response $ map (\(Bene cs ps _,ds) ->
				(intercalate ", " (cs ++ ps), ResponseOne $ intercalate ", " $ map prettyDimensioned ds)) os
			uiMain
			),
		("mostra beni",do
			Stato bs os <- lift get
			output True $ Response $ zip (map show [1..]) (map mostraBene bs)
			uiMain
			),


		("fine",return ())
		] "operazione"
	


s0 = Stato [caffe,farina] []
main = (\x -> runStateT (svolgi x >>= interazione) s0 ) $ do
	uiMain
	  
