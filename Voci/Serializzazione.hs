{-# LANGUAGE FlexibleInstances, GADTs , StandaloneDeriving, ScopedTypeVariables, FlexibleContexts, UndecidableInstances#-}
module Voci.Serializzazione where


import Data.Typeable
import Data.List (lookup)
import Numeric (showFFloat)
import Lib.Units
import Voci.Quantita
import Voci.Data
import Voci.Beni


instance Show a => Show (Bene a b) where
	show (Pesato x) = show ("P",x)
	show (Volumato x) = show ("V",x)
	show (Contato x) = show ("C",x)

instance Read a => Read (Bene a Pesi) where
	readsPrec _ x = case reads x of
		[(("P",x),r)] -> [(Pesato x,r)]
		_ -> []
instance Read a => Read (Bene a Volumi) where
	readsPrec _ x = case reads x of
		[(("V",x),r)]-> [(Volumato x,r)]
		_ -> []
instance Read a => Read (Bene a Unità) where
	readsPrec _ x = case reads x of
		[(("C",x),r)] -> [(Contato x,r)]
		_ -> []


instance Show (Contenitore b) where	
	show (Brick 	x) = show ("Brick",x)
	show (Bottiglia	x) = show ("Bottiglia",x)
	show (Flacone	x) = show ("Flacone",x)
	show (Damigiana	x) = show ("Damigiana",x)
	show (Pacchetto	x) = show ("Pacchetto",x)
	show (Sacco 	x) = show ("Sacco",x)
	show (Sacchetto	x) = show ("Sacchetto",x)
	show (Cassetta	x) = show ("Cassetta",x)
	show (Scatola	x) = show ("Scatola",x)
	show (Plateau	x) = show ("Plateau",x)
	show (Scatolone	x) = show ("Scatolone",x)


restore :: Read a => (String,a -> b) -> ReadS b
restore z x = case reads x of
	[(x,r)] -> case fst x /= fst z of
		True -> []
		False -> [(snd z $ snd x,r)]
	_ -> []

f `alt` g = \x -> let fx = f x in if null fx then g x else fx where
restore1 :: forall  a b . Read a => [(String,a -> b)] -> ReadS b
restore1 xs x =  case reads x of
	[(x,r)] -> case lookup (fst x) xs of
		Nothing -> []
		Just y -> [(y $ snd x,r)]
	_ -> []

instance Read (Contenitore Volumi) where
	readsPrec _ = restore1 [("Brick",Brick),("Flacone",Flacone),("Damigiana",Damigiana),("Bottiglia",Bottiglia)]

instance Read (Contenitore Pesi) where
	readsPrec _  = restore1 [("Pacchetto",Pacchetto),("Sacco",Sacco),("Sacchetto",Sacchetto),("Cassetta",Cassetta)]

instance Read (Contenitore Unità) where
	readsPrec _  = restore1 [("Scatola",Scatola), ("Scatolone",Scatolone), ("Plateau",Plateau)]


deriving instance Show b => Show (Confezionamento b)
deriving instance (Read (Contenitore b), Read b) => Read (Confezionamento b)

instance (Show c, Show a) => Show (Voce a b c d) where
	show (AllaConfezione c b q) = show ("AllaConfezione",(c,b,q))
	show (AlPesoConfezionato c b q) = show ("AlPesoConfezionato",(c,b,q))
	show (AlVolumeConfezionato c b q) = show ("AlVolumeConfezionato",(c,b,q))
	show (AlPezzo b q) = show ("AlPezzo",(b,q))
	show (AlPeso b q) = show ("AlPeso",(b,q))
	show (AlPezzoStimato b dq q) = show ("AlPezzoStimato",(b,dq,q))
	show (AllaConfezioneStimata c b dq q) = show ("AllaConfezioneStimata",(c,b,dq,q))

instance (Read a) => Read (Voce a Volumi Volumi Confezionato)	where
	readsPrec _ = restore ("AllaConfezione", (\(c,b,q) -> AllaConfezione c b q)) `alt`
			restore ("AlVolumeConfezionato", (\(c,b,q) -> AlVolumeConfezionato c b q))
instance (Read a) => Read (Voce a Pesi Pesi Confezionato)	where
	readsPrec _ = restore ("AllaConfezione", (\(c,b,q) -> AllaConfezione c b q)) `alt`
			restore ("AlPesoConfezionato", (\(c,b,q) -> AlPesoConfezionato c b q))

instance (Read a) => Read (Voce a Unità Unità Confezionato)	where
	readsPrec _ = restore ("AllaConfezione", (\(c,b,q) -> AllaConfezione c b q))
instance (Read a) => Read (Voce a Unità Pesi Confezionato)	where
	readsPrec _ = restore ("AllaConfezioneStimata", (\(c,b,dq,q) -> AllaConfezioneStimata c b dq q))
instance (Read a) => Read (Voce a Unità Pesi Sfuso)	where
	readsPrec _ = restore ("AlPezzoStimato", (\(b,dq,q) -> AlPezzoStimato b dq q))
instance (Read a) => Read (Voce a Unità Unità Sfuso)	where
	readsPrec _ = restore ("AlPezzo", (\(b,q) -> AlPezzo b q))
instance (Read a) => Read (Voce a Pesi Pesi Sfuso)	where
	readsPrec _ = restore ("AlPeso", (\(b,q) -> AlPeso b q))

instance (Show c, Show a) => Show (Ordine a b c d) where
	-- | in denaro si può ordinare tutto
	show (InDenaro q v) = show ("InDenaro",(q,v))
	show (InConfezioni q v) = show ("InConfezioni",(q,v))
	show (InPezzi q v) = show ("InPezzi",(q,v))
	show (InPeso q v) = show ("InPeso",(q,v))
	show (InVolume q v) = show ("InVolume",(q,v))

instance Read a => Read (Ordine a Volumi Volumi Confezionato) where
	readsPrec _ = restore ("InVolume", uncurry InVolume) `alt` 
			restore ("InConfezioni", uncurry InConfezioni) `alt`
				restore ("InDenaro", uncurry InDenaro)
instance Read a => Read (Ordine a Pesi Pesi Confezionato) where
	readsPrec _ = restore ("InPeso", uncurry InPeso) `alt`
			 restore ("InConfezioni", uncurry InConfezioni) `alt`
				restore ("InDenaro", uncurry InDenaro)
instance Read a => Read (Ordine a Unità Unità Confezionato) where
	readsPrec _ =	 restore ("InConfezioni", uncurry InConfezioni) `alt`
				restore ("InDenaro", uncurry InDenaro)
instance Read a => Read (Ordine a Unità Unità Sfuso) where
	readsPrec _ =	 restore ("InPezzi", uncurry InPezzi) `alt`
				restore ("InDenaro", uncurry InDenaro)

instance Read a => Read (Ordine a Unità Pesi Sfuso) where
	readsPrec _ =	 restore ("InPezzi", uncurry InPezzi) `alt`
				restore ("InPeso", uncurry InPeso) `alt`
					restore ("InDenaro", uncurry InDenaro)

instance Read a => Read (Ordine a Unità Pesi Confezionato) where
	readsPrec _ =	 restore ("InConfezioni", uncurry InConfezioni) `alt`
				restore ("InPeso", uncurry InPeso) `alt`
					restore ("InDenaro", uncurry InDenaro)
instance Read a => Read (Ordine a Pesi Pesi Sfuso) where
	readsPrec _ = restore ("InPeso", uncurry InPeso) `alt`
			restore ("InDenaro", uncurry InDenaro)

instance Show (BWord a) where
	show (PWord x) = show ("PWord",x)
	show (VWord x) = show ("VWord",x)
	show (UWord x) = show ("UWord",x)

instance Read (BWord Pesi) where
	readsPrec _ = restore ("PWord", PWord)
instance Read (BWord Volumi) where
	readsPrec _ = restore ("VWord", VWord)
instance Read (BWord Unità) where
	readsPrec _ = restore ("UWord", UWord)

