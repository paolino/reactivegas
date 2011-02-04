{-# LANGUAGE Rank2Types, FlexibleContexts, ExistentialQuantification #-}

import Voci.Data
import Lib.NaturalLanguage
import Lib.Units
import Voci.Quantita
import Voci.Serializzazione
import Voci.Names
import Voci.Beni
import Voci.Boxes
import Voci.Ordini
import Data.Typeable
import Debug.Trace

caffèBene = Pesato (PWord $ Singolare (Maschile "caffè"))
caffèVoce = AllaConfezione (Primo $ Pacchetto $ 250 :? Grammo) caffèBene (toRational 2.9 :? (Euro,Unità)) 
caffèOrdinePeso = InPeso (toRational 2.5 :? Chilogrammo) caffèVoce
caffèOrdineDenaro = InDenaro (35 :? Euro) caffèVoce
caffèOrdineConfezioni = InConfezioni (12 :? Unità) caffèVoce

inPeso :: (Valuta b Pesi d Pesi, VoceOf (BWord b) b Pesi d) => BOrdine b Pesi d -> Maybe (BOrdine b Pesi d)
inPeso x = flip InPeso (voceOf x) `fmap` valuta x 
inDenaro :: (Valuta b c d Denaro, VoceOf (BWord b) b c d) => BOrdine b c d -> Maybe (BOrdine b c d)
inDenaro x = flip InDenaro (voceOf x) `fmap` valuta x 
inConfezioni x = flip InConfezioni (voceOf x) `fmap` valuta x
inPezzi x = flip InPezzi (voceOf x) `fmap` valuta x
inVolume x = flip InVolume (voceOf x) `fmap` valuta x

inQualcosa f x = flip f (voceOf x) `fmap` valuta x

data BoxQ = forall a. UnitClass a => BoxQ (Quantità a)

-- mkBoxQ f (BoxOrdine x) = BoxQ (f x)
main = mapM_ (putStrLn . render . singolare) [
	BoxOrdine caffèOrdinePeso, BoxOrdine caffèOrdineConfezioni, BoxOrdine caffèOrdineDenaro
	]
	

