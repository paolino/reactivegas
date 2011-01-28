
{-# LANGUAGE GADTs #-}
module Voci.Data where

import Lib.Units (Pesi,Volumi,Unità, Denaro)
import Voci.Quantita (Quantità)

-- | costruttori di beni, distinti per unita' di descrizione
data Bene a b where
	Pesato 		:: a -> Bene a Pesi
	Volumato 	:: a -> Bene a Volumi
	Contato 	:: a -> Bene a Unità

-- | costruttori di contenitore, ognuno con il suo contenuto nominale, distinti per tipo di unita' 
data Contenitore b where 
	Brick 		:: Quantità Volumi 	-> Contenitore Volumi
	Bottiglia 	:: Quantità Volumi 	-> Contenitore Volumi
	Flacone 	:: Quantità Volumi 	-> Contenitore Volumi 
	Damigiana 	:: Quantità Volumi 	-> Contenitore Volumi
	Pacchetto 	:: Quantità Pesi 	-> Contenitore Pesi
	Sacco 		:: Quantità Pesi 	-> Contenitore Pesi
	Sacchetto 	:: Quantità Pesi 	-> Contenitore Pesi
	Cassetta 	:: Quantità Pesi 	-> Contenitore Pesi
	Scatola 	:: Quantità Unità 	-> Contenitore Unità
	Plateau 	:: Quantità Unità 	-> Contenitore Unità
	Scatolone 	:: Quantità Unità 	-> Contenitore Unità
	
-- | tag distintivo per i beni confezionati
data Confezionato

-- | tag distintivo per i beni sfusi
data Sfuso

-- | costruttori di confezionamenti , distinti per unita' e per tag di sfuso-confezionato. Inscatolato puo' inscatolare ricorsivamente
data Confezionamento b d where
	-- | sfuso unitario
	UnitariaSfuso 	:: Confezionamento Unità Sfuso
	-- | sfuso in peso
	SolidoSfuso 	:: Confezionamento Pesi Sfuso
	-- | primo confezionamento bene unitario
	UnitariaConfezionato 	:: Contenitore Unità -> Confezionamento Unità Confezionato
	-- | primo confezionamento bene in peso
	SolidoConfezionato 	:: Contenitore Pesi ->  Confezionamento Pesi Confezionato
	-- | primo confezionamento bene in volume
	Liquido 	:: Contenitore Volumi -> Confezionamento Volumi Confezionato
	-- | ulteriore confezionamento in unita' di qualsiasi primo confezionamento
	Inscatolato 	:: Contenitore Unità -> Confezionamento b Confezionato -> Confezionamento b Confezionato

-- | sfoglia la cipolla di confezionamento, valida solo per i confezionati
explode :: Confezionamento b Confezionato -> [Confezionamento b Confezionato]
explode u@(UnitariaConfezionato _ ) = [u]
explode s@(SolidoConfezionato _ ) = [s]
explode l@(Liquido _ ) = [l]
explode i@(Inscatolato _  c) = i: explode c


-- | costruttori di voci. A causa della prezzatura a peso stimato per i beni unitari si aggiunge un tag di unità (c)
data Voce a b c d where
	-- | tutto  il confezionato
	AllaConfezione	:: Confezionamento b Confezionato -> Bene a b -> Quantità Denaro -> Voce a b b Confezionato
	-- | gli sfusi unitari
	AlPezzo 	:: Confezionamento Unità Sfuso -> Bene a Unità -> Quantità Denaro -> Voce a Unità Unità Sfuso
	-- | tutti i pesi
	AlPeso 		:: Confezionamento Pesi d -> Bene a Pesi -> Quantità (Denaro,Pesi) -> Voce a Pesi Pesi d
	-- | i volumi confezionati
	AlVolume 	:: Confezionamento Volumi Confezionato -> Bene a Volumi -> Quantità (Denaro,Volumi) 
				-> Voce a Volumi Volumi Confezionato
	-- | unitari sia confezionati che sfusi dove il prezzo si esprime in peso e si stima il peso del bene
	AlPesoStimato	:: Confezionamento Unità d -> Bene a Unità -> (Quantità Pesi, Quantità Pesi) 
				-> Quantità (Denaro,Pesi) -> Voce a Unità Pesi d

-- | costruttori di ordine, associano una quantità ad una voce
data Ordine a b c d where
	-- | in denaro si può ordinare tutto
	InDenaro 	:: Quantità Denaro 	-> Voce a b c d -> Ordine a b c d
	-- | esclusi gli sfusi
	InConfezioni 	:: Quantità Unità 	-> Voce a b c Confezionato -> Ordine a b c Confezionato
	-- | unità sfuse, compresi gli stimati in peso
	InPezzi		:: Quantità Unità 	-> Voce a Unità c Sfuso -> Ordine a Unità c Sfuso
	-- | tutti pesi, sfusi e non , esclusi gli stimati
	InPeso	 	:: Quantità Pesi 	-> Voce a Pesi Pesi d -> Ordine a Pesi Pesi d
	-- | i volumi , che si commerciano solo confezionati 
	InVolume	:: Quantità Volumi -> Voce a Volumi Volumi Confezionato -> Ordine a Volumi Volumi Confezionato


data BWord a where
	PWord :: Molteplicita Word -> BWord Pesi
	VWord :: Molteplicita Word -> BWord Volumi
	UWord :: (Word,Word) -> BWord Unità
	 
type BBene b = Bene (BWord b) b
type BScaffale b = Scaffale (BWord b) b
