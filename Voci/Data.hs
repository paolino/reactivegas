
{-# LANGUAGE GADTs,EmptyDataDecls #-}
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
	

count (Scatola x) = x
count (Scatolone x) = x
count (Plateau x) = x
-- | costruttori di confezionamenti 
data Confezionamento b = Primo (Contenitore b) | Inscatolato (Contenitore Unità) (Confezionamento b) 

-- | tag distintivo per i beni confezionati
data Confezionato

-- | tag distintivo per i beni sfusi
data Sfuso

-- | costruttori di voci. A causa della prezzatura a peso stimato per i beni unitari si aggiunge un tag di unità (c)
data Voce a b c d where
	
	-- | tutto  il confezionato
	AllaConfezione	:: Confezionamento b -> Bene a b -> Quantità (Denaro,Unità) -> Voce a b b Confezionato
	-- | gli sfusi unitari
	AlPezzo 	:: Bene a Unità -> Quantità (Denaro,Unità) -> Voce a Unità Unità Sfuso
	-- | tutti i pesi
	AlPeso	:: Bene a Pesi -> Quantità (Denaro,Pesi) -> Voce a Pesi Pesi Sfuso
	-- | unitari sia confezionati che sfusi dove il prezzo si esprime in peso e si stima il peso del bene
	AlPesoConfezionato :: Confezionamento Pesi -> Bene a Pesi -> Quantità (Denaro,Pesi) -> Voce a Pesi Pesi Confezionato
	-- | unitari sia confezionati che sfusi dove il prezzo si esprime in peso e si stima il peso del bene
	AlVolumeConfezionato :: Confezionamento Volumi -> Bene a Volumi -> Quantità (Denaro,Volumi) -> Voce a Volumi Volumi Confezionato
	-- | unitari sia confezionati che sfusi dove il prezzo si esprime in peso e si stima il peso del bene
	AlPezzoStimato	:: Bene a Unità -> (Quantità Pesi, Quantità Pesi) 
		-> Quantità (Denaro,Pesi) -> Voce a Unità Pesi Sfuso
	AllaConfezioneStimata ::  Confezionamento Unità -> Bene a Unità   
		-> (Quantità Pesi, Quantità Pesi) 
		-> Quantità (Denaro,Pesi) -> Voce a Unità Pesi Confezionato

-- | costruttori di ordine, associano una quantità ad una voce
data Ordine a b c d where
	-- | in denaro si può ordinare tutto
	InDenaro 	:: Quantità Denaro 	-> Voce a b c d -> Ordine a b c d
	-- | esclusi gli sfusi
	InConfezioni 	:: Quantità Unità 	-> Voce a b c Confezionato -> Ordine a b c Confezionato
	-- | unità sfuse, compresi gli stimati in peso
	InPezzi		:: Quantità Unità 	-> Voce a Unità c Sfuso -> Ordine a Unità c Sfuso
	-- | tutti pesi, sfusi e non , esclusi gli stimati
	InPeso	 	:: Quantità Pesi 	-> Voce a b Pesi d -> Ordine a b Pesi d
	-- | i volumi , che si commerciano solo confezionati 
	InVolume	:: Quantità Volumi -> Voce a Volumi Volumi Confezionato -> Ordine a Volumi Volumi Confezionato



