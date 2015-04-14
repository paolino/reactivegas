{-# LANGUAGE GADTs #-}

module Voci.Beni where
import Prelude hiding (Word)
import Voci.Data
import Lib.NaturalLanguage
import Lib.Units -- (Pesi,Volumi,Unità, Denaro,UnitClass (..))

------------------------------------ datatype reale dei nomi dei beni -------------------------
data BWord a where
	PWord :: Molteplicita Word -> BWord Pesi
	VWord :: Molteplicita Word -> BWord Volumi
	UWord :: (Word,Word) -> BWord Unità

fromPesato :: BBene Pesi -> Molteplicita Word
fromPesato (Pesato (PWord x)) = x	 

fromVolumato :: BBene Volumi -> Molteplicita Word
fromVolumato (Volumato (VWord x)) = x	 

type BBene b = Bene (BWord b) b
type BVoce b c d = Voce (BWord b) b c d
type BOrdine b c d = Ordine (BWord b) b c d

