

{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, FlexibleContexts, Rank2Types, ScopedTypeVariables, GADTs, ExistentialQuantification, StandaloneDeriving,UndecidableInstances #-}
module Eventi.Voci where

import Lib.Units -- (Pesi,Volumi,Unità, Denaro,UnitClass (..))
import Lib.NaturalLanguage
import Lib.QInteger
import Eventi.Servizio

import Voci.Core
import Voci.Instances

type Indice = QInteger


data EsternoVoci a = NuovaVoce Voce | EliminaVoce Indice


