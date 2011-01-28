module Voci.Language where

import Lib.NaturalLanguage 

---------------------------------- parte specifica del linguaggio per le descrizioni ------------

data WSfuso = WSfuso
instance Polimorfo WSfuso where
	singolareA WSfuso (Maschile x) = Maschile $ x ++ " sfuso"
	singolareA WSfuso (Femminile x) = Femminile $ x ++ " sfusa"
	pluraleA WSfuso (Maschile x) = Maschile $ x ++ " sfusi"
	pluraleA WSfuso (Femminile x) = Femminile $ x ++ " sfuse"

data Misurato = Misurato 
instance Polimorfo Misurato where
	singolareA Misurato (Maschile x) = Maschile $ x ++ " misurato"
	singolareA Misurato (Femminile x) = Femminile $ x ++ " misurata"
	pluraleA Misurato (Maschile x) = Maschile $ x ++ " misurati"
	pluraleA Misurato (Femminile x) = Femminile $ x ++ " misurate"

data Contenuto = Contenuto 
instance Polimorfo Contenuto where
	singolareA Contenuto (Maschile x) = Maschile $ x ++ " contenuto"
	singolareA Contenuto (Femminile x) = Femminile $ x ++ " contenuta"
	pluraleA Contenuto (Maschile x) = Maschile $ x ++ " contenuti"
	pluraleA Contenuto (Femminile x) = Femminile $ x ++ " contenute"

data Contenente = Contenente 
instance Polimorfo Contenente where
	singolareA Contenente (Maschile x) = Maschile $ x ++ " contenente"
	singolareA Contenente (Femminile x) = Femminile $ x ++ " contenente"
	pluraleA Contenente (Maschile x) = Maschile $ x ++ " contenenti"
	pluraleA Contenente (Femminile x) = Femminile $ x ++ " contenenti"

data Rosso = Rosso 
instance Polimorfo Rosso where
	singolareA Rosso (Maschile x) = Maschile $ x ++ " rosso"
	singolareA Rosso (Femminile x) = Femminile $ x ++ " rossa"
	pluraleA Rosso (Maschile x) = Maschile $ x ++ " rossi"
	pluraleA Rosso (Femminile x) = Femminile $ x ++ " rosse"

