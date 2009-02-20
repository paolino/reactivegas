module CSV where

import qualified Text.CSV

import Eventi

class Serialize a where
	toCSV :: a -> [String]
	fromCSV :: [String] -> a
	fromCSVs ::[[String]] -> [a]
	toCSVs :: [a] -> [[String]]
instance Serialize Evento where
	fromCSV  = read . unwords
	toCSV (Membro r u) = ["Membro", show r, show u]
	toCSV (Responsabile r u pk) = ["Responsabile", show r, show u, show pk]
	toCSV (Apertura r o) = ["Apertura", show r, show o]
	toCSV (Chiusura r o) = ["Chiusura", show r, show o]
	toCSV (Accredito r u v) = ["Accredito", show r, show u , show v]
	toCSV (Richiesta r u v o) = ["Spesa", show r ,show u, show v, show o]
	toCSV (Saldo r u v) = ["Saldo", show r, show u , show v]
	fromCSVs = map fromCSV . filter (not . null . head) . filter (not . null)
	toCSVs = map toCSV

----- hacking prova.csv -----
test () = do
 	Right t <- Text.CSV.parseCSVFromFile "prova.csv"
	return  $ fromCSVs t :: IO Storia
