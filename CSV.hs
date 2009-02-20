module CSV where

----- hacking prova.csv -----
test () = do
 	Right t <- Text.CSV.parseCSVFromFile "prova.csv"
	return  $ map read t :: IO Conoscenza
