{-# LANGUAGE FlexibleContexts #-}
module Eventi.Amministrazione where

import Data.List (delete, (\\))

import Control.Monad.Error (when, throwError)
import Control.Monad.Reader (asks)

import Lib.Firmabile (Segreto, Chiave, cryptobox)
import Lib.Aspetti (ParteDi)
import Core.Costruzione (scelte, libero, CostrAction, runSupporto)

import Eventi.Anagrafe (Anagrafe, Responsabili, Utente, costrUtenti, responsabili, utenti)


-- costrNuovoResponsabile :: (Monad m, Anagrafe `ParteDi` s, Responsabili `ParteDi` s) => CostrAction m c (Utente,(Chiave,Segreto)) s
costrNuovoResponsabile s kp kn = (,) "nuove chiavi da responsabile" . runSupporto s kn kp $ do 
	us <- asks utenti
	(rs,rs') <- asks responsabili
	let us' = us \\ (map fst $ rs ++ rs')
	when (null us') $ throwError "nessun utente che non sia già tra i responsabili, o in elezione"
	u <- scelte (zip us' us') "scegli il tuo nome di utente"
	p <- libero "immetti la tua password, una frasetta lunga possibilmente"
	return $ (u,cryptobox p)

costrGestionePatch :: (Monad m) => [String] -> CostrAction m c [String] ()
costrGestionePatch xs s kp kn = return . (,) "correzione patch" . runSupporto () kn kp $ do
	when (null xs) $ throwError "non ci sono eventi da eliminare"
	x <- scelte (zip xs xs) "seleziona evento da eliminare"
	return $ delete x xs


-- costrLogout :: [String] -> CostrAction c 
	
