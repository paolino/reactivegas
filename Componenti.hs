{-# LANGUAGE StandaloneDeriving #-}
module Componenti where

import Control.Monad.Writer (tell)
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S

import Eventi
import Bugs
import Lib (modifica, injectM)

--------------------------------------------------------------------------------------------------

bool t f x = if x then t else f

type Componente = Evento -> Estratto -> Either String Estratto

setResponsabile pk m est = est {responsabili = M.insert pk (Just m) (responsabili est)}

controlloEsistenzaResponsabile pk est = bool (Right est) (Left "il responsabile non esiste") $ pk `M.member` responsabili est 

controlloNonEsistenzaResponsabile pk est = bool (Left "il responsabile esiste") (Right est) $ pk `M.member` responsabili est

nuovoMembro m est = est {membri = S.insert m (membri est)}

controlloMembro m est = bool (Right est) (Left $ "il membro " ++ show m ++ " e' sconosciuto") $ S.member m (membri est) 

vIdentita :: Componente
vIdentita (_,Novizio m ) = Right . nuovoMembro m 
vIdentita (r,Nick m) =  Right . setResponsabile r m
vIdentita (_,Accredito m _ ) = controlloMembro m 
vIdentita (_,Richiesta m _ _) = controlloMembro m
vIdentita (_,Saldo r _) = controlloEsistenzaResponsabile r
vIdentita _ = Right

-----------------------------------------------------------------------------------------------
vMovimenti :: Componente
vMovimenti (r1 ,Saldo r2 v ) est = do
	Right $ est{conti_responsabili = modifica (+v) r1 0 . modifica (subtract v) r2 0 $ conti_responsabili est}
vMovimenti (r, Accredito m v ) est = do
	Right $ est{
		conti_responsabili = modifica (+v) r 0 (conti_responsabili est),
		conti_membri = modifica (+v) m 0 (conti_membri est)
		}
vMovimenti _ est = Right est

controlloOrdineNuovo o est = case o `S.member` tuttibeni est of
	True -> Left $ "il bene " ++ show o ++ " e' gia' stato consegnato o e' in stato aperto"
	False -> Right est
controllaCopertura m o v est = case  M.findWithDefault 0 m (conti_membri est) >= v of 
	True -> Right est
	False -> Left $ "copertura economica non sufficiente per " ++ show m ++ " nella richiesta per il bene " ++ show o
controllaOrdineAperto o est = case M.lookup o (aperti est) of
	Nothing -> Left $ "il bene " ++ show o ++ " non si trova in stato aperto "
	Just _  -> Right est
vOrdine ::Componente
vOrdine (_ , Apertura o) est = do
	controlloOrdineNuovo o est 
	Right $ est {aperti = M.insert o [] $ aperti est}
vOrdine (_, Chiusura o) est = do
	controllaOrdineAperto o est 
	Right $ est {
		aperti = M.delete o $ aperti est,
		chiusi = S.insert (Successo o) $ chiusi est
		}
vOrdine (_, Fallimento o) est = do
	controllaOrdineAperto o est
	Right $ est {
		aperti = M.delete o $ aperti est,
		chiusi = S.insert (Fallito o) $ chiusi est,
		conti_membri = foldr (\r -> modifica (subtract (valore r)) (membro r) 0) (conti_membri est) (aperti est M.! o)
		}
vOrdine (_, r@(Richiesta m o v)) est = do
	controllaOrdineAperto o est
	controllaCopertura m o v est
	Right $ est {
		aperti = M.insertWith (++) o [r] $ aperti est,
		conti_membri = modifica (subtract v) m 0 $ conti_membri est
		}
vOrdine _ est = Right est
-----------------------------------------------------------------------------------
controlloPromuovendo re est = bool (Right est) (Left "responsabile non in promozione") $ re `M.member` promuovendi est 
controlloNonPromuovendo re est = bool (Left "responsabile in promozione") (Right est) $ re `M.member` promuovendi est 
controlloLicenziando re est = bool (Right est) (Left "responsabile non in licenziamento") $ re `M.member` licenziandi est
controlloNonLicenziando re est = bool (Left "responsabile in licenziamento") (Right est) $ re `M.member` licenziandi est
promozione t re r est =	do
	controlloPromuovendo re est
	let 	ps = promuovendi est
	Right $ if M.size (ps M.! re) >= M.size (responsabili est) `div` 2 
		then est{
			promuovendi = M.delete re ps,
			responsabili = M.insert re Nothing (responsabili est)
			}
		else est {promuovendi = M.insertWith M.union re (M.singleton r t) ps}

licenziamento t re r est = do
	controlloLicenziando re est
	let 	ls = licenziandi est
	Right $ if M.size (ls M.! re) >= M.size (responsabili est) `div` 2 
		then est {
			licenziandi = M.delete re ls,
			responsabili = M.delete re $ responsabili est
			}
		else est {licenziandi = M.insertWith M.union re (M.singleton r t) ls}


vAmministrativo (r,e@(Promozione re)) est = do
	controlloNonEsistenzaResponsabile re est
	controlloNonPromuovendo re est
	Right $ est {promuovendi = M.insert re (M.singleton r True) $ promuovendi est}

vAmministrativo (r,e@(Licenziamento re)) est = do
	controlloEsistenzaResponsabile re est
	controlloNonLicenziando re est
	Right $ est {licenziandi = M.insert re (M.singleton r True) $ licenziandi est}
		

vAmministrativo (r,e@(Assenso re)) est = liftM2 mplus (promozione True re r) (licenziamento True re r) est
vAmministrativo (r,e@(Dissenso re)) est = liftM2 mplus (promozione False re r) (licenziamento False re r) est
vAmministrativo _ est = Right est

vAttesa (r,_) est = if all (elem r) dichiarazioni
	then Right est
	else Left "mancano dichiarazioni amministrative"
	where dichiarazioni = concatMap (map M.keys . M.elems) [promuovendi est, licenziandi est]
	
componenti :: [Componente]
componenti = [vAttesa, vAmministrativo, vIdentita, vOrdine, vMovimenti]

validaEvento :: Validatore
validaEvento e s = case injectM s (map ($e) componenti) of 	
			Left t -> tell [Left (e,t)] >> return s
			Right s' -> tell [Right e] >> return s'

