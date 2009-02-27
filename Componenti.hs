{-# LANGUAGE StandaloneDeriving #-}
module Componenti where

import Control.Arrow
import Codec.Crypto.RSA
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Either
import Control.Applicative

import Eventi
import Fields
import Lib (modifica)

--------------------------------------------------------------------------------------------------

nuovoResponsabile r pk est = est {responsabili = M.insert pk r (responsabili est)}

vResponsabile :: Componente
vResponsabile (pk, Responsabile r) = Right . nuovoResponsabile r pk 
vResponsabile _ = Right 
----------------------------------------------------------------------------------------------
nuovoMembro m est = est {membri = S.insert m (membri est)}
controlloMembro m est = case S.member m (membri est) of
	False -> Left $ "il membro " ++ show m ++ " e' sconosciuto"
	True -> Right est

vMembro :: Componente
vMembro (_,Membro m ) = Right . nuovoMembro m 
vMembro (_,Accredito m _ ) = controlloMembro m 
vMembro (_,Richiesta m _ _) = controlloMembro m
vMembro _ = Right
-----------------------------------------------------------------------------------------------
vMovimenti :: Componente
vMovimenti (r1 ,Saldo r2 v ) est = do
	-- controlloResponsabile r2 est 
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

