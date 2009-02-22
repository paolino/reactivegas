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
image :: Evento -> B.ByteString
image e = B.pack $ concat $ showU e

controlloResponsabile r est = case M.lookup r (responsabili est) of
		Nothing -> Left $ "responsabile "++ show r ++ " non conosciuto"
		Just pk -> Right pk

nuovoResponsabile r pk est = est {responsabili = M.insert r pk (responsabili est)}
controlloFirma ev est = controlloResponsabile (responsabile ev) est >>= \pk -> case verify pk (image ev) ((\(Firma f) -> f) . firma $ev) of
			True -> Right est
			False -> Left $ "firma non verificata del responsabile " ++ show (responsabile ev)

vResponsabile :: Componente
vResponsabile (Bootstrap r pk) est = case M.null (responsabili est) of
	True -> Right (nuovoResponsabile r pk est)
	False -> Left "Un solo evento di Boostrap ammesso un una conoscenza"
vResponsabile ev@(Responsabile _ _ r pk _) est = controlloFirma ev est >>= Right . nuovoResponsabile r pk 
vResponsabile ev est = controlloFirma ev est
----------------------------------------------------------------------------------------------
nuovoMembro m est = est {membri = S.insert m (membri est)}
controlloMembro m est = case S.member m (membri est) of
	False -> Left "membro sconosciuto"
	True -> Right est

vMembro :: Componente
vMembro (Membro _ _ m _) = Right . nuovoMembro m 
vMembro (Accredito _ _ m _ _) = controlloMembro m 
vMembro (Richiesta _ _ m _ _ _) = controlloMembro m
vMembro _ = Right
-----------------------------------------------------------------------------------------------
vTempoCorretto :: Tempo -> Componente
vTempoCorretto t (Bootstrap _ _) est = Right est
vTempoCorretto t ev est = case tempo ev == t of
	True -> Right est
	False -> Left "evento prodotto fuori dalla giornata"
------------------------------------------------------------------------------
vSaldo :: Componente
vSaldo (Saldo _ r1 r2 v _) est = controlloResponsabile r2 est >> Right 
	(est{conti_responsabili = modifica (+v) r1 0 . modifica (subtract v) r2 0 $ conti_responsabili est})
vSaldo _ est = Right est
------------------------------------------------------------------------------


