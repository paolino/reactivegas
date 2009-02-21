{-# LANGUAGE StandaloneDeriving #-}
module Crypto where

import Codec.Crypto.RSA
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import Data.List
import Text.ParserCombinators.ReadP as P
import System.Random
import System.Console.GetOpt
import System.Environment
import Data.Maybe 
import Data.Char
import Text.CSV
import Control.Applicative

import Eventi
import Fields


image :: Evento -> B.ByteString
image e = B.pack $ concat $ showU e

nuovoResponsabile r pk est = est {responsabili = M.insert r pk (responsabili est)}
controlloFirma ev est = case M.lookup (responsabile ev) (responsabili est) of
		Nothing -> Left $ "responsabile "++ show (responsabile ev) ++ " non conosciuto"
		Just pk -> case verify pk (image ev) ((\(Firma f) -> f) . firma $ev) of
			True -> Right est
			False -> Left $ "firma non verificata del responsabile " ++ show (responsabile ev)

-- | controlla che la firma di un Evento sia valida e quindi che il responsabile esista

verifica :: Componente
verifica (Bootstrap r pk) est = Right (nuovoResponsabile r pk est)
verifica ev@(Responsabile _ _ r pk _) est = controlloFirma ev est >>= Right . nuovoResponsabile r pk 
verifica ev est = controlloFirma ev est


firmatore :: PrivateKey -> Tempo -> Evento ->  (Either String Evento)
firmatore _ _ (Bootstrap _ _) = Left "evento di bootstrap infirmabile"
firmatore pk t ev  = Right $ let ev' = ev{tempo = t} in ev'{firma = Firma (sign pk (image ev'))}
------------------------------------------------
------------------------------------------------
--
keys :: StdGen -> [(PublicKey,PrivateKey)]
keys g = let (pu,pr,g') = generateKeyPair g 512 in (pu,pr):keys g'

paolino:sara:roby:teo:pippo:_ = map User ["paolino","sara","roby","teo","pippo"]
ora = read "22 Febbraio 2009"
main = do
	k1:k2:k3:k4:k5:_ <- keys <$> newStdGen
	let 	Right e1 = firmatore (snd k1) ora (Responsabile undefined paolino sara (fst k2) undefined)
		Right e2 = firmatore (snd k2) ora (Responsabile undefined sara roby (fst k3) undefined)
		Right e3 = firmatore (snd k1) ora (Responsabile undefined sara teo (fst k4) undefined)
		Right e4 = firmatore (snd k4) ora (Responsabile undefined teo pippo (fst k5) undefined)
		kon = [Bootstrap paolino (fst k1), e1 ,e2 , e3 ,e4]
		(est,resp) = valida (validaEvento [verifica]) kon
	print est
	mapM_ print resp

