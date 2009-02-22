import System.Random
import System.Console.GetOpt
import System.Environment
import Data.Maybe 
import Control.Arrow
import Control.Monad.Error
import Control.Monad.Reader
import Data.Either

import Codec.Crypto.RSA

import Componenti 
import Eventi
import Fields
------------------------------------------------
------------------------------------------------
type Signing  = Reader (Tempo,Utente)
contestualmente :: (Tempo,Utente) -> Signing a -> a
contestualmente = flip runReader

firmo :: Evento ->  Signing Evento
firmo ev  =  do	(t,Utente u pu (Just pr)) <- ask 
		let 	ev' = ev{tempo = t} 
			ev'' = ev'{responsabile = u}
		return $ ev''{firma = Firma (sign pr (image ev''))}


---- un set di creatori di eventi sotto l'ambiente reale, un utente e un tempo attuale.
uResponsabile m pk 	= firmo (Responsabile undefined undefined m pk undefined)
uMembro m 		= firmo (Membro undefined undefined m undefined)
uAccredito m v 		= firmo (Accredito undefined undefined m v undefined)
uSaldo m v 		= firmo (Saldo undefined undefined m v undefined)
uApertura o t 		= firmo (Apertura undefined undefined o t undefined)
uChiusura o 		= firmo (Chiusura undefined undefined o undefined)
uFallimento o 		= firmo (Fallimento undefined undefined o undefined)
uRichiesta m o v 	= firmo (Richiesta undefined undefined m o v undefined)
uBootstrap m pk 	= return $ Bootstrap m pk
----------------------------------------------------------------------------
-------------------------------------------------------------------------------------


data Utente = Utente {user :: User, pu :: PublicKey, pr :: Maybe PrivateKey}

nomi = ["paolino","sara","roby","teo","pippo"]
paolino:sara:roby:teo:pippo:_ = map (\(u,(pu,pr)) -> Utente (User u) pu (Just pr)) us
	where
	us = zip nomi ks
	ks = keys $ mkStdGen 0 where keys g = let (pu,pr,g') = generateKeyPair g 512 in (pu,pr):keys g'

ora = read "22 Febbraio 2009"
componenti = [vTempoCorretto ora, vResponsabile, vMembro, vSaldo]
validazione = valida $ validaEvento componenti
prova  = let 	kon = [Bootstrap (user paolino) (pu paolino), e1 ,e2 , e3 ,e5]
		[e1,e5] = contestualmente (ora,paolino) $ sequence [uResponsabile (user sara) (pu sara), uMembro (user sara)]
		e2 = contestualmente (ora,sara) (uResponsabile (user roby) (pu roby))
		e3 = contestualmente (ora,roby) (uMembro (user roby))
		in (kon,(validazione kon))

