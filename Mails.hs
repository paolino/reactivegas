module Mails where

import Network.SMTP.Client
import Network.Socket
import Integrity (mkRPatch, mkPatch, Patch, RPatch , Conoscenza(..), validazioneRs, validazioneS)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Rfc2822 (Field(..),NameAddr)
import Control.Applicative
import Fields

domain = "gmail.com"
sincronizzatore = NameAddr (Just "paolino") "paolo.veronelli@gmail.com"
responsabile = sincronizzatore -- me la canto e me la suono

relay = SockAddrInet (fromIntegral 25) 4259442129 -- :P lo storico spammer

sendRPatch :: Show a => Int -> RPatch a -> IO ()
sendRPatch n p = sendSMTP Nothing domain relay [m] where
	m = Message [From [responsabile],To [sincronizzatore],
		Subject $ "patch responsabile (" ++ show n ++ ")"] $ show p

sendPatch :: Show a => Int -> Patch a -> IO ()
sendPatch n p = sendSMTP Nothing domain relay [m] where
	m = Message [From [sincronizzatore],To [responsabile],
		Subject $ "patch sincronizzatore (" ++ show n ++ ")"] $ show p

printLeft w x = case x of 
	Left s -> putStrLn s 
	Right _ -> putStrLn w

fromRight (Right x) = x	
fromRight _ = error "you meant that"
test = do
	pu <- read <$> readFile "paolino.publ"
	pr <- read <$> readFile "paolino.priv"
	let base = Base pu -- sono il sincronizzatore
	(sendPatch 1 :: Patch (Int,String) -> IO ()) 
		. fromRight $ mkPatch [] [pu] pr base -- mi eleggo responsabile
	p <- read <$> (putStr "Aspetto la patch del sincronizzatore:" >> getLine)
	let conoscenza1 = Conoscenza p base  -- conoscenza  patchata
	printLeft  "validazione ok" $ validazioneS conoscenza1
	let eventi = [(1,"ciao"),(2,"bye")] -- due eventi qualsiasi (anche in forma)
	sendRPatch 2 . fromRight $ mkRPatch eventi pu pr $ conoscenza1
	rp <- read <$> (putStr "Aspetto la patch del responsabile:" >> getLine)
	printLeft "validazione ok" $ validazioneRs [rp] conoscenza1 

