module Core.Amministrazione where


import Codec.Crypto.RSA
import Codec.Crypto.SimpleAes
import Data.ByteString.Lazy.Char8 as B
import Core.Costruzione 

data UtenteForte = UtenteForte Utente B.ByteString B.ByteString

type Password = B.ByteString

mkUtenteForte :: Utente -> PublicKey -> PrivateKey -> String -> UtenteForte
mkUtenteForte u pu pr pw = UtenteForte u (encryptMsg CBC (B.pack pw) pr) (pack pu)

discloseUtenteForte :: UtenteForte -> Password -> (PublicKey,PrivateKey,Utente)

publicUtenteForte :: UtenteForte -> (Utente,PublicKey)



data Chiavi = Chiavi (StdGen -> (Utente,B.ByteString,B.ByteString)) 
richiestaChiavi s kp kn = (,) "crea nuove chiavi responsabile" $ do
	n <- libero "nome del responsabile"
	rs <- responsabili
	when (n `elem` rs) $ throwError "responsabile già presente"
	return (\g -> let (pu,pr,_) = generate
