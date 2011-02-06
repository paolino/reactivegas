module Core.Contesto (esterno, Contesto (..), nuovoContesto, Contestualizzato, motiva, flatten) where

import Core.Types
import Debug.Trace

tit x = (trace (show x)) x
-- | Il contesto tiene conto degli eventi ai quali abbiamo reagito prima di trovarci a reagire all'ultimo. La catena ha sempre in testa un evento esterno, seguita da un numero qualsiasi di eventi interni
data Contesto d = Boot | Primo (Esterno d) | Oltre (Esterno d) [Interno] deriving (Show,Eq)

esterno = f where
	f Boot = Nothing
	f (Primo e) = Just e
	f (Oltre e _) = Just e
nuovoContesto = Boot
-- | il passaggio da un contesto all'altro deve seguire il protocollo : esterno, interno, interno, interno. ......
motiva :: (Show d) => Either Interno (Esterno d) -> Contesto d -> Contesto d
motiva (Right x) Boot =  Primo x
motiva (Left x) (Primo y) =  Oltre y [x]
motiva (Left x) (Oltre y xs) =  Oltre y (xs ++ [x])
motiva r q = error ("ricontestualizzazione fallita " ++ show (r,q))

-- | alcuni valori hanno senso solo insieme al loro contesto
type Contestualizzato d r = (Contesto d ,r)

-- | schiaccia  un classico contesto
flatten :: Contesto String -> [(String,String)]
flatten Boot = []
flatten (Primo y) = [y]
flatten (Oltre y xs) = y: map ((,) "Interno") xs

