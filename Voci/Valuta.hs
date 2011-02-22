module Voci.Ordini

import Voci.Data
import Voci.Compare

class Match a b where
	(=!=) a b :: Bool

instance Match (Ordine a b c d) (Voce a b c d) where
	(InPezzi x y) =!= z = y == z
