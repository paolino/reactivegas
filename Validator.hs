import Control.Monad.State
import Eventi


validate :: Evento -> State Extract Bool
validate m@(Membro _ _) = modify (step m) >> return True
validate r@(Responsabile _ _ _) = modify (step r) >> return True
validate s@(Saldo _ _ _) = modify (step s) >> return True
validate a@(Accredito _ m _) = do
	ms <- membri <$> get
	if m `S.member` ms then do	
		modify $ step a 
		return True
	 else return False
validate a@(Apertura _ o) = do
	bs <- tuttibeni . beni <$> get
	if o `S.member` bs then return False
		else modify (step a) >> return True
validate c@(Chiusura _ o) = do
	as <- aperti . beni <$> get
	if not (o `S.member` as) then return False
		else modify (step c) >> return True

validate f@(Fallimento _ o) = do
	as <- aperti . beni <$> get
	if not (o `S.member` as) then return False
		else modify (step f) >> return True
validate r@(Richiesta _ m o v) = do
	Extract bs@(Beni as _) cs@(Conti cms _) ms rs <- get 
	if and [m `S.member` ms, o `S.member` as, M.findWithDefault 0 m cms >= v] then do
		modify (step r) >> return True
		else return False
	
----------------------------- summarizing knowledge from a list of valid events ------------------------------------------------



{-
mkExtract :: Conoscenza -> Extract
mkExtract = foldr step emptyExtract

modifica :: (Valore -> Valore) -> User -> Map User Valore -> Map User Valore
modifica dv m = M.insertWith (const dv) m (dv 0)

step :: Evento -> Extract -> Extract
step (Membro _ m) 		k@(Extract _ _ ms _) = k{membri = m `S.insert` ms} 
step (Responsabile _ m _) 	k@(Extract _ _ _ rs) = k{responsabili = m `S.insert` rs} 
-- accredita sul conto  del responsabile e sul conto del membro
step (Accredito r m v) 		k@(Extract _ (Conti cms crs) _ _) =  k{conti = Conti (modifica (+v) m cms) (modifica (+v) r crs)}
-- accredita sul conto del primo responsabile e scredita sul secondo
step (Saldo r1 r2 v)  		k@(Extract _ (Conti cms crs) _ _) =  k{conti = Conti cms (modifica (flip (-) v) r2  . modifica (+v) r1 $ crs)}
step (Fallimento _ o) 		k@(Extract (Beni vs as) _ _ _ ) = k{beni = Beni vs (S.insert (Fallito o) as)}
step (Chiusura r o)  		k@(Extract (Beni vs as) _ _ _ ) = k{beni = Beni vs (S.insert (Successo o) as)}
step (Apertura r o)  		k@(Extract (Beni vs as) _ _ _ ) = 
	if Fallito o `S.member` as || Successo o `S.member` as then k else k{beni = Beni (S.insert o vs) as}
step (Richiesta r m o v) 	k@(Extract b@(Beni vs as) (Conti cms crs)  _ _ ) 
	| Fallito o `S.member` as = k
	| Successo o `S.member` as = k{conti = Conti (modifica (flip (-) v) m cms) (modifica (flip (-) v) r crs)}
	| otherwise = k{conti = Conti (modifica (flip (-) v) m cms) crs} -- denaro impegnato ma non ancora speso
-}
