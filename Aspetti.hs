{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, FunctionalDependencies , UndecidableInstances, OverlappingInstances, TypeOperators, NoMonomorphismRestriction #-}

-- modulo di programmazione dello stato pluggable
module Aspetti where

import Control.Applicative ((<$>)) 
import Control.Monad.State (gets ,modify)

class ParteDi l ls where
	see :: ls -> l
	set :: l -> ls -> ls

instance ParteDi l (l,ls) where 
	see (l,_) = l 
	set l (_,ls) = (l,ls) 
instance ParteDi l ls => ParteDi l (l',ls) where 
	see (_,ls) = see ls
	set l (l',ls) = (l',set l ls)

seeset :: ParteDi a ls => (a -> a) -> ls -> ls	
seeset f x =  set  (f $ see x) x

modifica = modify . seeset
osserva = gets see

infixr 8 .<
-- | by hand adding an annotation
(.<) :: l -> ls -> (l,ls)
(.<) = (,) 

infixr 8 :*:
type a :*: b = (a,b)

