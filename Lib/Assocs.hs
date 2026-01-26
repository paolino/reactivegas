-- | alcune funzioni comode per la gestione delle liste associative
module Lib.Assocs where

import Data.List (lookup)
import Data.Maybe (isNothing)

-----------------------------------------

-- | aggiorna l'elemento selezionato fornendo un valore di default se l'elemento manca
update ::
    (Eq a) =>
    -- | indice
    a ->
    -- | modificatore
    (b -> b) ->
    -- | valore di default
    b ->
    -- | lista iniziale
    [(a, b)] ->
    -- | lista modificata
    [(a, b)]
update k dv v kvs = case lookup k kvs of
    Nothing -> (k, dv v) : kvs
    Just v -> (k, dv v) : filter ((/=) k . fst) kvs

-- | imposta il valore dell'elemento selezionato
upset ::
    (Eq a) =>
    -- | indice
    a ->
    -- | valore di default
    b ->
    -- | lista iniziale
    [(a, b)] ->
    -- | lista modificata
    [(a, b)]
upset x y = update x (const y) undefined

-- | controlla la assenza di un elemento
assente ::
    (Eq a) =>
    -- | indice
    a ->
    -- | lista associativa
    [(a, b)] ->
    -- | risultato della ricerca
    Bool
assente x = isNothing . lookup x

-- | elimina l'elemento selezionato
elimina ::
    (Eq a) =>
    -- | indice
    a ->
    -- | lista iniziale
    [(a, b)] ->
    -- | lista modificata
    [(a, b)]
elimina k kvs = let (xs, ys) = break ((== k) . fst) kvs in xs ++ if null ys then ys else tail ys

-- | update monadico
updateM ::
    (Monad m, Eq a) =>
    -- | indice
    a ->
    -- | modificatore
    (b -> m b) ->
    -- | valore di default
    b ->
    -- | lista iniziale
    [(a, b)] ->
    -- | lista modificata
    m [(a, b)]
updateM k dv v kvs = case lookup k kvs of
    Nothing -> dv v >>= \v' -> return $ (k, v') : kvs
    Just v -> dv v >>= \v' -> return $ (k, v') : filter ((/=) k . fst) kvs

-- | ricerca con default
(?) ::
    (Eq a) =>
    -- | lista ricercata
    [(a, b)] ->
    -- | indice accoppiato al valore di default
    (a, b) ->
    -- | valore estratto
    b
xs ? (k, t) = maybe t id $ lookup k xs
