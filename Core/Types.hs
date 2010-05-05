module Core.Types where
import Lib.Firmabile (Chiave,Segreto)
-- | come ci si riferisce ad un evento
type Evento = String


-- | gli eventi prodotti dai reattori sono maneggiati come stringhe
type Interno = Evento

-- | gli eventi provenienti dall'esterno sono stringhe accoppiate con un valore di tipo libero per l'applicazione
type Esterno d = (d,Evento)
-- | I log dei reattori sono diretti all'utente 
type Message = String
type Utente = String
-- | un utente con chiave pubblica
type Responsabile = (Utente,(Chiave,Segreto))

