{- |
Module      : Core.Types
Description : Core type definitions for the reactive system
Copyright   : (c) Paolo Veronelli, 2025
License     : BSD-3-Clause

Fundamental type aliases used throughout the application for
events, users, and session management.
-}
module Core.Types
    ( Evento
    , Interno
    , Indice
    , Esterno
    , Message
    , Utente
    , Responsabile
    , SessioneAcquisto (..)
    , SessioneOrdinante (..)
    ) where

import Lib.Firmabile (Chiave, Segreto)
import Lib.QInteger (QInteger)

-- | Event identifier (how we refer to an event)
type Evento = String

-- | Internal events produced by reactors (handled as strings)
type Interno = Evento

-- | Index type for referencing items
type Indice = QInteger

-- | External events are strings paired with application-specific data
type Esterno d = (d, Evento)

-- | Log messages from reactors directed to users
type Message = String

-- | User identifier
type Utente = String

-- | A user with public/private key pair for signing
type Responsabile = (Utente, (Chiave, Segreto))

-- | Purchase session state
newtype SessioneAcquisto = SessioneAcquisto (Maybe Indice)

-- | Ordering session state
newtype SessioneOrdinante = SessioneOrdinante (Maybe Utente)
