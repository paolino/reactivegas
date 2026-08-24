{- |
Module      : Reactivegas.Core
Description : Pure domain core for reactivegas events
Copyright   : (c) 2026 Paolo Veronelli
License     : BSD3
Maintainer  : Paolo Veronelli <paolo.veronelli@gmail.com>
Stability   : experimental

Pure, IO-free kernel of the modernized reactivegas architecture:
canonical event envelopes, Ed25519 / BLAKE3 primitives and state
reduction. This package must never depend on server IO.
-}
module Reactivegas.Core
    ( coreVersion
    ) where

-- | Version of the core event-format contract.
coreVersion :: String
coreVersion = "0.1.0.0"
