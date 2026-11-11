{-# LANGUAGE GHC2021, OverloadedStrings #-}
module TypeNegative where

import KelGroups.Event qualified as E
import KelGroups.Fold qualified as F
import S28DemoApp

-- Keeps the proposal parameter correct; only the application-event type is wrong.
eventOnlyMismatch :: Either (F.IntegratedError DemoError) (F.IntegratedResult DemoState)
eventOnlyMismatch = F.applyIntegratedEvent demoIntegration foundingDemo "admin-key-1"
  (E.IEApp (DemoState 0 []))

-- The historical production boundary cannot consume DemoEvent with DemoState.
historicalEventMismatch = F.applyEvent (\st _ -> st) foundingDemo
  ("admin-key-1", E.App (DemoAdd 1))
