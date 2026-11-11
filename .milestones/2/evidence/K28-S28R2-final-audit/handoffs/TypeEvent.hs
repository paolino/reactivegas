{-# LANGUAGE GHC2021, OverloadedStrings #-}
module TypeNegative where
import KelGroups.Event qualified as E
import KelGroups.Fold qualified as F
import S28DemoApp
eventOnlyMismatch = F.applyIntegratedEvent demoIntegration foundingDemo "admin-key-1" (E.IEApp (DemoState 0 []))
