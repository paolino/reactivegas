{-# LANGUAGE GHC2021, OverloadedStrings #-}
module TypeNegative where
import KelGroups.Event qualified as E
import KelGroups.Fold qualified as F
import S28DemoApp
historicalEventMismatch = F.applyEvent (\st _ -> st) foundingDemo ("admin-key-1", E.App (DemoAdd 1))
