{-# LANGUAGE GHC2021, OverloadedStrings #-}
module Main (main) where
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import KelGroups.Event (BaseMutation(..))
import KelGroups.Fold (enactMutation)
import KelGroups.State (emptyState, members)
import S28AppApiSpec qualified
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.Hspec (hspec)
main :: IO ()
main = do
  args <- getArgs
  if args == ["witness"] then do
    let gs = emptyState ()
        post = enactMutation gs (ChangeRolesVoted "absent-witness" Set.empty)
        ok = Map.keysSet (members post) `Set.isSubsetOf` Map.keysSet (members gs)
    putStrLn $ "EFFECT-WITNESS absent-target-changeRoles pre=" ++ show (Map.size (members gs)) ++ " post=" ++ show (Map.size (members post)) ++ " noInsertion=" ++ show ok
    if ok then pure () else exitFailure
  else hspec S28AppApiSpec.spec
