module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import View.App (appComponent)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI appComponent unit body
