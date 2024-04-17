
module Test.Main where

import Test.Lambda.Util.InfiniteList (infiniteListSpecs)

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (Spec)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Prelude

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] baseSpecs

baseSpecs :: Spec Unit
baseSpecs = do
  infiniteListSpecs
