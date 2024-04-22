-- Copyright 2024 Silvio Mayolo
--
-- Lambdagames is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Lambdagames. If not, see
-- <https://www.gnu.org/licenses/>.
module Test.Main where

import Test.Lambda.Util.InfiniteList (infiniteListSpecs)
import Test.Lambda.LookupMap (lookupMapSpecs)
import Test.Lambda.Type (lambdaTypeSpecs)
import Test.Lambda.Util (utilSpecs)

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
  lookupMapSpecs
  lambdaTypeSpecs
  utilSpecs
