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
module Test.Lambda.LookupMap(lookupMapSpecs) where

import Lambda.LookupMap (runLookupMap, fromMap)

import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec (describe, it, Spec)
import Data.Map (Map)
import Data.Map.Gen (genMap)
import Data.Map (lookup, singleton) as Map
import Data.Maybe (Maybe(..))
import Prelude

arbitraryMap :: forall k v. Ord k => Arbitrary k => Arbitrary v => Gen (Map k v)
arbitraryMap = genMap arbitrary arbitrary

lookupMapSpecs :: Spec Unit
lookupMapSpecs = do
  describe "LookupMap" do
    describe "fromMap" do
      it "looks up results in the given map" do
        quickCheck ado
          m :: Map String Int <- arbitraryMap
          k <- arbitrary
          in runLookupMap (fromMap m) k == Map.lookup k m
      it "looks up existing results in the given map" do
        quickCheck $ \(k :: String) (v :: Int) ->
            runLookupMap (fromMap $ Map.singleton k v) k == Just v
