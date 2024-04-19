
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
