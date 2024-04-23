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

import Lambda.LookupMap (LookupMap, runLookupMap, lookup, fromMap, singleton)

import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec (describe, it, Spec)
import Data.Map (Map)
import Data.Map.Gen (genMap)
import Data.Map (lookup, singleton) as Map
import Data.Tuple (Tuple)
import Data.Maybe (Maybe(..))
import Control.MonadPlus (alt, empty)
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
    describe "Functor LookupMap" do
      it "maps over the results of the lookup" do
        quickCheck $ \(k :: String) (v :: Int) (f :: Int -> Tuple Int Int) ->
            let x = singleton k v in
            runLookupMap (map f x) k == Just (f v)
    describe "Apply LookupMap" do
      it "maps LookupMap functions over LookupMap results" do
        quickCheck $ \(k :: String) (v :: Int) (f :: Int -> Tuple Int Int) ->
            let ff = singleton k f
                xx = singleton k v in
            runLookupMap (ff <*> xx) k == Just (f v)
      it "cancels out inputs that don't appear on both sides" do
        quickCheck $ \(k :: String) (v :: Int) (f :: Int -> Tuple Int Int) ->
            let ff = singleton k f
                xx = singleton k v in
            runLookupMap (ff <*> xx) k == Just (f v)
    describe "Applicative LookupMap (pure)" do
      it "maps all inputs to the constant output" do
        quickCheck $ \s v ->
            let xx = pure v :: LookupMap String Int in
            runLookupMap xx s == Just v
    describe "Alt LookupMap" do
      it "takes a value from the left-hand side if it exists" do
        quickCheck $ \(k :: String) (v :: Int) (x :: LookupMap String Int) ->
            runLookupMap (singleton k v `alt` x) k == Just v
    describe "Alternative LookupMap (pure)" do
      it "never produces any output" do
        quickCheck $ \s ->
            let xx = empty :: LookupMap String Int in
            runLookupMap xx s == Nothing
    describe "lookup" do
      it "works like runLookupMap" do
        quickCheck $ \(s :: String) (x :: LookupMap String Int) ->
            lookup s x == runLookupMap x s
