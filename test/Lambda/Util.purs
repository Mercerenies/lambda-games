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
module Test.Lambda.Util(utilSpecs) where

import Lambda.Util (toList, toUnfoldable, fromChars)

import Test.Spec.QuickCheck (quickCheck)
import Test.Spec (describe, it, Spec)
import Data.List (List)
import Data.List (toUnfoldable) as List
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Prelude

utilSpecs :: Spec Unit
utilSpecs = do
  describe "toList" do
    it "is the identity on lists" do
      quickCheck $ \(a :: List Int) -> toList a == a
    it "reverses List.toUnfoldable" do
      quickCheck $ \(a :: List Int) -> toList (List.toUnfoldable a :: Array Int) == a
      quickCheck $ \(a :: Array Int) -> List.toUnfoldable (toList a) == a
  describe "toUnfoldable" do
    it "behaves like List.toUnfoldable on lists" do
      quickCheck $ \(a :: List Int) -> (toUnfoldable a :: Array Int) == List.toUnfoldable a
  describe "fromChars" do
    it "behaves like fromCharArray" do
      quickCheck $ \(a :: Array Char) -> fromChars a == fromCharArray a
    it "reverses toCharArray" do
      quickCheck $ \(a :: String) -> fromChars (toCharArray a) == a
      quickCheck $ \(a :: Array Char) -> toCharArray (fromChars a) == a
