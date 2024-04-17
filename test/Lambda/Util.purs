
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
