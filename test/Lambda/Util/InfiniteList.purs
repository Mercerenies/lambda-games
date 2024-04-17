
module Test.Lambda.Util.InfiniteList(infiniteListSpecs) where

import Lambda.Util.InfiniteList (InfiniteList, head, tail, lazyCons, cons, take,
                                 repeat, cycle)

import Test.Spec.QuickCheck (quickCheck)
import Test.Spec (describe, it, Spec)
import Data.Lazy (defer)
import Data.List (List(..), (:))
import Data.Function (on)
import Control.Comonad(extend)
import Prelude

-- We can't test infinite lists for equality, so test the first 20
-- elements, as a heuristic.
prefixEquals :: forall a. Eq a => InfiniteList a -> InfiniteList a -> Boolean
prefixEquals = (==) `on` take prefixLength
    where prefixLength = 20

infix 4 prefixEquals as =:=

infiniteListSpecs :: Spec Unit
infiniteListSpecs = do
  describe "Infinite list" do
    describe "head" do
      it "cancels off with cons" do
        quickCheck $ \(a :: Int) xs -> head (cons a xs) == a
      it "cancels off with lazyCons" do
        quickCheck $ \(a :: Int) lxs -> head (lazyCons a lxs) == a
    describe "tail" do
      it "cancels off with cons" do
        quickCheck $ \(a :: Int) xs -> tail (cons a xs) =:= xs
      it "cancels off with lazyCons" do
        quickCheck $ \(a :: Int) xs -> tail (lazyCons a (defer \_ -> xs)) =:= xs
    describe "repeat" do
      it "repeats the same element forever" do
        quickCheck $ \(a :: Int) -> repeat a =:= cycle (a : Nil)
    describe "extend" do
      it "is the identity on `head`" do
        quickCheck $ \(xs :: InfiniteList Int) -> extend head xs =:= xs
