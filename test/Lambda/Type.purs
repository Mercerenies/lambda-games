
module Test.Lambda.Type(lambdaTypeSpecs) where

import Lambda.Type (isClosed, makeClosed)

import Test.Spec.QuickCheck (quickCheck)
import Test.Spec (describe, it, Spec)
import Prelude

lambdaTypeSpecs :: Spec Unit
lambdaTypeSpecs = do
  describe "TType" do
    describe "makeClosed" do
      it "makes a type expression closed" do
        quickCheck $ \a -> isClosed (makeClosed a)
