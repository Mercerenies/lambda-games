
module Test.Lambda.Type(lambdaTypeSpecs) where

import Lambda.Type (isClosed, makeClosed, freeVariables, substitute)

import Test.Spec.QuickCheck (quickCheck)
import Test.Spec (describe, it, Spec)
import Data.List (elem)
import Prelude

lambdaTypeSpecs :: Spec Unit
lambdaTypeSpecs = do
  describe "TType" do
    describe "makeClosed" do
      it "makes a type expression closed" do
        quickCheck $ \a -> isClosed (makeClosed a)
    describe "substitute" do
      it "has no effect when applied to a non-free variable" do
        quickCheck $ \ttype var substType -> var `elem` freeVariables ttype ||
                                             ttype == substitute var substType ttype
