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
