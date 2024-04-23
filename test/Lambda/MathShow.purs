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
module Test.Lambda.MathShow(mathShowSpecs) where

import Lambda.MathShow (Latex(..), escapeLatexText, texttt, parenthesizeIf)

import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Prelude

mathShowSpecs :: Spec Unit
mathShowSpecs = do
  describe "escapeLatexText" do
    it "leaves alone most \"ordinary\" strings" do
      escapeLatexText "abc" `shouldEqual` Latex "abc"
      escapeLatexText "99 D 99" `shouldEqual` Latex "99 D 99"
      escapeLatexText "" `shouldEqual` Latex ""
      escapeLatexText "x   y" `shouldEqual` Latex "x   y"
    it "escapes backslashes" do
      escapeLatexText "a\\b" `shouldEqual` Latex "a\\textbackslashb"
    it "escapes braces" do
      escapeLatexText "{ a b }" `shouldEqual` Latex "\\{ a b \\}"
    it "escapes complex strings" do
      escapeLatexText "{ a \\ b }" `shouldEqual` Latex "\\{ a \\textbackslash b \\}"
  describe "texttt" do
    it "wraps the strings in a \\texttt block" do
      texttt "abc" `shouldEqual` Latex "\\texttt{abc}"
    it "escapes the inner string" do
      quickCheck $ \s -> texttt s == Latex "\\texttt{" <> escapeLatexText s <> Latex "}"
  describe "parenthesizeIf" do
    it "does nothing when given a false input" do
      quickCheck $ \s -> parenthesizeIf false (Latex s) == Latex s
    it "adds parentheses when given a true input" do
      quickCheck $ \s -> parenthesizeIf true (Latex s) == Latex "\\left(" <> Latex s <> Latex "\\right)"
