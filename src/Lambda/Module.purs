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

-- The functions exported directly from the Javascript module we
-- bundle.

module Lambda.Module(
                     parseAndDescribe, parseAndDescribeLatex,
                     parseAndDescribeLatexEff
                    ) where

import Lambda.Type.Builtins (allBuiltins, reservedNames)
import Lambda.Predicate (Predicate)
import Lambda.Predicate.Simplify (simplify)
import Lambda.Util (toList, repeatedly)
import Lambda.Type.Parser (parseExpression)
import Lambda.Type.Free (describeFreeTheorem, describeFreeTheoremLatex)
import Lambda.Type (makeClosed)
import Lambda.MathShow (Latex)

import Prelude
import Data.Either (Either)
import Data.Bifunctor (lmap)
import Data.List (List)
import Effect (Effect)
import Effect.Exception (Error)
import Control.Monad.Error.Class (liftEither)

parseAndDescribe :: String -> Either String String
parseAndDescribe input = do
  ttype <- lmap show $ parseExpression input
  let ttype' = makeClosed ttype
  lmap show $ describeFreeTheorem simplify' allBuiltins reservedNames' ttype'

parseAndDescribeLatex :: String -> Either String Latex
parseAndDescribeLatex input = do
  ttype <- lmap show $ parseExpression input
  let ttype' = makeClosed ttype
  lmap show $ describeFreeTheoremLatex simplify' allBuiltins reservedNames' ttype'

parseAndDescribeLatexEff :: String -> Effect Latex
parseAndDescribeLatexEff = liftEither <<< lmap toError <<< parseAndDescribeLatex

foreign import toError :: String -> Error

reservedNames' :: List String
reservedNames' = toList reservedNames

-- Run the simplification pipeline a couple of times, so we can
-- simplify as much as is reasonable.
simplify' :: Predicate -> Predicate
simplify' = repeatedly 6 simplify
