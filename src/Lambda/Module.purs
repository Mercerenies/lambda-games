
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
