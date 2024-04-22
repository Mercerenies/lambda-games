
-- The functions exported directly from the Javascript module we
-- bundle.

module Lambda.Module(
                     parseAndDescribe
                    ) where

import Prelude

import Lambda.Type.Builtins (allBuiltins, reservedNames)
import Lambda.Predicate (Predicate)
import Lambda.Predicate.Simplify (simplify)
import Lambda.Util (toList, repeatedly)
import Lambda.Type.Parser (parseExpression)
import Lambda.Type.Free (describeFreeTheoremWith)
import Lambda.Type (makeClosed)
import Data.Either (Either)
import Data.Bifunctor (lmap)
import Data.List (List)

parseAndDescribe :: String -> Either String String
parseAndDescribe input = do
  ttype <- lmap show $ parseExpression input
  let ttype' = makeClosed ttype
  lmap show $ describeFreeTheoremWith simplify' allBuiltins reservedNames' ttype'

reservedNames' :: List String
reservedNames' = toList reservedNames

-- Run the simplification pipeline a couple of times, so we can
-- simplify as much as is reasonable.
simplify' :: Predicate -> Predicate
simplify' = repeatedly 6 simplify
