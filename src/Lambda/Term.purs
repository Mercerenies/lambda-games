
module Lambda.Term(
                   Term(..),
                   prettyShow
                  ) where

import Lambda.Util(parenthesizeIf)

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Term = Var String
          | App Term Term

derive instance Eq Term
derive instance Generic Term _

instance showTerm :: Show Term where
    show x = genericShow x

prettyShow :: Term -> String
prettyShow = prettyShowPrec defaultPrecedence

defaultPrecedence :: Int
defaultPrecedence = 0

appLeftPrecedence :: Int
appLeftPrecedence = 1

appRightPrecedence :: Int
appRightPrecedence = 2

prettyShowPrec :: Int -> Term -> String
prettyShowPrec _ (Var x) = x
prettyShowPrec n (App left right) =
    let left' = prettyShowPrec appLeftPrecedence left
        right' = prettyShowPrec appRightPrecedence right in
    parenthesizeIf (n >= appRightPrecedence) $ left' <> " " <> right'
