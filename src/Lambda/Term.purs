
module Lambda.Term(
                   Term(..)
                  ) where

import Lambda.PrettyShow(class PrettyShow, parenthesizeIf)

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Term = Var String
          | App Term Term
          | TypeApp Term Term
          | Equals Term Term

derive instance Eq Term
derive instance Generic Term _

instance showTerm :: Show Term where
    show x = genericShow x

instance PrettyShow Term where
    prettyShow = prettyShowPrec defaultPrecedence

defaultPrecedence :: Int
defaultPrecedence = 0

eqPrecedence :: Int
eqPrecedence = 1

appLeftPrecedence :: Int
appLeftPrecedence = 2

appRightPrecedence :: Int
appRightPrecedence = 3

typeAppPrecedence :: Int
typeAppPrecedence = 4

prettyShowPrec :: Int -> Term -> String
prettyShowPrec _ (Var x) = x
prettyShowPrec n (App left right) =
    let left' = prettyShowPrec appLeftPrecedence left
        right' = prettyShowPrec appRightPrecedence right in
    parenthesizeIf (n >= appRightPrecedence) $ left' <> " " <> right'
prettyShowPrec n (TypeApp left right) =
    let left' = prettyShowPrec typeAppPrecedence left
        right' = prettyShowPrec defaultPrecedence right in
    parenthesizeIf (n >= typeAppPrecedence) $ left' <> "[" <> right' <> "]"
prettyShowPrec n (Equals left right) =
    let left' = prettyShowPrec eqPrecedence left
        right' = prettyShowPrec eqPrecedence right in
    parenthesizeIf (n >= eqPrecedence) $ left' <> " = " <> right'
