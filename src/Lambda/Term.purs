
module Lambda.Term(
                   Term(..),
                   substitute
                  ) where

import Lambda.PrettyShow(class PrettyShow, parenthesizeIf)

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Term = Var String
          | App Term Term
          | TypeApp Term Term
          | Subscript Term Term
          | OperatorSectionLeft String Term
          | OperatorSectionRight Term String

derive instance Eq Term
derive instance Generic Term _

instance showTerm :: Show Term where
    show x = genericShow x

instance PrettyShow Term where
    prettyShow = prettyShowPrec defaultPrecedence

substitute :: String -> Term -> Term -> Term
substitute x t (Var x') | x == x' = t
                        | otherwise = Var x'
substitute x t (App a b) = App (substitute x t a) (substitute x t b)
substitute x t (TypeApp a b) = TypeApp (substitute x t a) (substitute x t b)
substitute x t (Subscript a b) = Subscript (substitute x t a) (substitute x t b)
substitute x t (OperatorSectionLeft o a) = OperatorSectionLeft o (substitute x t a)
substitute x t (OperatorSectionRight a o) = OperatorSectionRight (substitute x t a) o

defaultPrecedence :: Int
defaultPrecedence = 0

appLeftPrecedence :: Int
appLeftPrecedence = 1

appRightPrecedence :: Int
appRightPrecedence = 2

typeAppPrecedence :: Int
typeAppPrecedence = 3 -- TODO Show TypeApp with Haskell's TypeApplication syntax

prettyShowPrec :: Int -> Term -> String
prettyShowPrec _ (Var x) = x
prettyShowPrec n (App left right) =
    let left' = prettyShowPrec appLeftPrecedence left
        right' = prettyShowPrec appRightPrecedence right in
    parenthesizeIf (n >= appRightPrecedence) $ left' <> " " <> right'
prettyShowPrec n (TypeApp left right) =
    let left' = prettyShowPrec typeAppPrecedence left
        right' = prettyShowPrec defaultPrecedence right in
    parenthesizeIf (n > typeAppPrecedence) $ left' <> "[" <> right' <> "]"
prettyShowPrec n (Subscript left right) =
    let left' = prettyShowPrec typeAppPrecedence left
        right' = prettyShowPrec defaultPrecedence right in
    -- TODO We use TypeApp precedence here, since the two operators
    -- are identical. We might change this later.
    parenthesizeIf (n > typeAppPrecedence) $ left' <> "[" <> right' <> "]"
prettyShowPrec _ (OperatorSectionLeft o a) =
    let a' = prettyShowPrec defaultPrecedence a in
    parenthesizeIf true $ o <> " " <> a'
prettyShowPrec _ (OperatorSectionRight a o) =
    let a' = prettyShowPrec defaultPrecedence a in
    parenthesizeIf true $ a' <> " " <> o
