
module Lambda.Term(
                   Term(..),
                   substitute, freeVariables, allVariables
                  ) where

import Lambda.PrettyShow (class PrettyShow, parenthesizeIf)

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Set (Set)
import Data.Set (singleton, delete, insert) as Set

data Term = Var String
          | App Term Term
          | TypeApp Term Term
          | Subscript Term Term
          | OperatorSectionLeft String Term
          | OperatorSectionRight Term String
          | Fn String Term -- Lambda abstraction

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
substitute x t (Fn x' body)
    | x == x' = Fn x' t
    | otherwise = Fn x' (substitute x t body)

freeVariables :: Term -> Set String
freeVariables (Var x) = Set.singleton x
freeVariables (App a b) = freeVariables a <> freeVariables b
freeVariables (TypeApp a b) = freeVariables a <> freeVariables b
freeVariables (Subscript a b) = freeVariables a <> freeVariables b
freeVariables (OperatorSectionLeft _ a) = freeVariables a
freeVariables (OperatorSectionRight a _) = freeVariables a
freeVariables (Fn x a) = Set.delete x (freeVariables a)

allVariables :: Term -> Set String
allVariables (Var x) = Set.singleton x
allVariables (App a b) = allVariables a <> allVariables b
allVariables (TypeApp a b) = allVariables a <> allVariables b
allVariables (Subscript a b) = allVariables a <> allVariables b
allVariables (OperatorSectionLeft _ a) = allVariables a
allVariables (OperatorSectionRight a _) = allVariables a
allVariables (Fn x a) = Set.insert x $ allVariables a

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
prettyShowPrec n (Fn var body) =
    let body' = prettyShowPrec defaultPrecedence body in
    parenthesizeIf (n > defaultPrecedence) $ "λ" <> var <> ". " <> body'
