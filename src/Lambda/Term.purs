
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
import Data.Tuple (Tuple(..), fst, snd)

data Term = Var String
          | App Term Term
          | OperatorSectionLeft String Term
          | OperatorSectionRight Term String
          | OperatorApp Term String Term
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
substitute x t (OperatorSectionLeft o a) = OperatorSectionLeft o (substitute x t a)
substitute x t (OperatorSectionRight a o) = OperatorSectionRight (substitute x t a) o
substitute x t (OperatorApp lhs op rhs) = OperatorApp (substitute x t lhs) op (substitute x t rhs)
substitute x t (Fn x' body)
    | x == x' = Fn x' t
    | otherwise = Fn x' (substitute x t body)

freeVariables :: Term -> Set String
freeVariables (Var x) = Set.singleton x
freeVariables (App a b) = freeVariables a <> freeVariables b
freeVariables (OperatorSectionLeft _ a) = freeVariables a
freeVariables (OperatorSectionRight a _) = freeVariables a
freeVariables (OperatorApp a _ b) = freeVariables a <> freeVariables b
freeVariables (Fn x a) = Set.delete x (freeVariables a)

allVariables :: Term -> Set String
allVariables (Var x) = Set.singleton x
allVariables (App a b) = allVariables a <> allVariables b
allVariables (OperatorSectionLeft _ a) = allVariables a
allVariables (OperatorSectionRight a _) = allVariables a
allVariables (OperatorApp a _ b) = allVariables a <> allVariables b
allVariables (Fn x a) = Set.insert x $ allVariables a

defaultPrecedence :: Int
defaultPrecedence = -1

appLeftPrecedence :: Int
appLeftPrecedence = 100

appRightPrecedence :: Int
appRightPrecedence = 201

-- Operators known to our runtime. If Haskell defines the precedence
-- as x, we define the operator precedence to be 2 * x on the side we
-- associate and 2 * x + 1 on the side we don't, so that we can get
-- the parentheses correct.
operatorPrecedences :: String -> Tuple Int Int
operatorPrecedences "***" = Tuple 7 6 -- infixr 3 in Control.Arrow
operatorPrecedences "$" = Tuple 1 0 -- infixr 0 in Prelude
operatorPrecedences _ = Tuple 18 19 -- infixl 9 is the default fixity

operatorPrecedenceLeft :: String -> Int
operatorPrecedenceLeft = fst <<< operatorPrecedences

operatorPrecedenceRight :: String -> Int
operatorPrecedenceRight = snd <<< operatorPrecedences

operatorPrecedenceMax :: String -> Int
operatorPrecedenceMax s = max (operatorPrecedenceLeft s) (operatorPrecedenceRight s)

prettyShowPrec :: Int -> Term -> String
prettyShowPrec _ (Var x) = x
prettyShowPrec n (App left right) =
    let left' = prettyShowPrec appLeftPrecedence left
        right' = prettyShowPrec appRightPrecedence right in
    parenthesizeIf (n >= appRightPrecedence) $ left' <> " " <> right'
prettyShowPrec _ (OperatorSectionLeft o a) =
    let a' = prettyShowPrec (operatorPrecedenceRight o) a in
    parenthesizeIf true $ o <> " " <> a'
prettyShowPrec _ (OperatorSectionRight a o) =
    let a' = prettyShowPrec (operatorPrecedenceLeft o) a in
    parenthesizeIf true $ a' <> " " <> o
prettyShowPrec n (OperatorApp a o b) =
    let a' = prettyShowPrec (operatorPrecedenceLeft o) a
        b' = prettyShowPrec (operatorPrecedenceRight o) b in
    parenthesizeIf (n >= operatorPrecedenceMax o) $ a' <> " " <> o <> " " <> b'
prettyShowPrec n (Fn var body) =
    let body' = prettyShowPrec defaultPrecedence body in
    parenthesizeIf (n > defaultPrecedence) $ "\\" <> var <> " -> " <> body'
