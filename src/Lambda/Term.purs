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
module Lambda.Term(
                   Term(..), Pattern(..),
                   varsInPattern,
                   substitute, freeVariables, allVariables, isLambda
                  ) where

import Lambda.PrettyShow (class PrettyShow, prettyShow, parenthesizeIf)

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.List (List)
import Data.Set (Set)
import Data.Set (singleton, delete, insert, unions, member, difference, empty) as Set
import Data.Foldable (intercalate)

data Term = Var String
          | App Term Term
          | OperatorFunction String
          | OperatorSectionLeft String Term
          | OperatorSectionRight Term String
          | OperatorApp Term String Term
          | Fn String Term -- Lambda abstraction (TODO Consolidate with PatternFn)
          | PatternFn Pattern Term -- Lambda abstraction (with pattern matching)
          | TupleTerm (List Term) -- Must not contain exactly one term (we do not currently enforce this precondition)

data Pattern = VarPattern String
             | TuplePattern (List Pattern) -- Must not contain exactly one term (we do not currently enforce this precondition)

derive instance Eq Term
derive instance Generic Term _

derive instance Eq Pattern
derive instance Generic Pattern _

instance showTerm :: Show Term where
    show x = genericShow x

instance showPattern :: Show Pattern where
    show x = genericShow x

instance PrettyShow Term where
    prettyShow = prettyShowPrec defaultPrecedence

instance PrettyShow Pattern where
    prettyShow (VarPattern v) = v
    prettyShow (TuplePattern xs) = "(" <> intercalate ", " (map prettyShow xs) <> ")"

varsInPattern :: Pattern -> Set String
varsInPattern (VarPattern v) = Set.singleton v
varsInPattern (TuplePattern xs) = Set.unions $ map varsInPattern xs

substitute :: String -> Term -> Term -> Term
substitute x t (Var x') | x == x' = t
                        | otherwise = Var x'
substitute x t (App a b) = App (substitute x t a) (substitute x t b)
substitute _ _ (OperatorFunction o) = OperatorFunction o
substitute x t (OperatorSectionLeft o a) = OperatorSectionLeft o (substitute x t a)
substitute x t (OperatorSectionRight a o) = OperatorSectionRight (substitute x t a) o
substitute x t (OperatorApp lhs op rhs) = OperatorApp (substitute x t lhs) op (substitute x t rhs)
substitute x t (Fn x' body)
    | x == x' = Fn x' t
    | otherwise = Fn x' (substitute x t body)
substitute x t (PatternFn p body)
    | x `Set.member` varsInPattern p = PatternFn p body
    | otherwise = PatternFn p (substitute x t body)
substitute x t (TupleTerm ys) = TupleTerm $ map (substitute x t) ys

freeVariables :: Term -> Set String
freeVariables (Var x) = Set.singleton x
freeVariables (App a b) = freeVariables a <> freeVariables b
freeVariables (OperatorFunction _) = Set.empty
freeVariables (OperatorSectionLeft _ a) = freeVariables a
freeVariables (OperatorSectionRight a _) = freeVariables a
freeVariables (OperatorApp a _ b) = freeVariables a <> freeVariables b
freeVariables (Fn x a) = Set.delete x (freeVariables a)
freeVariables (PatternFn p a) = Set.difference (freeVariables a) (varsInPattern p)
freeVariables (TupleTerm xs) = Set.unions $ map freeVariables xs

allVariables :: Term -> Set String
allVariables (Var x) = Set.singleton x
allVariables (App a b) = allVariables a <> allVariables b
allVariables (OperatorFunction _) = Set.empty
allVariables (OperatorSectionLeft _ a) = allVariables a
allVariables (OperatorSectionRight a _) = allVariables a
allVariables (OperatorApp a _ b) = allVariables a <> allVariables b
allVariables (Fn x a) = Set.insert x $ allVariables a
allVariables (PatternFn p a) = freeVariables a <> varsInPattern p
allVariables (TupleTerm xs) = Set.unions $ map allVariables xs

defaultPrecedence :: Int
defaultPrecedence = -1

appLeftPrecedence :: Int
appLeftPrecedence = 100

appRightPrecedence :: Int
appRightPrecedence = 201

-- Operators known to our runtime.
--
-- Our convention:
--
-- If Haskell defines infixl N, we define { left: 2 N, assoc: 2 N + 1, right: 2 N + 1 }.
--
-- If Haskell defines infixr N, we define { left: 2 N + 1, assoc: 2 N + 1, right: 2 N }.
--
-- If Haskell defines infix N, we define { left: 2 N + 1, assoc: 2 N + 1, right: 2 N + 1 }.
type Fixity = { left :: Int, assoc :: Int, right :: Int }

infixl' :: Int -> Fixity
infixl' x = { left: 2 * x, assoc: 2 * x + 1, right: 2 * x + 1 }

infixr' :: Int -> Fixity
infixr' x = { left: 2 * x + 1, assoc: 2 * x + 1, right: 2 * x }

infix' :: Int -> Fixity
infix' x = { left: 2 * x + 1, assoc: 2 * x + 1, right: 2 * x + 1 }

operatorPrecedences :: String -> Fixity
operatorPrecedences "$" = infixr' 0 -- in Prelude
operatorPrecedences "+++" = infixr' 2 -- in Control.Arrow
operatorPrecedences "***" = infixr' 3 -- in Control.Arrow
operatorPrecedences "==" = infix' 4 -- in Prelude
operatorPrecedences "<=" = infix' 4 -- in Prelude
operatorPrecedences "<>" = infixr' 6 -- in Prelude
operatorPrecedences _ = infixl' 9 -- the default fixity

prettyShowPrec :: Int -> Term -> String
prettyShowPrec _ (Var x) = x
prettyShowPrec n (App left right) =
    let left' = prettyShowPrec appLeftPrecedence left
        right' = prettyShowPrec appRightPrecedence right in
    parenthesizeIf (n >= appRightPrecedence) $ left' <> " " <> right'
prettyShowPrec _ (OperatorFunction o) =
    parenthesizeIf true o
prettyShowPrec _ (OperatorSectionLeft o a) =
    let fixity = operatorPrecedences o
        a' = prettyShowPrec fixity.right a in
    parenthesizeIf true $ o <> " " <> a'
prettyShowPrec _ (OperatorSectionRight a o) =
    let fixity = operatorPrecedences o
        a' = prettyShowPrec fixity.left a in
    parenthesizeIf true $ a' <> " " <> o
prettyShowPrec n (OperatorApp a o b) =
    let fixity = operatorPrecedences o
        a' = prettyShowPrec fixity.left a
        b' = prettyShowPrec fixity.right b in
    parenthesizeIf (n >= fixity.assoc) $ a' <> " " <> o <> " " <> b'
prettyShowPrec n (Fn var body) =
    parenthesizeIf (n > defaultPrecedence) $ "\\" <> prettyShowLambdaTail (VarPattern var) body
prettyShowPrec n (PatternFn pattern body) =
    parenthesizeIf (n > defaultPrecedence) $ "\\" <> prettyShowLambdaTail pattern body
prettyShowPrec _ (TupleTerm xs) = "(" <> intercalate ", " (map (prettyShowPrec defaultPrecedence) xs) <> ")"

isLambda :: Term -> Boolean
isLambda (Fn _ _) = true
isLambda (PatternFn _ _) = true
isLambda _ = false

prettyShowLambdaTail :: Pattern -> Term -> String
prettyShowLambdaTail p (Fn v body) = prettyShow p <> " " <> prettyShowLambdaTail (VarPattern v) body
prettyShowLambdaTail p (PatternFn p' body) = prettyShow p <> " " <> prettyShowLambdaTail p' body
prettyShowLambdaTail p body = prettyShow p <> " -> " <> prettyShowPrec defaultPrecedence body
