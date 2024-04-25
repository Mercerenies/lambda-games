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
module Lambda.Predicate(
                        Predicate(..), substitute, allVariables, allQuantifiedVariables
                       ) where

import Lambda.PrettyShow (class PrettyShow, prettyShow, parenthesizeIf)
import Lambda.MathShow (class MathShow, Latex(..), texttt)
import Lambda.MathShow (parenthesizeIf) as MathShow
import Lambda.Term (Term(..))
import Lambda.Term (substitute, allVariables) as Term
import Lambda.Type (TType(..))

import Prelude
import Data.Set (Set)
import Data.Set (insert) as Set
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- TODO Can we get this and Relation to all just be specializations of PredicateZipper? It seems kind of silly having a separate type for this.
data Predicate = Equals Term Term
               | Implies Predicate Predicate
               | And Predicate Predicate
               | Or Predicate Predicate
               | Forall String TType Predicate

derive instance Eq Predicate
derive instance Generic Predicate _

instance Show Predicate where
    show x = genericShow x

instance PrettyShow Predicate where
    prettyShow = prettyShowPrec defaultPrecedence

instance MathShow Predicate where
    mathShow = mathShowPrec defaultPrecedence

substitute :: String -> Term -> Predicate -> Predicate
substitute x t = go
    where go (Equals lhs rhs) = Equals (Term.substitute x t lhs) (Term.substitute x t rhs)
          go (Implies lhs rhs) = Implies (go lhs) (go rhs)
          go (And lhs rhs) = And (go lhs) (go rhs)
          go (Or lhs rhs) = Or (go lhs) (go rhs)
          go (Forall x' ttype body)
              | x == x' = Forall x' ttype body
              | otherwise = Forall x' ttype $ go body

-- All variables at BOTH the term and predicate level.
--
-- (TODO Include variables in types here too)
allVariables :: Predicate -> Set String
allVariables (Equals a b) = Term.allVariables a <> Term.allVariables b
allVariables (Implies lhs rhs) = allVariables lhs <> allVariables rhs
allVariables (And lhs rhs) = allVariables lhs <> allVariables rhs
allVariables (Or lhs rhs) = allVariables lhs <> allVariables rhs
allVariables (Forall x _ body) = Set.insert x (allVariables body)

allQuantifiedVariables :: Predicate -> Set String
allQuantifiedVariables (Equals _ _) = mempty
allQuantifiedVariables (Implies lhs rhs) = allQuantifiedVariables lhs <> allQuantifiedVariables rhs
allQuantifiedVariables (And lhs rhs) = allQuantifiedVariables lhs <> allQuantifiedVariables rhs
allQuantifiedVariables (Or lhs rhs) = allQuantifiedVariables lhs <> allQuantifiedVariables rhs
allQuantifiedVariables (Forall x _ body) = Set.insert x (allQuantifiedVariables body)

defaultPrecedence :: Int
defaultPrecedence = 0

impliesRightPrecedence :: Int
impliesRightPrecedence = 1

impliesLeftPrecedence :: Int
impliesLeftPrecedence = 2

orPrecedence :: Int
orPrecedence = 3

andPrecedence :: Int
andPrecedence = 4

-- Just parenthesize operators, to be completely unambiguous.
termNeedsParens :: Term -> Boolean
termNeedsParens (OperatorApp _ _ _) = true
termNeedsParens _ = false

prettyShowPrec :: Int -> Predicate -> String
prettyShowPrec _ (Equals a b) =
    let a' = parenthesizeIf (termNeedsParens a) $ prettyShow a
        b' = parenthesizeIf (termNeedsParens b) $ prettyShow b in
    a' <> " = " <> b'
prettyShowPrec n (Implies lhs rhs) =
    let lhs' = prettyShowPrec impliesLeftPrecedence lhs
        rhs' = prettyShowPrec impliesRightPrecedence rhs in
    parenthesizeIf (n >= impliesLeftPrecedence) $ lhs' <> " => " <> rhs'
prettyShowPrec n (Or lhs rhs) =
    let lhs' = prettyShowPrec orPrecedence lhs
        rhs' = prettyShowPrec orPrecedence rhs in
    parenthesizeIf (n > orPrecedence) $ lhs' <> " ∨ " <> rhs'
prettyShowPrec n (And lhs rhs) =
    let lhs' = prettyShowPrec andPrecedence lhs
        rhs' = prettyShowPrec andPrecedence rhs in
    parenthesizeIf (n > andPrecedence) $ lhs' <> " ∧ " <> rhs'
prettyShowPrec n (Forall var varType body) =
    parenthesizeIf (n >= impliesLeftPrecedence) $ "∀ " <> prettyShowForall var varType body

-- We collate consecutive foralls over the same domain, to get prettier output.
prettyShowForall :: String -> TType -> Predicate -> String
prettyShowForall var varType (Forall var' varType' body') | varType == varType' =
    var <> " " <> prettyShowForall var' varType' body'
prettyShowForall var varType body =
    let body' = prettyShowPrec defaultPrecedence body
        varType' = prettyShowQuantifiedType varType in
    var <> ": " <> varType' <> ". " <> body'

-- Parenthesize the type if it's "complicated", so that the resulting
-- output looks nicer. This is mostly a heuristic thing and is just
-- designed to pretty up the output.
prettyShowQuantifiedType :: TType -> String
prettyShowQuantifiedType t = parenthesizeIf (isComplex t) $ prettyShow t
    where isComplex (TForall _ _) = true
          isComplex (TArrow _ rhs) = isComplex rhs
          isComplex _ = false

mathShowPrec :: Int -> Predicate -> Latex
mathShowPrec _ (Equals a b) =
    let a' = MathShow.parenthesizeIf (termNeedsParens a) $ texttt (prettyShow a)
        b' = MathShow.parenthesizeIf (termNeedsParens b) $ texttt (prettyShow b) in
    a' <> Latex " = " <> b'
mathShowPrec n (Implies lhs rhs) =
    let lhs' = mathShowPrec impliesLeftPrecedence lhs
        rhs' = mathShowPrec impliesRightPrecedence rhs in
    MathShow.parenthesizeIf (n >= impliesLeftPrecedence) $ lhs' <> Latex " \\implies " <> rhs'
mathShowPrec n (Or lhs rhs) =
    let lhs' = mathShowPrec orPrecedence lhs
        rhs' = mathShowPrec orPrecedence rhs in
    MathShow.parenthesizeIf (n > orPrecedence) $ lhs' <> Latex " \\vee " <> rhs'
mathShowPrec n (And lhs rhs) =
    let lhs' = mathShowPrec andPrecedence lhs
        rhs' = mathShowPrec andPrecedence rhs in
    MathShow.parenthesizeIf (n > andPrecedence) $ lhs' <> Latex " \\wedge " <> rhs'
mathShowPrec n (Forall var varType body) =
    MathShow.parenthesizeIf (n >= impliesLeftPrecedence) $ Latex "\\forall " <> mathShowForall var varType body

-- We collate consecutive foralls over the same domain, to get prettier output.
mathShowForall :: String -> TType -> Predicate -> Latex
mathShowForall var varType (Forall var' varType' body') | varType == varType' =
    texttt var <> Latex ", " <> mathShowForall var' varType' body'
mathShowForall var varType body =
    let body' = mathShowPrec defaultPrecedence body
        varType' = prettyShowQuantifiedType varType in
    texttt var <> Latex ": " <> texttt varType' <> Latex ". \\, " <> body'
