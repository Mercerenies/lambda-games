
module Lambda.Predicate(
                        Predicate(..), equals, substitute, allVariables
                       ) where

import Lambda.PrettyShow (class PrettyShow, prettyShow, parenthesizeIf)
import Lambda.Term (Term)
import Lambda.Term (substitute, allVariables) as Term
import Lambda.Type (TType(..))

import Prelude
import Data.Set (Set)
import Data.Set (insert) as Set
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Predicate = Operator String Term Term
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

equals :: Term -> Term -> Predicate
equals = Operator "="

substitute :: String -> Term -> Predicate -> Predicate
substitute x t = go
    where go (Operator op lhs rhs) = Operator op (Term.substitute x t lhs) (Term.substitute x t rhs)
          go (Implies lhs rhs) = Implies (go lhs) (go rhs)
          go (And lhs rhs) = And (go lhs) (go rhs)
          go (Or lhs rhs) = Or (go lhs) (go rhs)
          go (Forall x' ttype body)
              | x == x' = Forall x' ttype body
              | otherwise = Forall x' ttype $ go body

-- All variables at BOTH the term and predicate level.
allVariables :: Predicate -> Set String
allVariables (Operator _ a b) = Term.allVariables a <> Term.allVariables b
allVariables (Implies lhs rhs) = allVariables lhs <> allVariables rhs
allVariables (And lhs rhs) = allVariables lhs <> allVariables rhs
allVariables (Or lhs rhs) = allVariables lhs <> allVariables rhs
allVariables (Forall x _ body) = Set.insert x (allVariables body)

allQuantifiedVariables :: Predicate -> Set String
allQuantifiedVariables (Operator _ _ _) = mempty
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

prettyShowPrec :: Int -> Predicate -> String
prettyShowPrec _ (Operator op a b) = prettyShow a <> " " <> op <> " " <> prettyShow b
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
