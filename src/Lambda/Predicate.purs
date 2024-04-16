
module Lambda.Predicate(
                        Predicate(..)
                       ) where

import Lambda.PrettyShow(class PrettyShow, prettyShow, parenthesizeIf)
import Lambda.Term(Term)
import Lambda.Type(TType)

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Predicate = Equals Term Term
               | Implies Predicate Predicate
               | Forall String TType Predicate

derive instance Eq Predicate
derive instance Generic Predicate _

instance Show Predicate where
    show x = genericShow x

instance PrettyShow Predicate where
    prettyShow = prettyShowPrec defaultPrecedence

defaultPrecedence :: Int
defaultPrecedence = 0

impliesRightPrecedence :: Int
impliesRightPrecedence = 1

impliesLeftPrecedence :: Int
impliesLeftPrecedence = 2

prettyShowPrec :: Int -> Predicate -> String
prettyShowPrec _ (Equals a b) = prettyShow a <> " = " <> prettyShow b
prettyShowPrec n (Implies lhs rhs) =
    let lhs' = prettyShowPrec impliesLeftPrecedence lhs
        rhs' = prettyShowPrec impliesRightPrecedence rhs
    in parenthesizeIf (n >= impliesLeftPrecedence) $ lhs' <> " => " <> rhs'
prettyShowPrec _ (Forall var varType body) =
    let body' = prettyShowPrec defaultPrecedence body in
    "∀ " <> var <> " : " <> prettyShow varType <> " . " <> body'
