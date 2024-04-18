
module Lambda.Predicate(
                        Predicate(..), substitute
                       ) where

import Lambda.PrettyShow (class PrettyShow, prettyShow, parenthesizeIf)
import Lambda.Term (Term)
import Lambda.Term (substitute) as Term
import Lambda.Type (TType)

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

substitute :: String -> Term -> Predicate -> Predicate
substitute x t = go
    where go (Equals lhs rhs) = Equals (Term.substitute x t lhs) (Term.substitute x t rhs)
          go (Implies lhs rhs) = Implies (go lhs) (go rhs)
          go (Forall x' ttype body)
              | x == x' = Forall x' ttype body
              | otherwise = Forall x' ttype $ go body

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
        rhs' = prettyShowPrec impliesRightPrecedence rhs in
    parenthesizeIf (n >= impliesLeftPrecedence) $ lhs' <> " => " <> rhs'
prettyShowPrec n (Forall var varType body) =
    parenthesizeIf (n >= impliesLeftPrecedence) $ "∀ " <> prettyShowForall var varType body

-- We collate consecutive foralls over the same domain, to get prettier output.
prettyShowForall :: String -> TType -> Predicate -> String
prettyShowForall var varType (Forall var' varType' body') | varType == varType' =
    var <> " " <> prettyShowForall var' varType' body'
prettyShowForall var varType body =
    let body' = prettyShowPrec defaultPrecedence body in
    var <> ": " <> prettyShow varType <> ". " <> body'
