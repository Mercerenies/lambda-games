
module Lambda.Type.Relation(
                            PredicateZipper, runPredicateZipper,
                            Relation, runRelation, identityRelation,
                            rImplies, rForall,
                            mapTerms,
                            describeRelation,
                            allVariablesWith, allVariables, allQuantifiedVariables
                           ) where

import Lambda.Predicate (Predicate(..), equals)
import Lambda.Predicate (allVariables, allQuantifiedVariables) as Predicate
import Lambda.Term (Term(..))
import Lambda.Term (allVariables) as Term
import Lambda.Type (TType)
import Lambda.PrettyShow (prettyShow)

import Prelude
import Data.Bifunctor (class Bifunctor, bimap)
import Control.Biapply (class Biapply, biapply)
import Control.Biapplicative (class Biapplicative)
import Data.Set (Set)
import Data.Set (insert, delete) as Set

-- A Predicate with holes of types a1 and a2 in place of the terms.
data PredicateZipper a b = PEquals a b |
                           PImplies Predicate (PredicateZipper a b) | -- Note: Left-hand is still just Predicate for now
                           PForall String TType (PredicateZipper a b)

-- A relation is defined to be a predicate quantified by two terms.
-- You can think of it as (Term -> Term -> Predicate) but more
-- restricted so we can introspect on the values.
type Relation = PredicateZipper (Term -> Term) (Term -> Term)

-- Note: The Bifunctor, Biapply, and Biapplicative instances might
-- look a little weird, but they do obey the corresponding typeclass
-- laws. Proof of conformance is available (in Agda) at
-- https://gist.github.com/Mercerenies/9c839c412a8b9324fb1c3085a2b41111

instance Bifunctor PredicateZipper where
    bimap f g (PEquals a b) = PEquals (f a) (g b)
    bimap f g (PImplies lhs rhs) = PImplies lhs (bimap f g rhs)
    bimap f g (PForall name ttype rhs) = PForall name ttype (bimap f g rhs)

instance Biapply PredicateZipper where
    biapply (PEquals f g) (PEquals a b) = PEquals (f a) (g b)
    biapply (PImplies lhs rhs) y = PImplies lhs (biapply rhs y)
    biapply (PForall name ttype rhs) y = PForall name ttype (biapply rhs y)
    biapply x (PImplies lhs rhs) = PImplies lhs (biapply x rhs)
    biapply x (PForall name ttype rhs) = PForall name ttype (biapply x rhs)

instance Biapplicative PredicateZipper where
    bipure = PEquals

runPredicateZipper :: forall a b. PredicateZipper a b -> (a -> Term) -> (b -> Term) -> Predicate
runPredicateZipper p f g = go p
    where go (PEquals a b) = equals (f a) (g b)
          go (PImplies lhs rhs) = Implies lhs (go rhs)
          go (PForall name ttype rhs) = Forall name ttype (go rhs)

runRelation :: Relation -> Term -> Term -> Predicate
runRelation r a b = runPredicateZipper r (_ $ a) (_ $ b)

identityRelation :: forall a b. PredicateZipper (a -> a) (b -> b)
identityRelation = PEquals identity identity

rImplies :: forall a b. Predicate -> PredicateZipper a b -> PredicateZipper a b
rImplies = PImplies

rForall :: forall a b. String -> TType -> PredicateZipper a b -> PredicateZipper a b
rForall = PForall

mapTerms :: forall a a' b b' c d. (a' -> a) -> (b' -> b) -> PredicateZipper (a -> c) (b -> d) -> PredicateZipper (a' -> c) (b' -> d)
mapTerms f g = bimap (_ <<< f) (_ <<< g)

describeRelation :: Relation -> Term -> Term -> String
describeRelation r left right = prettyShow (runRelation r left right)

-- (TODO Include variables in types here too)
allVariablesWith :: forall a b. (a -> Set String) -> (b -> Set String) -> (Predicate -> Set String) ->
                    PredicateZipper a b -> Set String
allVariablesWith fa fb fpred = go
    where go (PEquals a b) = fa a <> fb b
          go (PImplies lhs rhs) = fpred lhs <> go rhs
          go (PForall name _ rhs) = Set.insert name (go rhs)

allVariables :: PredicateZipper (Term -> Term) (Term -> Term) -> Set String
allVariables = allVariablesWith termVars termVars Predicate.allVariables
    where -- For term variables, we simply substitute `TVar "_"` in
          -- and check the resulting term. Our parser forbids the use
          -- of `TVar "_"` as a variable name, so we can safely remove
          -- it from the resutl.
          termVars termFunction = Set.delete "_" $ Term.allVariables (termFunction (Var "_"))

allQuantifiedVariables :: forall a b. PredicateZipper a b -> Set String
allQuantifiedVariables = allVariablesWith (const mempty) (const mempty) Predicate.allQuantifiedVariables
