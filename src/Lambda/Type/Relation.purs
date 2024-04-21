
module Lambda.Type.Relation(
                            PredicateZipper, runPredicateZipper,
                            Relation, runRelation, identityRelation,
                            rImplies, rForall,
                            mapTerms,
                            describeRelation
                           ) where

import Lambda.Predicate (Predicate(..), equals)
import Lambda.Term (Term)
import Lambda.Type (TType)
import Lambda.PrettyShow (prettyShow)

import Prelude
import Data.Bifunctor (class Bifunctor, bimap)
import Control.Biapply (class Biapply, biapply)
import Control.Biapplicative (class Biapplicative)

-- A Predicate with holes of types a1 and a2 in place of the terms.
data PredicateZipper a b = PEquals a b |
                           PImplies Predicate (PredicateZipper a b) | -- Note: Left-hand is still just Predicate for now
                           PForall String TType (PredicateZipper a b)

-- A relation is defined to be a predicate quantified by two terms.
-- You can think of it as (Term -> Term -> Predicate) but more
-- restricted so we can introspect on the values.
type Relation = PredicateZipper (Term -> Term) (Term -> Term)

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
