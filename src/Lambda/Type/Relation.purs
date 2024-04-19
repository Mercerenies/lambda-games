
module Lambda.Type.Relation(
                            Relation(..), runRelation, identityRelation,
                            describeRelation
                           ) where

import Lambda.Predicate (Predicate(..))
import Lambda.Term (Term(..))
import Lambda.PrettyShow (prettyShow)

import Prelude

newtype Relation = Relation (Term -> Term -> Predicate)

runRelation :: Relation -> Term -> Term -> Predicate
runRelation (Relation r) = r

identityRelation :: Relation
identityRelation = Relation Equals

describeRelation :: Relation -> Term -> Term -> String
describeRelation (Relation r) left right = prettyShow (r left right)
