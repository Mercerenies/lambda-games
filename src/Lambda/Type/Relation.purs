
module Lambda.Type.Relation(
                            Relation(..), identityRelation,
                            describeRelation
                           ) where

import Lambda.Predicate (Predicate(..))
import Lambda.Term (Term(..))
import Lambda.PrettyShow (prettyShow)

import Prelude

newtype Relation = Relation (Term -> Term -> Predicate)

identityRelation :: Relation
identityRelation = Relation Equals

describeRelation :: Relation -> Term -> Term -> String
describeRelation (Relation r) left right = prettyShow (r left right)
