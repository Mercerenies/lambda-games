
module Lambda.Type.Relation(
                            Relation(..), runRelation, identityRelation,
                            describeRelation
                           ) where

import Lambda.Predicate (Predicate, equals)
import Lambda.Term (Term)
import Lambda.PrettyShow (prettyShow)

newtype Relation = Relation (Term -> Term -> Predicate)

runRelation :: Relation -> Term -> Term -> Predicate
runRelation (Relation r) = r

identityRelation :: Relation
identityRelation = Relation equals

describeRelation :: Relation -> Term -> Term -> String
describeRelation (Relation r) left right = prettyShow (r left right)
