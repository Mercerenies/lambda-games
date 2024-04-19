
module Lambda.Type.Relation(
                            Relation(..), runRelation, identityRelation,
                            LambdaContextT,
                            describeRelation
                           ) where

import Lambda.Predicate (Predicate, equals)
import Lambda.Term (Term)
import Lambda.PrettyShow (prettyShow)
import Lambda.Type.Error (KindError)
import Lambda.Monad.Names (NamesT)

import Control.Monad.Except.Trans (ExceptT)

newtype Relation = Relation (Term -> Term -> Predicate)

-- The type that Lambda functions (at the type level) must run in (for
-- underlying monad m).
type LambdaContextT m = ExceptT KindError (NamesT String m)

runRelation :: Relation -> Term -> Term -> Predicate
runRelation (Relation r) = r

identityRelation :: Relation
identityRelation = Relation equals

describeRelation :: Relation -> Term -> Term -> String
describeRelation (Relation r) left right = prettyShow (r left right)
