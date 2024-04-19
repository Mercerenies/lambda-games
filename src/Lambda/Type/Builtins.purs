
module Lambda.Type.Builtins where

import Lambda.Type (TType(..))
import Lambda.Type.Relation (Relation(..), runRelation)
import Lambda.Type.Functions (Lambda, lambda1)
import Lambda.Type.Error (KindError)
import Lambda.Term (Term(..))
import Lambda.Util.InfiniteList (InfiniteList, intersperse)
import Lambda.Predicate (Predicate(..), equals)
import Lambda.Monad.Names (class MonadNames, withFreshName, freshStrings)

import Data.List (List(..), (:))
import Data.NonEmpty ((:|))
import Control.Monad.Error.Class (class MonadError)
import Prelude

indexNames :: InfiniteList String
indexNames = intersperse (freshStrings "i" :| freshStrings "j" : freshStrings "k" : Nil)

listType :: forall m. MonadNames String m => MonadError KindError m => Lambda m Relation
listType = lambda1 \r -> ado
    innerRelation <- withFreshName indexNames \i -> pure (elementwiseConstraint r i)
    in Relation \xs ys -> And (lengthConstraint xs ys) (runRelation innerRelation xs ys)
  where lengthConstraint xs ys =
            App (Var "length") xs `equals` App (Var "length") ys
        elementwiseConstraint r i =
            Relation \xs ys -> Forall i (TVar "Nat") $
                                 Operator "<" (Var i) (App (Var "length") xs) `Implies`
                                   runRelation r (Subscript xs (Var i)) (Subscript ys (Var i))

