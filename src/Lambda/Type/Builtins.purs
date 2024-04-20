
module Lambda.Type.Builtins(
                            basicType, listType,
                            namedBuiltinsMap, allBuiltins
                           ) where

import Lambda.Type (TType(..))
import Lambda.Type.Relation (Relation(..), identityRelation, runRelation)
import Lambda.Type.Functions (Lambda(..), lambda1)
import Lambda.Type.Error (class FromKindError)
import Lambda.Term (Term(..))
import Lambda.Util.InfiniteList (InfiniteList, intersperse)
import Lambda.Predicate (Predicate(..), equals)
import Lambda.Monad.Names (class MonadNames, withFreshName, freshStrings)
import Lambda.LookupMap (LookupMap)
import Lambda.LookupMap (fromMap) as LookupMap

import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Control.Monad.Error.Class (class MonadError)
import Prelude

indexNames :: InfiniteList String
indexNames = intersperse (freshStrings "i" :| freshStrings "j" : freshStrings "k" : Nil)

basicType :: forall m. Lambda m Relation
basicType = Ground identityRelation

listType :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Lambda m Relation
listType = lambda1 \r -> ado
    innerRelation <- withFreshName indexNames \i -> pure (elementwiseConstraint r i)
    in Relation \xs ys -> And (lengthConstraint xs ys) (runRelation innerRelation xs ys)
  where lengthConstraint xs ys =
            App (Var "length") xs `equals` App (Var "length") ys
        elementwiseConstraint r i =
            Relation \xs ys -> Forall i (TVar "Nat") $
                                 Operator "<" (Var i) (App (Var "length") xs) `Implies`
                                   runRelation r (Subscript xs (Var i)) (Subscript ys (Var i))

namedBuiltinsMap :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Map String (Lambda m Relation)
namedBuiltinsMap = Map.fromFoldable [
                    Tuple "Int" basicType,
                    Tuple "Float" basicType,
                    Tuple "Double" basicType,
                    Tuple "String" basicType,
                    Tuple "Boolean" basicType,
                    Tuple "List" listType
                   ]

allBuiltins :: forall e m. FromKindError e => MonadNames String m => MonadError e m => LookupMap String (Lambda m Relation)
allBuiltins = LookupMap.fromMap namedBuiltinsMap
