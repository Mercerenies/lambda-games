
module Lambda.Type.Builtins(
                            reservedNames,
                            basicType, listType,
                            namedBuiltinsMap, allBuiltins
                           ) where

import Lambda.Type.Relation (Relation, identityRelation)
import Lambda.Type.Functions (Lambda(..), lambda1)
import Lambda.Type.Error (class FromKindError)
import Lambda.Type.BuiltinsMap (BuiltinsMap(..))
import Lambda.Term (Term(..), allVariables)
import Lambda.Util.InfiniteList (InfiniteList, intersperse)
import Lambda.Util.InfiniteList (find) as InfiniteList
import Lambda.Monad.Names (class MonadNames, freshStrings)
import Lambda.LookupMap (LookupMap)
import Lambda.LookupMap (fromMap) as LookupMap

import Data.List (List(..), (:))
import Data.Set (member) as Set
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Data.Bifunctor (bimap)
import Data.String.Common (toLower)
import Control.Monad.Error.Class (class MonadError)
import Prelude

-- Names that we might introduce and assume are not being shadowed by
-- a lambda parameter anywhere.
reservedNames :: Array String
reservedNames = ["fmap"]

-- Names for use in generated lambda expressions.
temporaryVarNames :: InfiniteList String
temporaryVarNames = intersperse (freshStrings "x" :| freshStrings "y" : freshStrings "z" : freshStrings "w" : Nil)

-- Lift a function on terms to a lambda at the language level that
-- fills in the same holes as the original Purescript function.
--
-- Precondition: The function shall be a simple zipper on Term. That
-- is, it should simply fill in holes and should NOT introspect on the
-- term argument.
liftToLambda :: (Term -> Term) -> Term
liftToLambda f = Fn newVar (f (Var newVar))
    where allVars = allVariables (f (Var "_")) -- Just fill in a placeholder variable
          newVar = InfiniteList.find (\var -> not (var `Set.member` allVars)) temporaryVarNames

basicType :: forall m. Lambda m Relation
basicType = Ground identityRelation

listType :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Lambda m Relation
listType = lambda1 \r -> pure $ bimap liftToFmap liftToFmap r
    where liftToFmap :: (Term -> Term) -> Term -> Term
          liftToFmap f = App (App (Var "fmap") (liftToLambda f))

namedBuiltinsMap :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Map String (Lambda m Relation)
namedBuiltinsMap = Map.fromFoldable [
                    Tuple "Int" basicType,
                    Tuple "Float" basicType,
                    Tuple "Double" basicType,
                    Tuple "String" basicType,
                    Tuple "Boolean" basicType,
                    Tuple "List" listType
                   ]

allBuiltins :: forall e m. FromKindError e => MonadNames String m => MonadError e m => BuiltinsMap m
allBuiltins = BuiltinsMap { lookupMap: LookupMap.fromMap namedBuiltinsMap }
