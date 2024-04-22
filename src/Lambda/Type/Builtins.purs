
module Lambda.Type.Builtins(
                            reservedNames,
                            basicBuiltin, listBuiltin, tuple2Builtin,
                            namedBuiltinsMap, allBuiltins
                           ) where

import Lambda.Type.Relation (Relation, identityRelation, zipRelationsWith, TermHole)
import Lambda.Type.Functions (Lambda(..))
import Lambda.Type.Functions.Factory (lambda1, lambda2)
import Lambda.Type.Error (class FromKindError)
import Lambda.Type.BuiltinsMap (BuiltinsMap(..), Builtin(..))
import Lambda.Term (Term(..), allVariables)
import Lambda.Util.InfiniteList (InfiniteList)
import Lambda.Util.InfiniteList (find) as InfiniteList
import Lambda.Monad.Names (class MonadNames, freshStrings, interspersedStrings)
import Lambda.LookupMap (fromMap) as LookupMap

import Data.List (List(..), (:))
import Data.Set (member) as Set
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Data.Bifunctor (bimap)
import Control.Monad.Error.Class (class MonadError)
import Prelude

-- Names that we might introduce and assume are not being shadowed by
-- a lambda parameter anywhere.
reservedNames :: Array String
reservedNames = ["fmap", "first", "second"]

-- Names for use in generated lambda expressions.
temporaryVarNames :: InfiniteList String
temporaryVarNames = interspersedStrings ("x" :| "y" : "z" : "w" : Nil)

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

basicBuiltin :: forall m. InfiniteList String -> Builtin m
basicBuiltin nameStream = Builtin {
                            relation: Ground identityRelation,
                            nameStream
                          }

listType :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Lambda m Relation
listType = lambda1 \r -> bimap liftToFmap liftToFmap r
    where liftToFmap :: TermHole -> TermHole
          liftToFmap f = App (App (Var "fmap") (liftToLambda f))

listNames :: InfiniteList String
listNames = interspersedStrings ("xs" :| "ys" : "zs" : "ws" : Nil)

listBuiltin :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Builtin m
listBuiltin = Builtin {
                relation: listType,
                nameStream: listNames
              }

tuple2Type :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Lambda m Relation
tuple2Type = lambda2 \ra rb -> zipRelationsWith liftToTuple liftToTuple ra rb
    where liftToTuple :: TermHole -> TermHole -> TermHole
          liftToTuple lhs rhs = App (OperatorApp (liftToLambda lhs) "***" (liftToLambda rhs))

tuple2Builtin :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Builtin m
tuple2Builtin = Builtin {
                  relation: tuple2Type,
                  nameStream: freshStrings "tup"
                }

namedBuiltinsMap :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Map String (Builtin m)
namedBuiltinsMap = Map.fromFoldable [
                    Tuple "Int" $ basicBuiltin (freshStrings "n"),
                    Tuple "Float" $ basicBuiltin (freshStrings "f"),
                    Tuple "Double" $ basicBuiltin (freshStrings "d"),
                    Tuple "String" $ basicBuiltin (freshStrings "s"),
                    Tuple "Boolean" $ basicBuiltin (freshStrings "b"),
                    Tuple "List" listBuiltin,
                    Tuple "Tuple2" tuple2Builtin
                   ]

allBuiltins :: forall e m. FromKindError e => MonadNames String m => MonadError e m => BuiltinsMap m
allBuiltins = BuiltinsMap (LookupMap.fromMap namedBuiltinsMap)
