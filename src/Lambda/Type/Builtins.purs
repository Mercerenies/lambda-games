
module Lambda.Type.Builtins(
                            reservedNames,
                            basicBuiltin, listBuiltin, tuple2Builtin, tuple3Builtin,
                            tuple4Builtin, tuple5Builtin, eitherBuiltin,
                            namedBuiltinsMap, allBuiltins
                           ) where

import Lambda.Type.Relation (Relation, identityRelation,
                             zipRelationsWith, zipRelationsWith3, zipRelationsWith4, zipRelationsWith5,
                             TermHole)
import Lambda.Type.Functions (Lambda(..))
import Lambda.Type.Functions.Factory (lambda1, lambda2, lambda3, lambda4, lambda5)
import Lambda.Type.Error (class FromKindError)
import Lambda.Type.BuiltinsMap (BuiltinsMap(..), Builtin(..))
import Lambda.Term (Term(..), Pattern(..), allVariables)
import Lambda.Util (toList)
import Lambda.Util.InfiniteList (InfiniteList)
import Lambda.Util.InfiniteList (find, take) as InfiniteList
import Lambda.Monad.Names (class MonadNames, freshStrings, interspersedStrings, alphabetForever)
import Lambda.LookupMap (fromMap) as LookupMap

import Data.List (List(..), (:), zipWith)
import Data.Set (member) as Set
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Data.Bifunctor (bimap)
import Data.Foldable (class Foldable, foldMap, length)
import Control.Monad.Error.Class (class MonadError)
import Prelude

-- Names that we might introduce and assume are not being shadowed by
-- a lambda parameter anywhere.
reservedNames :: Array String
reservedNames = ["fmap", "first", "second", "Left", "Right", "left", "right"]

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
tuple2Type = lambda2 \ra rb -> zipRelationsWith liftToTuple2 liftToTuple2 ra rb
    where liftToTuple2 :: TermHole -> TermHole -> TermHole
          liftToTuple2 a b = App (OperatorApp (liftToLambda a) "***" (liftToLambda b))

tuple2Builtin :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Builtin m
tuple2Builtin = Builtin {
                  relation: tuple2Type,
                  nameStream: freshStrings "pair"
                }

-- The function written here is:
--
-- map3 f g h (a, b, c) = (f a, g b, h c)
-- map4 f g h k (a, b, c, d) = (f a, g b, h c, k d)
-- ... etc ...
--
-- But partially applied to the functions and written in the term
-- language.
tupleMap :: forall f. Foldable f => f Term -> Term
tupleMap fs =
    let allVars = foldMap allVariables fs
        newVar prefix = InfiniteList.find (\var -> not (var `Set.member` allVars)) (freshStrings prefix)
        newVars = InfiniteList.take (length fs) $ map newVar alphabetForever
        tuplePattern = TuplePattern $ map VarPattern newVars
        tupleTerm = TupleTerm $ zipWith (\f v -> f `App` Var v) (toList fs) newVars in
    PatternFn tuplePattern tupleTerm

liftToTuple :: Array TermHole -> TermHole
liftToTuple xs = App (tupleMap $ map liftToLambda xs)

tuple3Type :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Lambda m Relation
tuple3Type = lambda3 \ra rb rc -> zipRelationsWith3 go go ra rb rc
    where go a b c = liftToTuple [a, b, c]

tuple3Builtin :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Builtin m
tuple3Builtin = Builtin {
                  relation: tuple3Type,
                  nameStream: freshStrings "trip"
                }

tuple4Type :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Lambda m Relation
tuple4Type = lambda4 \ra rb rc rd -> zipRelationsWith4 go go ra rb rc rd
    where go a b c d = liftToTuple [a, b, c, d]

tuple4Builtin :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Builtin m
tuple4Builtin = Builtin {
                  relation: tuple4Type,
                  nameStream: freshStrings "tup"
                }

tuple5Type :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Lambda m Relation
tuple5Type = lambda5 \ra rb rc rd re -> zipRelationsWith5 go go ra rb rc rd re
    where go a b c d e = liftToTuple [a, b, c, d, e]

tuple5Builtin :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Builtin m
tuple5Builtin = Builtin {
                  relation: tuple5Type,
                  nameStream: freshStrings "tup"
                }

eitherType :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Lambda m Relation
eitherType = lambda2 \ra rb -> zipRelationsWith liftToEither liftToEither ra rb
    where liftToEither :: TermHole -> TermHole -> TermHole
          liftToEither a b = App (OperatorApp (liftToLambda a) "+++" (liftToLambda b))

eitherBuiltin :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Builtin m
eitherBuiltin = Builtin {
                  relation: eitherType,
                  nameStream: freshStrings "eit"
                }

namedBuiltinsMap :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Map String (Builtin m)
namedBuiltinsMap = Map.fromFoldable [
                    Tuple "Int" $ basicBuiltin (freshStrings "n"),
                    Tuple "Float" $ basicBuiltin (freshStrings "f"),
                    Tuple "Double" $ basicBuiltin (freshStrings "d"),
                    Tuple "String" $ basicBuiltin (freshStrings "s"),
                    Tuple "Boolean" $ basicBuiltin (freshStrings "b"),
                    Tuple "List" listBuiltin,
                    Tuple "Tuple0" $ basicBuiltin (freshStrings "unit"),
                    Tuple "Void" $ basicBuiltin (freshStrings "void"),
                    Tuple "Tuple2" tuple2Builtin,
                    Tuple "Tuple3" tuple3Builtin,
                    Tuple "Tuple4" tuple4Builtin,
                    Tuple "Tuple5" tuple5Builtin,
                    Tuple "Either" eitherBuiltin
                   ]

allBuiltins :: forall e m. FromKindError e => MonadNames String m => MonadError e m => BuiltinsMap m
allBuiltins = BuiltinsMap (LookupMap.fromMap namedBuiltinsMap)
