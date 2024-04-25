-- Copyright 2024 Silvio Mayolo
--
-- Lambdagames is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Lambdagames. If not, see
-- <https://www.gnu.org/licenses/>.
module Lambda.Type.Builtins(
                            reservedNames,
                            basicBuiltin, listBuiltin, tuple2Builtin, tuple3Builtin,
                            tuple4Builtin, tuple5Builtin, eitherBuiltin, semigroupBuiltin,
                            monoidBuiltin, eqBuiltin,
                            namedBuiltinsMap, allBuiltins
                           ) where

import Lambda.Type (TType(..))
import Lambda.Type.Relation (Relation, identityRelation,
                             zipRelationsWith, zipRelationsWith3, zipRelationsWith4, zipRelationsWith5,
                             TermHole)
import Lambda.Type.Functions (TaggedLambda, TaggedLambdaF(..), LambdaF(..))
import Lambda.Type.Functions.Factory (lambda1, lambda2, lambda3, lambda4, lambda5, lambdaCtx1)
import Lambda.Type.Error (class FromKindError)
import Lambda.Type.BuiltinsMap (BuiltinsMap(..), Builtin(..))
import Lambda.Type.Typeclass (WithContexts(..), TypeclassBody, MethodName(..))
import Lambda.Type.Typeclass (singleton) as Typeclass
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
reservedNames = ["fmap", "first", "second", "Left", "Right", "left", "right", "mempty"]

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

basicBuiltin :: forall m. TType -> InfiniteList String -> Builtin m
basicBuiltin ttype nameStream = Builtin {
                                  relation: TaggedLambdaF ttype $ Ground (NonContext identityRelation),
                                  nameStream
                                }

-- Interesting note: `[a]` and `Maybe a` both have free theorems that
-- can be stated the exact same way in terms of fmap.
liftToFmap :: TermHole -> TermHole
liftToFmap f = App (App (Var "fmap") (liftToLambda f))

listType :: forall e m. FromKindError e => MonadNames String m => MonadError e m =>
            TaggedLambda TType m (WithContexts Relation)
listType = lambda1 (TGround "List") \r -> NonContext $ bimap liftToFmap liftToFmap r

listNames :: InfiniteList String
listNames = interspersedStrings ("xs" :| "ys" : "zs" : "ws" : Nil)

listBuiltin :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Builtin m
listBuiltin = Builtin {
                relation: listType,
                nameStream: listNames
              }

maybeType :: forall e m. FromKindError e => MonadNames String m => MonadError e m =>
             TaggedLambda TType m (WithContexts Relation)
maybeType = lambda1 (TGround "Maybe") \r -> NonContext $ bimap liftToFmap liftToFmap r

maybeNames :: InfiniteList String
maybeNames = interspersedStrings ("mx" :| "my" : "mz" : "mw" : Nil)

maybeBuiltin :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Builtin m
maybeBuiltin = Builtin {
                 relation: maybeType,
                 nameStream: maybeNames
               }

tuple2Type :: forall e m. FromKindError e => MonadNames String m => MonadError e m =>
              TaggedLambda TType m (WithContexts Relation)
tuple2Type = lambda2 (TGround "Tuple2") \ra rb -> NonContext $ zipRelationsWith liftToTuple2 liftToTuple2 ra rb
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

tuple3Type :: forall e m. FromKindError e => MonadNames String m => MonadError e m =>
              TaggedLambda TType m (WithContexts Relation)
tuple3Type = lambda3 (TGround "Tuple3") \ra rb rc -> NonContext $ zipRelationsWith3 go go ra rb rc
    where go a b c = liftToTuple [a, b, c]

tuple3Builtin :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Builtin m
tuple3Builtin = Builtin {
                  relation: tuple3Type,
                  nameStream: freshStrings "trip"
                }

tuple4Type :: forall e m. FromKindError e => MonadNames String m => MonadError e m =>
              TaggedLambda TType m (WithContexts Relation)
tuple4Type = lambda4 (TGround "Tuple4") \ra rb rc rd -> NonContext $ zipRelationsWith4 go go ra rb rc rd
    where go a b c d = liftToTuple [a, b, c, d]

tuple4Builtin :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Builtin m
tuple4Builtin = Builtin {
                  relation: tuple4Type,
                  nameStream: freshStrings "tup"
                }

tuple5Type :: forall e m. FromKindError e => MonadNames String m => MonadError e m =>
              TaggedLambda TType m (WithContexts Relation)
tuple5Type = lambda5 (TGround "Tuple5") \ra rb rc rd re -> NonContext $ zipRelationsWith5 go go ra rb rc rd re
    where go a b c d e = liftToTuple [a, b, c, d, e]

tuple5Builtin :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Builtin m
tuple5Builtin = Builtin {
                  relation: tuple5Type,
                  nameStream: freshStrings "tup"
                }

eitherType :: forall e m. FromKindError e => MonadNames String m => MonadError e m =>
              TaggedLambda TType m (WithContexts Relation)
eitherType = lambda2 (TGround "Either") \ra rb -> NonContext $ zipRelationsWith liftToEither liftToEither ra rb
    where liftToEither :: TermHole -> TermHole -> TermHole
          liftToEither a b = App (OperatorApp (liftToLambda a) "+++" (liftToLambda b))

eitherBuiltin :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Builtin m
eitherBuiltin = Builtin {
                  relation: eitherType,
                  nameStream: freshStrings "eit"
                }

-- Builtins of kind 'Constraint' should never use their name stream,
-- so make one with contrived names so we notice if we ever
-- accidentally use it.
unusedNameStream :: InfiniteList String
unusedNameStream = freshStrings "UNUSED_VARIABLE"

semigroupTypeclass :: TType -> TypeclassBody
semigroupTypeclass a = Typeclass.singleton (OperatorName "<>") (a `TArrow` (a `TArrow` a))

semigroupType :: forall e m. FromKindError e => MonadNames String m => MonadError e m =>
                 TaggedLambda TType m (WithContexts Relation)
semigroupType = lambdaCtx1 (TGround "Semigroup") \ta -> Context $ semigroupTypeclass ta

semigroupBuiltin :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Builtin m
semigroupBuiltin = Builtin {
                     relation: semigroupType,
                     nameStream: unusedNameStream
                   }

monoidTypeclass :: TType -> TypeclassBody
monoidTypeclass a = semigroupTypeclass a <> Typeclass.singleton (BasicName "mempty") a

monoidType :: forall e m. FromKindError e => MonadNames String m => MonadError e m =>
              TaggedLambda TType m (WithContexts Relation)
monoidType = lambdaCtx1 (TGround "Monoid") \ta -> Context $ monoidTypeclass ta

monoidBuiltin :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Builtin m
monoidBuiltin = Builtin {
                  relation: monoidType,
                  nameStream: unusedNameStream
                }

eqTypeclass :: TType -> TypeclassBody
eqTypeclass a = Typeclass.singleton (OperatorName "==") (a `TArrow` (a `TArrow` TGround "Boolean"))

eqType :: forall e m. FromKindError e => MonadNames String m => MonadError e m =>
          TaggedLambda TType m (WithContexts Relation)
eqType = lambdaCtx1 (TGround "Eq") \ta -> Context $ eqTypeclass ta

eqBuiltin :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Builtin m
eqBuiltin = Builtin {
              relation: eqType,
              nameStream: unusedNameStream
            }

-- Note: We don't use the Eq superclass for Ord. A lawful instance of
-- Ord (that is, an Ord instance which is a valid total ordering,
-- mathematically) can have its free theorem described fully in terms
-- of (<=). So we state the free theorem optimistically, in terms of
-- (<=) alone. We could have equivalently done so in terms of
-- `compare`, but I think (<=) makes more readable theorems.

ordTypeclass :: TType -> TypeclassBody
ordTypeclass a = Typeclass.singleton (OperatorName "<=") (a `TArrow` (a `TArrow` TGround "Boolean"))

ordType :: forall e m. FromKindError e => MonadNames String m => MonadError e m =>
           TaggedLambda TType m (WithContexts Relation)
ordType = lambdaCtx1 (TGround "Ord") \ta -> Context $ ordTypeclass ta

ordBuiltin :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Builtin m
ordBuiltin = Builtin {
               relation: ordType,
               nameStream: unusedNameStream
             }

namedBuiltinsMap :: forall e m. FromKindError e => MonadNames String m => MonadError e m => Map String (Builtin m)
namedBuiltinsMap = Map.fromFoldable [
                    Tuple "Int" $ basicBuiltin (TGround "Int") (freshStrings "n"),
                    Tuple "Float" $ basicBuiltin (TGround "Float") (freshStrings "f"),
                    Tuple "Double" $ basicBuiltin (TGround "Double") (freshStrings "d"),
                    Tuple "String" $ basicBuiltin (TGround "String") (freshStrings "s"),
                    Tuple "Boolean" $ basicBuiltin (TGround "Boolean") (freshStrings "b"),
                    Tuple "List" listBuiltin,
                    Tuple "Maybe" maybeBuiltin,
                    Tuple "Tuple0" $ basicBuiltin (TGround "Tuple0") (freshStrings "unit"),
                    Tuple "Void" $ basicBuiltin (TGround "Void") (freshStrings "void"),
                    Tuple "Tuple2" tuple2Builtin,
                    Tuple "Tuple3" tuple3Builtin,
                    Tuple "Tuple4" tuple4Builtin,
                    Tuple "Tuple5" tuple5Builtin,
                    Tuple "Either" eitherBuiltin,
                    Tuple "Semigroup" semigroupBuiltin,
                    Tuple "Monoid" monoidBuiltin,
                    Tuple "Eq" eqBuiltin,
                    Tuple "Ord" ordBuiltin
                   ]

allBuiltins :: forall e m. FromKindError e => MonadNames String m => MonadError e m => BuiltinsMap m
allBuiltins = BuiltinsMap (LookupMap.fromMap namedBuiltinsMap)
