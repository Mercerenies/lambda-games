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
module Lambda.Type.LambdaContext.FreeTheoremEnv(
                                                FreeTheoremEnv,
                                                fromBuiltinsMap,
                                                addBinding, withBinding, lookupBinding, lookupBuiltin,
                                                askVariableNamer,
                                                doBoundSubstitutionsLeft, doBoundSubstitutionsRight
                                               ) where

import Lambda.Type (TType)
import Lambda.Type (substitute) as Type
import Lambda.Type.BuiltinsMap (BuiltinsMap, Builtin)
import Lambda.Type.BuiltinsMap (lookup, variableNamer) as BuiltinsMap
import Lambda.Type.Relation (Relation)
import Lambda.Util.InfiniteList (InfiniteList)

import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe)
import Data.Map (Map)
import Data.Map (empty, insert, lookup, toUnfoldable) as Map
import Data.List (List)
import Data.Foldable (foldr)
import Control.Monad.Reader.Class (class MonadReader, class MonadAsk, local, asks)
import Prelude

newtype FreeTheoremEnv m = FreeTheoremEnv {
      builtinsMap :: BuiltinsMap m,
      quantifiedBindings :: Map String RelationBinding
    }

newtype RelationBinding = RelationBinding {
      relation :: Relation,
      leftSubstitution :: TType,
      rightSubstitution :: TType
    }

fromBuiltinsMap :: forall m. BuiltinsMap m -> FreeTheoremEnv m
fromBuiltinsMap builtinsMap = FreeTheoremEnv { builtinsMap, quantifiedBindings: Map.empty }

addBinding :: forall m. String -> Relation -> Tuple TType TType ->
              FreeTheoremEnv m -> FreeTheoremEnv m
addBinding x relation (Tuple leftSubstitution rightSubstitution) (FreeTheoremEnv env) =
    let newBinding = RelationBinding { relation, leftSubstitution, rightSubstitution } in
    FreeTheoremEnv $ env { quantifiedBindings = Map.insert x newBinding env.quantifiedBindings }

withBinding :: forall m n a. MonadReader (FreeTheoremEnv n) m =>
               String -> Relation -> Tuple TType TType ->
               m a -> m a
withBinding x r tup = local (addBinding x r tup)

lookupBinding :: forall m n. MonadAsk (FreeTheoremEnv n) m => String -> m (Maybe Relation)
lookupBinding v = asks \(FreeTheoremEnv env) -> getRelation <$> Map.lookup v env.quantifiedBindings
    where getRelation (RelationBinding { relation }) = relation

lookupBuiltin :: forall m n. MonadAsk (FreeTheoremEnv n) m => String -> m (Maybe (Builtin n))
lookupBuiltin v = asks \(FreeTheoremEnv env) -> BuiltinsMap.lookup v env.builtinsMap

askVariableNamer :: forall m n. MonadAsk (FreeTheoremEnv n) m => m (String -> InfiniteList String)
askVariableNamer = asks \(FreeTheoremEnv env) -> BuiltinsMap.variableNamer env.builtinsMap

mapToList :: forall k v. Ord k => Map k v -> List (Tuple k v)
mapToList = Map.toUnfoldable

-- Do a Type.substitute on the type, for each binding in the bindings map.
doBoundSubstitutions :: forall m n. MonadAsk (FreeTheoremEnv n) m => (RelationBinding -> TType) -> TType -> m TType
doBoundSubstitutions f t = ado
  bindings <- asks \(FreeTheoremEnv env) -> env.quantifiedBindings
  in mapToList bindings # foldr (\(Tuple x binding) t' -> Type.substitute x (f binding) t') t

-- Do a Type.substitute on the type, for each binding in the bindings map, using leftVariable.
doBoundSubstitutionsLeft :: forall m n. MonadAsk (FreeTheoremEnv n) m => TType -> m TType
doBoundSubstitutionsLeft = doBoundSubstitutions onLeft
    where onLeft (RelationBinding binding) = binding.leftSubstitution

-- Do a Type.substitute on the type, for each binding in the bindings map, using rightVariable.
doBoundSubstitutionsRight :: forall m n. MonadAsk (FreeTheoremEnv n) m => TType -> m TType
doBoundSubstitutionsRight = doBoundSubstitutions onRight
    where onRight (RelationBinding binding) = binding.rightSubstitution
