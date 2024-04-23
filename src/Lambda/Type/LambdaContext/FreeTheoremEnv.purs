
module Lambda.Type.LambdaContext.FreeTheoremEnv(
                                                FreeTheoremEnv,
                                                fromBuiltinsMap,
                                                addBinding, withBinding, lookupBinding, lookupBuiltin,
                                                askVariableNamer,
                                                doBoundSubstitutionsLeft, doBoundSubstitutionsRight
                                               ) where

import Lambda.Type (TType(..))
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
      leftVariable :: String,
      rightVariable :: String
    }

fromBuiltinsMap :: forall m. BuiltinsMap m -> FreeTheoremEnv m
fromBuiltinsMap builtinsMap = FreeTheoremEnv { builtinsMap, quantifiedBindings: Map.empty }

addBinding :: forall m. String -> Relation -> Tuple String String ->
              FreeTheoremEnv m -> FreeTheoremEnv m
addBinding x relation (Tuple leftVariable rightVariable) (FreeTheoremEnv env) =
    let newBinding = RelationBinding { relation, leftVariable, rightVariable } in
    FreeTheoremEnv $ env { quantifiedBindings = Map.insert x newBinding env.quantifiedBindings }

withBinding :: forall m n a. MonadReader (FreeTheoremEnv n) m =>
               String -> Relation -> Tuple String String ->
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
doBoundSubstitutions :: forall m n. MonadAsk (FreeTheoremEnv n) m => (RelationBinding -> String) -> TType -> m TType
doBoundSubstitutions f t = ado
  bindings <- asks \(FreeTheoremEnv env) -> env.quantifiedBindings
  in mapToList bindings # foldr (\(Tuple x binding) t' -> Type.substitute x (TVar $ f binding) t') t

-- Do a Type.substitute on the type, for each binding in the bindings map, using leftVariable.
doBoundSubstitutionsLeft :: forall m n. MonadAsk (FreeTheoremEnv n) m => TType -> m TType
doBoundSubstitutionsLeft = doBoundSubstitutions onLeft
    where onLeft (RelationBinding binding) = binding.leftVariable

-- Do a Type.substitute on the type, for each binding in the bindings map, using rightVariable.
doBoundSubstitutionsRight :: forall m n. MonadAsk (FreeTheoremEnv n) m => TType -> m TType
doBoundSubstitutionsRight = doBoundSubstitutions onRight
    where onRight (RelationBinding binding) = binding.rightVariable
