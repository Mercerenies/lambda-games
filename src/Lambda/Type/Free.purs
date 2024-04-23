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
module Lambda.Type.Free(
                        LambdaContextT(..), ReaderContext(..), RelationBinding(..),
                        unLambdaContextT, runLambdaContextT,
                        relationify,
                        FreeTheoremOptions(..), describeFreeTheoremGeneral,
                        describeFreeTheorem, describeFreeTheoremLatex
                       ) where

import Lambda.Type (TType(..), suggestedVariableName, functionNames)
import Lambda.Type (substitute) as Type
import Lambda.Type.Relation (Relation, identityRelation, rForall, rImplies, runRelation, mapTerms)
import Lambda.Type.Error (TypeError(..))
import Lambda.Type.Functions (Lambda(..), expectGround, assertKind, getKind)
import Lambda.Type.BuiltinsMap (BuiltinsMap, Builtin(..), variableNamer)
import Lambda.Type.BuiltinsMap (lookup) as BuiltinsMap
import Lambda.Term (Term(..))
import Lambda.Predicate (Predicate)
import Lambda.Monad.Names (NamesT, withFreshName, withFreshName2, freshStrings, runNamesTWith, class MonadNames)
import Lambda.PrettyShow (prettyShow)
import Lambda.MathShow (mathShow, Latex(..), texttt)

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.List (List)
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map (lookup, insert, empty, toUnfoldable) as Map
import Control.Monad.Error.Class (class MonadThrow, class MonadError, throwError)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, asks, local)
import Prelude

-- The type that Lambda functions (at the type level) must run in (for
-- underlying monad m).
newtype LambdaContextT m a =
    LambdaContextT (ReaderT (ReaderContext m) (NamesT String m) a)

newtype ReaderContext m = ReaderContext {
      builtinsMap :: BuiltinsMap (LambdaContextT m),
      quantifiedBindings :: Map String RelationBinding
    }

newtype RelationBinding = RelationBinding {
      relation :: Relation,
      leftVariable :: String,
      rightVariable :: String
    }

derive instance Newtype (LambdaContextT m a) _
derive newtype instance Functor m => Functor (LambdaContextT m)
derive newtype instance Apply m => Apply (LambdaContextT m)
derive newtype instance Applicative m => Applicative (LambdaContextT m)
derive newtype instance Bind m => Bind (LambdaContextT m)
derive newtype instance Monad m => Monad (LambdaContextT m)
derive newtype instance MonadThrow e m => MonadThrow e (LambdaContextT m)
derive newtype instance MonadError e m => MonadError e (LambdaContextT m)
derive newtype instance Monad m => MonadAsk (ReaderContext m) (LambdaContextT m)
derive newtype instance Monad m => MonadReader (ReaderContext m) (LambdaContextT m)
derive newtype instance Monad m => MonadNames String (LambdaContextT m)

unLambdaContextT :: forall m a. LambdaContextT m a -> ReaderT (ReaderContext m) (NamesT String m) a
unLambdaContextT (LambdaContextT x) = x

runLambdaContextT :: forall m a. Monad m =>
                     LambdaContextT m a -> ReaderContext m -> List String -> m a
runLambdaContextT (LambdaContextT x) r reservedNames = runNamesTWith reservedNames (runReaderT x r)

appSection :: Term -> Term
appSection x = OperatorSectionLeft "$" x

addBinding :: forall m. String -> RelationBinding -> ReaderContext m -> ReaderContext m
addBinding x t (ReaderContext ctx) =
    ReaderContext $ ctx { quantifiedBindings = Map.insert x t ctx.quantifiedBindings }

-- Lift a closed type into a relation.
relationify :: forall m. MonadError TypeError m =>
               TType -> LambdaContextT m (Lambda (LambdaContextT m) Relation)
relationify (TVar x) = do
  bindings <- asks \(ReaderContext r) -> r.quantifiedBindings
  case Map.lookup x bindings of
    Just (RelationBinding relBinding) -> pure (Ground relBinding.relation)
    Nothing -> throwError $ UnboundVariable x
relationify (TGround x) = do
    lam <- asks $ \(ReaderContext r) -> BuiltinsMap.lookup x r.builtinsMap
    case lam of
      Nothing -> throwError $ UnboundGroundTerm x
      Just (Builtin { relation }) -> pure relation
relationify (TApp ff aa) = do
  f <- relationify ff
  a <- relationify aa
  case f of
    Ground _ -> throwError $ ExpectedTypeFunction ff
    Function { domain, codomain: _, body: f' } -> do
      assertKind domain (getKind a)
      f' a
relationify (TArrow a b) = do
  namer <- asks (\(ReaderContext r) -> variableNamer r.builtinsMap)
  withFreshName2 (suggestedVariableName namer a) $ \a1 a2 -> do
    ra <- relationify a >>= expectGround
    rb <- relationify b >>= expectGround
    aLeft <- doBoundSubstitutionsLeft a
    aRight <- doBoundSubstitutionsRight a
    pure $ Ground $ rForall a1 aLeft $ rForall a2 aRight $
                      runRelation ra (Var a1) (Var a2) `rImplies`
                        mapTerms (App (appSection (Var a1))) (App (appSection (Var a2))) rb
--relationify (TContextArrow _ _) =
--    unsafeThrow "Not supported yet"
relationify (TForall x body) = do
  withFreshName2 (freshStrings x) \x1 x2 -> do
    withFreshName functionNames \f -> do
      let rel = mapTerms (App (Var f)) identity identityRelation
          relBinding = RelationBinding {
                         relation: rel,
                         leftVariable: x1,
                         rightVariable: x2
                       }
      innerRelation <- (local (addBinding x relBinding) $ relationify body) >>= expectGround
      pure $ Ground $ rForall x1 (TVar "Type") $ rForall x2 (TVar "Type") $
                        rForall f (TVar x1 `TArrow` TVar x2) $ innerRelation

mapToList :: forall k v. Ord k => Map k v -> List (Tuple k v)
mapToList = Map.toUnfoldable

doBoundSubstitutionsLeft :: forall m n. MonadAsk (ReaderContext n) m => TType -> m TType
doBoundSubstitutionsLeft t = ado
  bindings <- asks (\(ReaderContext r) -> r.quantifiedBindings)
  in mapToList bindings #
       foldr (\(Tuple x (RelationBinding { leftVariable })) t' -> Type.substitute x (TVar leftVariable) t') t

doBoundSubstitutionsRight :: forall m n. MonadAsk (ReaderContext n) m => TType -> m TType
doBoundSubstitutionsRight t = ado
  bindings <- asks (\(ReaderContext r) -> r.quantifiedBindings)
  in mapToList bindings #
       foldr (\(Tuple x (RelationBinding { rightVariable })) t' -> Type.substitute x (TVar rightVariable) t') t

newtype FreeTheoremOptions a = FreeTheoremOptions {
      simplifier :: Predicate -> Predicate,
      builtinsMap :: BuiltinsMap (LambdaContextT (Either TypeError)),
      reservedNames :: List String,
      finalizer :: String -> TType -> Predicate -> a
    }

describeFreeTheoremGeneral :: forall a. FreeTheoremOptions a -> TType -> Either TypeError a
describeFreeTheoremGeneral (FreeTheoremOptions opts) t =
    runLambdaContextT fullDescription readerContext opts.reservedNames
  where fullDescription :: LambdaContextT (Either TypeError) a
        fullDescription = do
          namer <- asks (\(ReaderContext r) -> variableNamer r.builtinsMap)
          withFreshName (suggestedVariableName namer t) $ \a -> do
            r <- relationify t >>= expectGround
            let description = opts.simplifier (runRelation r (Var a) (Var a))
            pure $ opts.finalizer a t description
        readerContext :: ReaderContext (Either TypeError)
        readerContext = ReaderContext {
                          builtinsMap: opts.builtinsMap,
                          quantifiedBindings: Map.empty
                        }

describeFreeTheorem :: (Predicate -> Predicate) ->
                       BuiltinsMap (LambdaContextT (Either TypeError)) ->
                       List String -> TType -> Either TypeError String
describeFreeTheorem simplifier builtinsMap reservedNames =
    describeFreeTheoremGeneral (FreeTheoremOptions {
                                  simplifier, builtinsMap,
                                  reservedNames, finalizer
                                })
  where finalizer a t description =
            "If " <> a <> ": " <> prettyShow t <> " then " <> prettyShow description

describeFreeTheoremLatex :: (Predicate -> Predicate) ->
                            BuiltinsMap (LambdaContextT (Either TypeError)) ->
                            List String -> TType -> Either TypeError Latex
describeFreeTheoremLatex simplifier builtinsMap reservedNames =
    describeFreeTheoremGeneral (FreeTheoremOptions {
                                  simplifier, builtinsMap,
                                  reservedNames, finalizer
                                })
  where finalizer a t description =
            texttt a <> Latex ": " <> texttt (prettyShow t) <> Latex " \\implies " <> mathShow description
