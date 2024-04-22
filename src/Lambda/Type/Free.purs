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
                        LambdaContextT(..), unLambdaContextT, runLambdaContextT,
                        relationify, relationifyWithBindings,
                        FreeTheoremOptions(..), describeFreeTheoremGeneral,
                        describeFreeTheorem, describeFreeTheoremLatex
                       ) where

import Lambda.Type (TType(..), suggestedVariableName, functionNames)
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

import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..), (:))
import Data.Foldable (lookup) as Fold
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Control.Monad.Error.Class (class MonadThrow, class MonadError, throwError)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, asks)
import Prelude

-- The type that Lambda functions (at the type level) must run in (for
-- underlying monad m).
newtype LambdaContextT m a =
    LambdaContextT (ReaderT (BuiltinsMap (LambdaContextT m)) (NamesT String m) a)

derive instance Newtype (LambdaContextT m a) _
derive newtype instance Functor m => Functor (LambdaContextT m)
derive newtype instance Apply m => Apply (LambdaContextT m)
derive newtype instance Applicative m => Applicative (LambdaContextT m)
derive newtype instance Bind m => Bind (LambdaContextT m)
derive newtype instance Monad m => Monad (LambdaContextT m)
derive newtype instance MonadThrow e m => MonadThrow e (LambdaContextT m)
derive newtype instance MonadError e m => MonadError e (LambdaContextT m)
derive newtype instance Monad m => MonadAsk (BuiltinsMap (LambdaContextT m)) (LambdaContextT m)
derive newtype instance Monad m => MonadReader (BuiltinsMap (LambdaContextT m)) (LambdaContextT m)
derive newtype instance Monad m => MonadNames String (LambdaContextT m)

unLambdaContextT :: forall m a. LambdaContextT m a -> ReaderT (BuiltinsMap (LambdaContextT m)) (NamesT String m) a
unLambdaContextT (LambdaContextT x) = x

runLambdaContextT :: forall m a. Monad m =>
                     LambdaContextT m a -> BuiltinsMap (LambdaContextT m) -> List String -> m a
runLambdaContextT (LambdaContextT x) r reservedNames = runNamesTWith reservedNames (runReaderT x r)

-- Lift a closed type into a relation.
relationify :: forall m. MonadError TypeError m => TType -> LambdaContextT m (Lambda (LambdaContextT m) Relation)
relationify = relationifyWithBindings Nil

appSection :: Term -> Term
appSection x = OperatorSectionLeft "$" x

relationifyWithBindings :: forall m. MonadError TypeError m =>
                           List (Tuple String Relation) -> TType -> LambdaContextT m (Lambda (LambdaContextT m) Relation)
relationifyWithBindings bindings (TVar x)
    | Just rel <- Fold.lookup x bindings = pure (Ground rel)
    | otherwise = throwError $ UnboundVariable x
relationifyWithBindings _ (TGround x) = do
    lam <- asks $ BuiltinsMap.lookup x
    case lam of
      Nothing -> throwError $ UnboundGroundTerm x
      Just (Builtin { relation }) -> pure relation
relationifyWithBindings bindings (TApp ff aa) = do
  f <- relationifyWithBindings bindings ff
  a <- relationifyWithBindings bindings aa
  case f of
    Ground _ -> throwError $ ExpectedTypeFunction ff
    Function { domain, codomain: _, body: f' } -> do
      assertKind domain (getKind a)
      f' a
relationifyWithBindings bindings (TArrow a b) = do
  namer <- asks variableNamer
  withFreshName2 (suggestedVariableName namer a) $ \a1 a2 -> do
    ra <- relationifyWithBindings bindings a >>= expectGround
    rb <- relationifyWithBindings bindings b >>= expectGround
    pure $ Ground $ rForall a1 a $ rForall a2 a $
                      runRelation ra (Var a1) (Var a2) `rImplies`
                        mapTerms (App (appSection (Var a1))) (App (appSection (Var a2))) rb
--relationifyWithBindings _ (TContextArrow _ _) =
--    unsafeThrow "Not supported yet"
relationifyWithBindings bindings (TForall x body) = do
  withFreshName2 (freshStrings x) \x1 x2 -> do
    withFreshName functionNames \f -> do
      let rel = mapTerms (App (Var f)) identity identityRelation
      innerRelation <- relationifyWithBindings (Tuple x rel : bindings) body >>= expectGround
      pure $ Ground $ rForall x1 (TVar "Type") $ rForall x2 (TVar "Type") $
                        rForall f (TVar x1 `TArrow` TVar x2) $ innerRelation

newtype FreeTheoremOptions a = FreeTheoremOptions {
      simplifier :: Predicate -> Predicate,
      builtinsMap :: BuiltinsMap (LambdaContextT (Either TypeError)),
      reservedNames :: List String,
      finalizer :: String -> TType -> Predicate -> a
    }

describeFreeTheoremGeneral :: forall a. FreeTheoremOptions a -> TType -> Either TypeError a
describeFreeTheoremGeneral (FreeTheoremOptions opts) t =
    runLambdaContextT fullDescription opts.builtinsMap opts.reservedNames
  where fullDescription :: LambdaContextT (Either TypeError) a
        fullDescription = do
          namer <- asks variableNamer
          withFreshName (suggestedVariableName namer t) $ \a -> do
            r <- relationify t >>= expectGround
            let description = opts.simplifier (runRelation r (Var a) (Var a))
            pure $ opts.finalizer a t description

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
