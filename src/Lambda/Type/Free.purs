
module Lambda.Type.Free(
                        LambdaContextT(..), unLambdaContextT, runLambdaContextT,
                        relationify, relationifyWithBindings,
                        describeFreeTheorem, describeFreeTheoremWith
                       ) where

import Lambda.Type (TType(..), suggestedVariableName)
import Lambda.Type.Relation (Relation, identityRelation, rForall, rImplies, runRelation, mapTerms)
import Lambda.Type.Error (TypeError(..))
import Lambda.Type.Functions (Lambda(..), expectGround, assertKind, getKind)
import Lambda.Term (Term(..))
import Lambda.Predicate (Predicate)
import Lambda.Monad.Names (NamesT, withName, withFreshName, freshStrings, withName2, runNamesT, class MonadNames)
import Lambda.Util.InfiniteList (InfiniteList, intersperse)
import Lambda.PrettyShow (prettyShow)
import Lambda.LookupMap (LookupMap)
import Lambda.LookupMap (lookup) as LookupMap

import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..), (:))
import Data.NonEmpty ((:|))
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
    LambdaContextT (ReaderT (LookupMap String (Lambda (LambdaContextT m) Relation)) (NamesT String m) a)

derive instance Newtype (LambdaContextT m a) _
derive newtype instance Functor m => Functor (LambdaContextT m)
derive newtype instance Apply m => Apply (LambdaContextT m)
derive newtype instance Applicative m => Applicative (LambdaContextT m)
derive newtype instance Bind m => Bind (LambdaContextT m)
derive newtype instance Monad m => Monad (LambdaContextT m)
derive newtype instance MonadThrow e m => MonadThrow e (LambdaContextT m)
derive newtype instance MonadError e m => MonadError e (LambdaContextT m)
derive newtype instance Monad m => MonadAsk (LookupMap String (Lambda (LambdaContextT m) Relation)) (LambdaContextT m)
derive newtype instance Monad m => MonadReader (LookupMap String (Lambda (LambdaContextT m) Relation)) (LambdaContextT m)
derive newtype instance Monad m => MonadNames String (LambdaContextT m)

unLambdaContextT :: forall m a. LambdaContextT m a -> ReaderT (LookupMap String (Lambda (LambdaContextT m) Relation)) (NamesT String m) a
unLambdaContextT (LambdaContextT x) = x

runLambdaContextT :: forall m a. Monad m =>
                     LambdaContextT m a -> LookupMap String (Lambda (LambdaContextT m) Relation) -> m a
runLambdaContextT (LambdaContextT x) r = runNamesT (runReaderT x r)

functionNames :: InfiniteList String
functionNames = intersperse (freshStrings "f" :| freshStrings "g" : freshStrings "h" : Nil)

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
    lam <- asks $ LookupMap.lookup x
    case lam of
      Nothing -> throwError $ UnboundGroundTerm x
      Just expr -> pure expr
relationifyWithBindings bindings (TApp ff aa) = do
  f <- relationifyWithBindings bindings ff
  a <- relationifyWithBindings bindings aa
  case f of
    Ground _ -> throwError $ ExpectedTypeFunction ff
    Function { domain, codomain: _, body: f' } -> do
      assertKind domain (getKind a)
      f' a
relationifyWithBindings bindings (TArrow a b) = do
  withName2 (suggestedVariableName a) $ \a1 a2 -> do
    ra <- relationifyWithBindings bindings a >>= expectGround
    rb <- relationifyWithBindings bindings b >>= expectGround
    pure $ Ground $ rForall a1 a $ rForall a2 a $
                      runRelation ra (Var a1) (Var a2) `rImplies`
                        mapTerms (App (appSection (Var a1))) (App (appSection (Var a2))) rb
--relationifyWithBindings _ (TContextArrow _ _) =
--    unsafeThrow "Not supported yet"
relationifyWithBindings bindings (TForall x body) = do
  withName2 x \x1 x2 -> do
    withFreshName functionNames \f -> do
      let rel = mapTerms (App (Var f)) identity identityRelation
      innerRelation <- relationifyWithBindings (Tuple x rel : bindings) body >>= expectGround
      pure $ Ground $ rForall x1 (TVar "Type") $ rForall x2 (TVar "Type") $
                        rForall f (TVar x1 `TArrow` TVar x2) $
                          mapTerms (\left -> TypeApp left (Var x1)) (\right -> TypeApp right (Var x2)) innerRelation

describeFreeTheoremWith :: (Predicate -> Predicate) ->
                           LookupMap String (Lambda (LambdaContextT (Either TypeError)) Relation) ->
                           TType -> Either TypeError String
describeFreeTheoremWith simplifier builtins t = runLambdaContextT fullDescription builtins
  where fullDescription :: LambdaContextT (Either TypeError) String
        fullDescription = withName (suggestedVariableName t) $ \a -> do
         r <- relationify t >>= expectGround
         let description = prettyShow $ simplifier (runRelation r (Var a) (Var a))
         pure $ a <> " ~ " <> a <> " if " <> description

describeFreeTheorem :: LookupMap String (Lambda (LambdaContextT (Either TypeError)) Relation) ->
                       TType -> Either TypeError String
describeFreeTheorem = describeFreeTheoremWith identity
