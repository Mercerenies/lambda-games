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
                        relationify,
                        FreeTheoremOptions(..), describeFreeTheoremGeneral,
                        describeFreeTheorem, describeFreeTheoremLatex
                       ) where

import Lambda.Type (TType(..), suggestedVariableName, functionNames)
import Lambda.Type.Typeclass (WithContexts(..), TypeclassBody, TypeclassFunction(..),
                              expectGroundTy', expectGroundConstraint')
import Lambda.Type.Typeclass (toArray) as Typeclass
import Lambda.Type.Relation (Relation, identityRelation, rForall, rImplies, runRelation, mapTerms)
import Lambda.Type.Error (TypeError(..))
import Lambda.Type.Functions (LambdaF(..), TaggedLambdaF(..), TaggedLambda, assertKind,
                              getKind, getLambdaFromTagged)
import Lambda.Type.BuiltinsMap (BuiltinsMap, Builtin(..))
import Lambda.Type.LambdaContext.FreeTheoremEnv (FreeTheoremEnv, withBinding, lookupBinding, lookupBuiltin,
                                                 askVariableNamer, doBoundSubstitutionsLeft,
                                                 doBoundSubstitutionsRight)
import Lambda.Type.LambdaContext.FreeTheoremEnv (fromBuiltinsMap) as FreeTheoremEnv
import Lambda.Type.LambdaContext (LambdaContextT, runLambdaContextT)
import Lambda.Term (Term(..))
import Lambda.Predicate (Predicate)
import Lambda.Monad.Names (withFreshName, withFreshName2, freshStrings)
import Lambda.PrettyShow (prettyShow)
import Lambda.MathShow (mathShow, Latex(..), texttt)
import Lambda.Recursion (Mu(..), unMu)

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.List (List)
import Data.Either (Either)
import Data.Foldable (foldr)
import Data.Traversable (traverse)
import Control.Monad.Error.Class (class MonadError, throwError)
import Prelude

appSection :: Term -> Term
appSection x = OperatorSectionLeft "$" x

ground :: forall m. TType -> Relation -> TaggedLambda TType m (WithContexts Relation)
ground t = TaggedLambdaF t <<< Ground <<< NonContext

-- Lift a closed type into a relation.
relationify :: forall m. MonadError TypeError m =>
               TType -> LambdaContextT m (TaggedLambda TType (LambdaContextT m) (WithContexts Relation))
relationify (TVar x) = do
  rel <- lookupBinding x
  case rel of
    Just relation -> pure (ground (TVar x) relation)
    Nothing -> throwError $ UnboundVariable x
relationify (TGround x) = do
    lam <- lookupBuiltin x
    case lam of
      Nothing -> throwError $ UnboundGroundTerm x
      Just (Builtin { relation }) -> pure relation
relationify (TApp ff aa) = do
  f <- relationify ff
  a <- relationify aa
  case getLambdaFromTagged f of
    Ground _ -> throwError $ ExpectedTypeFunction ff
    Function { domain, codomain: _, body: f' } -> do
      assertKind domain (getKind $ getLambdaFromTagged a)
      unMu <$> f' (Mu a)
relationify (TArrow a b) = do
  namer <- askVariableNamer
  withFreshName2 (suggestedVariableName namer a) $ \a1 a2 -> do
    ra <- relationify a >>= expectGroundTy'
    rb <- relationify b >>= expectGroundTy'
    aLeft <- doBoundSubstitutionsLeft a
    aRight <- doBoundSubstitutionsRight a
    pure $ ground (TArrow a b) $ rForall a1 aLeft $ rForall a2 aRight $
                                   runRelation ra (Var a1) (Var a2) `rImplies`
                                     mapTerms (App (appSection (Var a1))) (App (appSection (Var a2))) rb
relationify (TContextArrow ctx b) = do
  rctx <- relationify ctx >>= expectGroundConstraint' >>= computeTypeclassAssumptions
  rb <- relationify b >>= expectGroundTy'
  pure $ ground (TContextArrow ctx b) $ foldr rImplies rb rctx
relationify (TForall x body) = do
  withFreshName2 (freshStrings x) \x1 x2 -> do
    withFreshName functionNames \f -> do
      let rel = mapTerms (App (Var f)) identity identityRelation
      innerRelation <- (withBinding x rel (Tuple x1 x2) $ relationify body) >>= expectGroundTy'
      pure $ ground (TForall x body) $ rForall x1 (TVar "Type") $ rForall x2 (TVar "Type") $
                                         rForall f (TVar x1 `TArrow` TVar x2) $ innerRelation

computeTypeclassAssumptions :: forall m. MonadError TypeError m =>
                               TypeclassBody -> LambdaContextT m (Array Predicate)
computeTypeclassAssumptions functions = traverse computeAssumption (Typeclass.toArray functions)
    where computeAssumption :: TypeclassFunction -> LambdaContextT m Predicate
          computeAssumption (TypeclassFunction { methodName, methodType }) = ado
            methodRelation <- relationify methodType >>= expectGroundTy'
            in runRelation methodRelation (Var methodName) (Var methodName)

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
          namer <- askVariableNamer
          withFreshName (suggestedVariableName namer t) $ \a -> do
            r <- relationify t >>= expectGroundTy'
            let description = opts.simplifier (runRelation r (Var a) (Var a))
            pure $ opts.finalizer a t description
        readerContext :: FreeTheoremEnv (LambdaContextT (Either TypeError))
        readerContext = FreeTheoremEnv.fromBuiltinsMap opts.builtinsMap

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
