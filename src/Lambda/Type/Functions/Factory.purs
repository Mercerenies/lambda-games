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
module Lambda.Type.Functions.Factory(
                                     lambda0, lambda1, lambda2, lambda3, lambda4, lambda5,
                                     lambda1Ctx
                                    ) where

-- Helpers for producing Lambdas of various function types.

import Lambda.Type.Functions (Lambda(..), class GroundKindInferrable)
import Lambda.Type.Error (class FromKindError)
import Lambda.Type.Kind (TKind(..), GroundKind(..))
import Lambda.Type.Typeclass (WithContexts, expectGroundTy)

import Prelude
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Control.Monad.Error.Class (class MonadError)

-- Helper for producing lambdas of kind Type
lambda0 :: forall e m r. FromKindError e => MonadError e m => GroundKindInferrable r =>
           WithContexts m r -> Lambda m (WithContexts m r)
lambda0 f = monoLambda (withContextsLambdaArgs GType) $ mono f

-- Helper for producing lambdas of kind (Type -> Type)
lambda1 :: forall e m r. FromKindError e => MonadError e m => GroundKindInferrable r =>
           (r -> WithContexts m r) -> Lambda m (WithContexts m r)
lambda1 f = monoLambda (withContextsLambdaArgs GType) \a -> mono (f a)

-- Helper for producing lambdas of kind (Type -> Type -> Type)
lambda2 :: forall e m r. FromKindError e => MonadError e m => GroundKindInferrable r =>
           (r -> r -> WithContexts m r) -> Lambda m (WithContexts m r)
lambda2 f = monoLambda (withContextsLambdaArgs GType) \a b -> mono (f a b)

-- Helper for producing lambdas of kind (Type -> Type -> Type -> Type)
lambda3 :: forall e m r. FromKindError e => MonadError e m => GroundKindInferrable r =>
           (r -> r -> r -> WithContexts m r) -> Lambda m (WithContexts m r)
lambda3 f = monoLambda (withContextsLambdaArgs GType) \a b c -> mono (f a b c)

-- Helper for producing lambdas of kind (Type -> Type -> Type -> Type -> Type)
lambda4 :: forall e m r. FromKindError e => MonadError e m => GroundKindInferrable r =>
           (r -> r -> r -> r -> WithContexts m r) -> Lambda m (WithContexts m r)
lambda4 f = monoLambda (withContextsLambdaArgs GType) \a b c d -> mono (f a b c d)

-- Helper for producing lambdas of kind (Type -> Type -> Type -> Type -> Type -> Type)
lambda5 :: forall e m r. FromKindError e => MonadError e m => GroundKindInferrable r =>
           (r -> r -> r -> r -> r -> WithContexts m r) -> Lambda m (WithContexts m r)
lambda5 f = monoLambda (withContextsLambdaArgs GType) \a b c d e -> mono (f a b c d e)

-- Helper for producing lambdas of kind (Type -> Constraint)
lambda1Ctx :: forall e m r. FromKindError e => MonadError e m => GroundKindInferrable r =>
           (r -> WithContexts m r) -> Lambda m (WithContexts m r)
lambda1Ctx f = monoLambda (withContextsLambdaArgs GConstraint) \a -> mono (f a)

-- Helper for producing lambdas which take arbitrary numbers of
-- arguments, all of which are of kind Type. e.g. (Type -> Type),
-- (Type -> Type -> Type), etc.

newtype Mono r = Mono r

runMono :: forall r. Mono r -> r
runMono = coerce

mono :: forall r. r -> Mono r
mono = coerce

type LambdaArgs m r s = {
      extractor :: Lambda m r -> m s,
      groundKind :: GroundKind
    }

withContextsLambdaArgs :: forall e m r. FromKindError e => MonadError e m => GroundKindInferrable r =>
                          GroundKind -> LambdaArgs m (WithContexts m r) r
withContextsLambdaArgs groundKind = {
    extractor: expectGroundTy,
    groundKind
  }

class MonoLambda x r s | x -> r s where
    monoLambda :: forall e m. FromKindError e => MonadError e m => LambdaArgs m r s -> x -> Lambda m r
    monoKind :: forall m. Proxy x -> LambdaArgs m r s -> TKind

instance MonoLambda (Mono r) r s where
    monoLambda _ = Ground <<< runMono
    monoKind _ args = Ty args.groundKind

instance (GroundKindInferrable r, MonoLambda x r s) => MonoLambda (s -> x) r s where
    monoLambda args f =
        Function {
          domain: Ty GType,
          codomain: monoKind (Proxy :: Proxy x) args,
          body: \a -> (monoLambda args <<< f) <$> args.extractor a
        }
    monoKind _ args = Ty GType `KArrow` monoKind (Proxy :: Proxy x) args
