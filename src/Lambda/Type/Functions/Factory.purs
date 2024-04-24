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
                                     lambda0, lambda1, lambda2, lambda3, lambda4, lambda5
                                    ) where

-- Helpers for producing Lambdas of various function types.

import Lambda.Type.Functions (Lambda, LambdaF(..), class GroundKindInferrable)
import Lambda.Type.Error (class FromKindError)
import Lambda.Type.Kind (TKind(..), GroundKind(..))
import Lambda.Type.Typeclass (WithContexts, expectGroundTy)
import Lambda.Recursion (Mu(..))

import Prelude
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Control.Monad.Error.Class (class MonadError)

-- Helper for producing lambdas of kind k
lambda0 :: forall e m r. FromKindError e => MonadError e m => GroundKindInferrable r =>
           WithContexts r -> Lambda m (WithContexts r)
lambda0 f = monoLambda withContextsLambdaArgs $ mono f

-- Helper for producing lambdas of kind (Type -> k)
lambda1 :: forall e m r. FromKindError e => MonadError e m => GroundKindInferrable r =>
           (r -> WithContexts r) -> Lambda m (WithContexts r)
lambda1 f = monoLambda withContextsLambdaArgs \a -> mono (f a)

-- Helper for producing lambdas of kind (Type -> Type -> k)
lambda2 :: forall e m r. FromKindError e => MonadError e m => GroundKindInferrable r =>
           (r -> r -> WithContexts r) -> Lambda m (WithContexts r)
lambda2 f = monoLambda withContextsLambdaArgs \a b -> mono (f a b)

-- Helper for producing lambdas of kind (Type -> Type -> Type -> k)
lambda3 :: forall e m r. FromKindError e => MonadError e m => GroundKindInferrable r =>
           (r -> r -> r -> WithContexts r) -> Lambda m (WithContexts r)
lambda3 f = monoLambda withContextsLambdaArgs \a b c -> mono (f a b c)

-- Helper for producing lambdas of kind (Type -> Type -> Type -> Type -> k)
lambda4 :: forall e m r. FromKindError e => MonadError e m => GroundKindInferrable r =>
           (r -> r -> r -> r -> WithContexts r) -> Lambda m (WithContexts r)
lambda4 f = monoLambda withContextsLambdaArgs \a b c d -> mono (f a b c d)

-- Helper for producing lambdas of kind (Type -> Type -> Type -> Type -> Type -> k)
lambda5 :: forall e m r. FromKindError e => MonadError e m => GroundKindInferrable r =>
           (r -> r -> r -> r -> r -> WithContexts r) -> Lambda m (WithContexts r)
lambda5 f = monoLambda withContextsLambdaArgs \a b c d e -> mono (f a b c d e)

-- Helper for producing lambdas which take arbitrary numbers of
-- arguments, all of which are of kind Type. e.g. (Type -> Type),
-- (Type -> Type -> Type), etc.

newtype Mono r = Mono r

runMono :: forall r. Mono r -> r
runMono = coerce

mono :: forall r. r -> Mono r
mono = coerce

type LambdaArgs m r s = {
      extractor :: Lambda m r -> m s
    }

withContextsLambdaArgs :: forall e m r. FromKindError e => MonadError e m => GroundKindInferrable r =>
                          LambdaArgs m (WithContexts r) r
withContextsLambdaArgs = {
    extractor: expectGroundTy
  }

class MonoLambda x r s | x -> r s where
    monoLambda :: forall e m. FromKindError e => MonadError e m => LambdaArgs m r s -> x -> Lambda m r
    monoKind :: Proxy x -> TKind

instance MonoLambda (Mono r) r s where
    monoLambda _ = Ground <<< runMono
    monoKind _ = Ty GType

instance (GroundKindInferrable r, MonoLambda x r s) => MonoLambda (s -> x) r s where
    monoLambda args f =
        Function {
          domain: Ty GType,
          codomain: monoKind (Proxy :: Proxy x),
          body: \(Mu a) -> (Mu <<< monoLambda args <<< f) <$> args.extractor a
        }
    monoKind _ = Ty GType `KArrow` monoKind (Proxy :: Proxy x)
