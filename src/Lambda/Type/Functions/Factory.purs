
module Lambda.Type.Functions.Factory(
                                     lambda1
                                    ) where

-- Helpers for producing Lambdas of various function types.

import Lambda.Type.Functions (Lambda(..), LambdaFunction, expectGround)
import Lambda.Type.Error (class FromKindError)
import Lambda.Type.Kind (TKind(..))

import Prelude
import Control.Monad.Error.Class (class MonadError)

-- Helper for producing lambdas of kind (Type -> Type)
lambda1 :: forall e m r. FromKindError e => MonadError e m => (r -> m r) -> Lambda m r
lambda1 f = Function { domain: Ty, codomain: Ty, body }
    where body :: LambdaFunction m r
          body a = do
            a' <- expectGround a
            gr <- f a'
            pure $ Ground gr
