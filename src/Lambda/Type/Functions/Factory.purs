
module Lambda.Type.Functions.Factory(
                                     lambda1
                                    ) where

-- Helpers for producing Lambdas of various function types.

import Lambda.Type.Functions (Lambda(..), expectGround)
import Lambda.Type.Error (class FromKindError)
import Lambda.Type.Kind (TKind(..))

import Prelude
import Control.Monad.Error.Class (class MonadError)

-- Helper for producing lambdas of kind (Type -> Type)
lambda1 :: forall e m r. FromKindError e => MonadError e m => (r -> r) -> Lambda m r
lambda1 f = Function { domain: Ty, codomain: Ty, body: \a -> (Ground <<< f) <$> expectGround a }
