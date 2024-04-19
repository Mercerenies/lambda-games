
module Lambda.Type.Functions(
                             Lambda(..), LambdaFunction,
                             getKind, assertKind, expectFunction, expectGround,
                             lambda1
                            ) where

import Lambda.Type.Kind (TKind(..))
import Lambda.Type.Error (KindError(..))

import Prelude
import Control.Monad.Error.Class (class MonadError, throwError)

-- Very simple lambda calculus built on top of the type system, so we
-- can define type-level operators.
data Lambda r = Ground r | Function { domain :: TKind, codomain :: TKind, body :: LambdaFunction r }

type LambdaFunction r = forall m. MonadError KindError m => Lambda r -> m (Lambda r)

getKind :: forall r. Lambda r -> TKind
getKind (Ground _) = Ty
getKind (Function { domain, codomain }) = domain `KArrow` codomain

assertKind :: forall m. MonadError KindError m => TKind -> TKind -> m Unit
assertKind expected actual
    | expected == actual = pure unit
    | otherwise = throwError $ KindError { expected, actual }

expectFunction :: forall m r. MonadError KindError m => TKind -> TKind -> Lambda r -> m (LambdaFunction r)
expectFunction domain codomain =
    case _ of
      Ground _ -> throwError $ KindError {
                    expected: domain `KArrow` codomain,
                    actual: Ty
                  }
      f @ (Function { body }) -> body <$ assertKind (getKind f) (domain `KArrow` codomain)

expectGround :: forall m r. MonadError KindError m => Lambda r -> m r
expectGround (Ground r) = pure r
expectGround f = throwError $ KindError {
                    expected: Ty,
                    actual: getKind f
                  }

-- Helper for producing lambdas of kind (Type -> Type)
lambda1 :: forall r. (r -> r) -> Lambda r
lambda1 f = Function { domain: Ty, codomain: Ty, body }
    where body :: LambdaFunction r
          body a = (Ground <<< f) <$> expectGround a
