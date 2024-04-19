
module Lambda.Type.Functions(
                             Lambda(..), LambdaFunction,
                             KindError(..),
                             getKind, assertKind, expectFunction, expectGround
                            ) where

import Lambda.Type.Kind (TKind(..))

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Control.Monad.Error.Class (class MonadError, throwError)

-- Very simple lambda calculus built on top of the type system, so we
-- can define type-level operators.
data Lambda r = Ground r | Function { domain :: TKind, codomain :: TKind, body :: LambdaFunction r }

type LambdaFunction r = forall m. MonadError KindError m => Lambda r -> m (Lambda r)

newtype KindError = KindError { expected :: TKind, actual :: TKind }

derive instance Eq KindError
derive instance Generic KindError _

instance Show KindError where
    show err = genericShow err

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
