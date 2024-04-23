
module Lambda.Type.Typeclass(
                              TypeclassBody(..), TypeclassFunction(..), MethodName(..),
                              WithContexts(..), expectGroundTy
                             ) where

import Lambda.Type (TType)
import Lambda.Type.Kind (GroundKind(..))
import Lambda.Type.Functions (Lambda, class GroundKindInferrable, getGroundKind, expectGround)
import Lambda.Type.Error (class FromKindError)

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Exception.Unsafe (unsafeThrow)
import Control.Monad.Error.Class (class MonadError)

newtype TypeclassBody = TypeclassBody (Array TypeclassFunction)

newtype TypeclassFunction = TypeclassFunction {
      methodName :: MethodName,
      methodType :: TType
    }

data MethodName = StringName String
                | OperatorName String

derive instance Eq TypeclassBody
derive instance Generic TypeclassBody _

instance Show TypeclassBody where
    show x = genericShow x

derive instance Eq TypeclassFunction
derive instance Generic TypeclassFunction _

instance Show TypeclassFunction where
    show x = genericShow x

derive instance Eq MethodName
derive instance Generic MethodName _

instance Show MethodName where
    show x = genericShow x

-- Adds support for constraint kinds to an existing kind system.
data WithContexts r = NonContext r | Context TypeclassBody

instance GroundKindInferrable r => GroundKindInferrable (WithContexts r) where
    getGroundKind (NonContext r) = getGroundKind r
    getGroundKind (Context _) = GConstraint

expectGroundTy :: forall e m r. FromKindError e => MonadError e m => GroundKindInferrable r =>
                  Lambda m (WithContexts r) -> m r
expectGroundTy lam = unwrapGround <$> expectGround GType lam
    where -- This is safe, because we know (per the typeclass instance
          -- above that is owned by this module) that GType can never
          -- belong to a value using constructor Context. If we decide
          -- to write the other assertion function (which would return
          -- `m TypeclassBody`), then the same assertion is NOT safe,
          -- as the underlying type r might return GConstraint.
          unwrapGround (NonContext r) = r
          unwrapGround (Context _) = unsafeThrow "expectGroundTy: unexpected context"
