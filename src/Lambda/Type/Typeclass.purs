
module Lambda.Type.Typeclass(
                              TypeclassBody(..), TypeclassFunction(..), MethodName(..)
                             ) where

import Lambda.Type (TType)
import Lambda.Type.Kind (GroundKind(..))
import Lambda.Type.Functions (class GroundKindInferrable, getGroundKind)

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

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
