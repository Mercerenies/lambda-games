
module Lambda.Type.Typeclass(
                              TypeclassBody(..), TypeclassFunction(..),
                              WithContexts(..), expectGroundTy, expectGroundConstraint
                             ) where

import Lambda.Type (TType)
import Lambda.Type.Kind (GroundKind(..))
import Lambda.Type.Functions (Lambda, class GroundKindInferrable, getGroundKind, expectGround)
import Lambda.Type.Error (class FromKindError)
import Lambda.Type.Relation (Relation)

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Exception.Unsafe (unsafeThrow)
import Control.Monad.Error.Class (class MonadError)

newtype TypeclassBody = TypeclassBody (Array TypeclassFunction)

newtype TypeclassFunction = TypeclassFunction {
      methodName :: String, -- //// (TODO) support operators as a simplification somehow
      methodType :: TType
    }

derive instance Eq TypeclassBody
derive instance Generic TypeclassBody _

instance Show TypeclassBody where
    show x = genericShow x

derive instance Eq TypeclassFunction
derive instance Generic TypeclassFunction _

instance Show TypeclassFunction where
    show x = genericShow x

-- Adds support for constraint kinds to an existing kind system.
data WithContexts r = NonContext r | Context TypeclassBody

instance GroundKindInferrable r => GroundKindInferrable (WithContexts r) where
    getGroundKind (NonContext r) = getGroundKind r
    getGroundKind (Context _) = GConstraint

expectGroundTy :: forall e m r. FromKindError e => MonadError e m => GroundKindInferrable r =>
                  Lambda m (WithContexts r) -> m r
expectGroundTy lam = unwrapGround <$> expectGround GType lam
    where -- Safety: This is safe, because we know (per the typeclass
          -- instance above that is owned by this module) that GType
          -- can never belong to a value using constructor Context.
          unwrapGround (NonContext r) = r
          unwrapGround (Context _) = unsafeThrow "expectGroundTy: unexpected context"

expectGroundConstraint :: forall e m. FromKindError e => MonadError e m =>
                          Lambda m (WithContexts Relation) -> m TypeclassBody
expectGroundConstraint = map unwrapConstraint <<< expectGround GConstraint
    where -- Safety: This is safe, because the Relation type (as a
          -- GroundTypeInferrable) will always give GType, not
          -- GConstraint. So anything of kind GConstraint must be a
          -- Context by progress of elimination. Note that this is
          -- only true for Relation, not arbitrary
          -- GroundKindInferrable r => r, so we restrict
          -- expectGroundConstraint's signature appropriately.
          unwrapConstraint (Context typeclass) = typeclass
          unwrapConstraint (NonContext _) = unsafeThrow "expectGroundConstraint: NonContext"
