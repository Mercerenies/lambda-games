
module Lambda.Type.Typeclass(
                              TypeclassBody, TypeclassFunction(..),
                              WithContexts(..), expectGroundTy, expectGroundConstraint,
                              toArray, singleton
                             ) where

import Lambda.Type.Relation (Relation)
import Lambda.Type.Kind (GroundKind(..))
import Lambda.Type.Functions (Lambda, class GroundKindInferrable, class NeverConstraint, getGroundKind, expectGround)
import Lambda.Type.Error (class FromKindError)

import Prelude
import Data.Generic.Rep (class Generic)
import Effect.Exception.Unsafe (unsafeThrow)
import Control.Monad.Error.Class (class MonadError)

newtype TypeclassBody = TypeclassBody (Array TypeclassFunction)

newtype TypeclassFunction = TypeclassFunction {
      methodName :: String, -- //// (TODO) support operators as a simplification somehow
      methodType :: Relation
    }

derive instance Generic TypeclassBody _
derive newtype instance Semigroup TypeclassBody
derive newtype instance Monoid TypeclassBody

derive instance Generic TypeclassFunction _

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

expectGroundConstraint :: forall e m r. FromKindError e => MonadError e m => NeverConstraint r =>
                          Lambda m (WithContexts r) -> m TypeclassBody
expectGroundConstraint = map unwrapConstraint <<< expectGround GConstraint
    where -- Safety: This is safe, because a type which lawfully
          -- implements NeverConstraint will never give GConstraint.
          -- So anything of kind GConstraint must be a Context by
          -- process of elimination. This prevents us from getting
          -- into situations where we try to expectGroundConstraint on
          -- a (WithContexts (WithContexts r)) and find a context at
          -- the wrong layer, for instance.
          unwrapConstraint (Context typeclass) = typeclass
          unwrapConstraint (NonContext _) = unsafeThrow "expectGroundConstraint: NonContext"

toArray :: TypeclassBody -> Array TypeclassFunction
toArray (TypeclassBody a) = a

singleton :: String -> Relation -> TypeclassBody
singleton methodName methodType = TypeclassBody [TypeclassFunction { methodName, methodType }]
