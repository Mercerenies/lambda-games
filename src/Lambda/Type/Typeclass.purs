
module Lambda.Type.Typeclass(
                              TypeclassBody, TypeclassFunction(..),
                              RelationifyFunction,
                              WithContexts(..), expectGroundTy, expectGroundConstraint,
                              toArray, singleton, computeAssumptions
                             ) where

import Lambda.Term (Term(..))
import Lambda.Type (TType)
import Lambda.Type.Relation (Relation, runRelation)
import Lambda.Type.Kind (GroundKind(..))
import Lambda.Type.Functions (Lambda, class GroundKindInferrable, class NeverConstraint, getGroundKind, expectGround)
import Lambda.Type.Error (class FromKindError)
import Lambda.Predicate (Predicate)

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Traversable (traverse)
import Effect.Exception.Unsafe (unsafeThrow)
import Control.Monad.Error.Class (class MonadError)

newtype TypeclassBody m = TypeclassBody (Array (TypeclassFunction m))

newtype TypeclassFunction m = TypeclassFunction {
      methodName :: String, -- //// (TODO) support operators as a simplification somehow
      methodType :: RelationifyFunction m -> m Relation
    }

type RelationifyFunction :: (Type -> Type) -> Type
type RelationifyFunction m = TType -> m (Lambda m (WithContexts m Relation))

derive newtype instance Semigroup (TypeclassBody m)
derive newtype instance Monoid (TypeclassBody m)

derive instance Generic (TypeclassFunction m) _

-- Adds support for constraint kinds to an existing kind system.
data WithContexts :: (Type -> Type) -> Type -> Type
data WithContexts m r = NonContext r | Context (TypeclassBody m)

instance GroundKindInferrable r => GroundKindInferrable (WithContexts m r) where
    getGroundKind (NonContext r) = getGroundKind r
    getGroundKind (Context _) = GConstraint

expectGroundTy :: forall e m r. FromKindError e => MonadError e m => GroundKindInferrable r =>
                  Lambda m (WithContexts m r) -> m r
expectGroundTy lam = unwrapGround <$> expectGround GType lam
    where -- Safety: This is safe, because we know (per the typeclass
          -- instance above that is owned by this module) that GType
          -- can never belong to a value using constructor Context.
          unwrapGround (NonContext r) = r
          unwrapGround (Context _) = unsafeThrow "expectGroundTy: unexpected context"

expectGroundConstraint :: forall e m r. FromKindError e => MonadError e m => NeverConstraint r =>
                          Lambda m (WithContexts m r) -> m (TypeclassBody m)
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

toArray :: forall m. TypeclassBody m -> Array (TypeclassFunction m)
toArray (TypeclassBody a) = a

singleton :: forall m. String -> (RelationifyFunction m -> m Relation) -> TypeclassBody m
singleton methodName methodType = TypeclassBody [TypeclassFunction { methodName, methodType }]

computeAssumptions :: forall m. Applicative m => RelationifyFunction m ->
                      TypeclassBody m -> m (Array Predicate)
computeAssumptions relationifyFunction = traverse computeAssumption <<< toArray
    where computeAssumption :: TypeclassFunction m -> m Predicate
          computeAssumption (TypeclassFunction { methodName, methodType }) = ado
              methodRelation <- methodType relationifyFunction
              in runRelation methodRelation (Var methodName) (Var methodName)
