
module Lambda.Type.Typeclass(
                              TypeclassBody, TypeclassFunction(..),
                              MethodName(..),
                              WithContexts(..),
                              expectGroundTy, expectGroundConstraint,
                              expectGroundTy', expectGroundConstraint',
                              singleton, toArray, methodNameToTerm
                             ) where

import Lambda.Term (Term(..))
import Lambda.Type (TType)
import Lambda.Type.Kind (GroundKind(..))
import Lambda.Type.Functions (LambdaF, TaggedLambdaF(..),
                              class GroundKindInferrable, class NeverConstraint, getGroundKind, expectGround)
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

data MethodName = BasicName String | OperatorName String

derive instance Eq TypeclassBody
derive newtype instance Semigroup TypeclassBody
derive newtype instance Monoid TypeclassBody

derive instance Eq TypeclassFunction
derive instance Generic TypeclassFunction _

derive instance Eq MethodName
derive instance Generic MethodName _

instance Show TypeclassFunction where
    show x = genericShow x

instance Show MethodName where
    show x = genericShow x

-- Adds support for constraint kinds to an existing kind system.
data WithContexts r = NonContext r | Context TypeclassBody

instance GroundKindInferrable r => GroundKindInferrable (WithContexts r) where
    getGroundKind (NonContext r) = getGroundKind r
    getGroundKind (Context _) = GConstraint

expectGroundTy :: forall e m r a. FromKindError e => MonadError e m => GroundKindInferrable r =>
                  LambdaF m (WithContexts r) a -> m r
expectGroundTy lam = unwrapGround <$> expectGround GType lam
    where -- Safety: This is safe, because we know (per the typeclass
          -- instance above that is owned by this module) that GType
          -- can never belong to a value using constructor Context.
          unwrapGround (NonContext r) = r
          unwrapGround (Context _) = unsafeThrow "expectGroundTy: unexpected context"

expectGroundConstraint :: forall e m r a. FromKindError e => MonadError e m => NeverConstraint r =>
                          LambdaF m (WithContexts r) a -> m TypeclassBody
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

-- Variants of the assertions that work on TaggedLambdas instead. Does
-- not use the tag.

expectGroundTy' :: forall e t m r a. FromKindError e => MonadError e m => GroundKindInferrable r =>
                   TaggedLambdaF t m (WithContexts r) a -> m r
expectGroundTy' (TaggedLambdaF _ lam) = expectGroundTy lam

expectGroundConstraint' :: forall e t m r a. FromKindError e => MonadError e m => NeverConstraint r =>
                           TaggedLambdaF t m (WithContexts r) a -> m TypeclassBody
expectGroundConstraint' (TaggedLambdaF _ lam) = expectGroundConstraint lam

singleton :: MethodName -> TType -> TypeclassBody
singleton methodName methodType = TypeclassBody [TypeclassFunction { methodName, methodType }]

toArray :: TypeclassBody -> Array TypeclassFunction
toArray (TypeclassBody functions) = functions

methodNameToTerm :: MethodName -> Term
methodNameToTerm (BasicName s) = Var s
methodNameToTerm (OperatorName s) = OperatorFunction s
