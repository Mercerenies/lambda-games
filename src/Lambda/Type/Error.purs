
module Lambda.Type.Error(
                         TypeError(..),
                         KindError(..),
                         class FromKindError, fromKindError, kindError
                        ) where

import Lambda.Type (TType)
import Lambda.Type.Kind (TKind)
import Lambda.PrettyShow (prettyShow)

import Prelude

data TypeError = UnboundVariable String
               | MismatchedKinds KindError
               | ExpectedTypeFunction TType

newtype KindError = KindError { expected :: TKind, actual :: TKind }

class FromKindError e where
    fromKindError :: KindError -> e

kindError :: forall e. FromKindError e => { expected :: TKind, actual :: TKind } -> e
kindError = fromKindError <<< KindError

derive instance Eq KindError
derive instance Eq TypeError

instance Show TypeError where
    show (UnboundVariable s) = "Unbound type variable: " <> s
    show (MismatchedKinds err) = show err
    show (ExpectedTypeFunction err) = "Expected type-level function, got " <> prettyShow err

instance Show KindError where
    show (KindError { expected, actual }) =
        "Kind error: Expected " <> prettyShow expected <> ", actual " <> prettyShow actual

instance FromKindError KindError where
    fromKindError = identity

instance FromKindError TypeError where
    fromKindError = MismatchedKinds
