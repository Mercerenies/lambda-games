
module Lambda.Type.Error(
                         TypeError(..),
                         KindError(..)
                        ) where

import Lambda.Type.Kind (TKind)
import Lambda.PrettyShow (prettyShow)

import Prelude

data TypeError = UnboundVariable String
               | MismatchedKinds KindError

newtype KindError = KindError { expected :: TKind, actual :: TKind }

derive instance Eq KindError
derive instance Eq TypeError

instance Show TypeError where
    show (UnboundVariable s) = "Unbound type variable: " <> s
    show (MismatchedKinds err) = show err

instance Show KindError where
    show (KindError { expected, actual }) =
        "Kind error: Expected " <> prettyShow expected <> ", actual " <> prettyShow actual
