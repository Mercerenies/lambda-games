
module Lambda.Type.Error(
                         TypeError(..)
                        ) where

import Prelude
import Data.Show (class Show)

data TypeError = UnboundVariable String

derive instance Eq TypeError

instance Show TypeError where
    show (UnboundVariable s) = "Unbound type variable: " <> s
