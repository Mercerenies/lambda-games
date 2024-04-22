-- Copyright 2024 Silvio Mayolo
--
-- Lambdagames is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Lambdagames. If not, see
-- <https://www.gnu.org/licenses/>.
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
               | UnboundGroundTerm String
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
    show (UnboundGroundTerm s) = "Unknown type: " <> s
    show (MismatchedKinds err) = show err
    show (ExpectedTypeFunction err) = "Expected type-level function, got " <> prettyShow err

instance Show KindError where
    show (KindError { expected, actual }) =
        "Kind error: Expected " <> prettyShow expected <> ", actual " <> prettyShow actual

instance FromKindError KindError where
    fromKindError = identity

instance FromKindError TypeError where
    fromKindError = MismatchedKinds
