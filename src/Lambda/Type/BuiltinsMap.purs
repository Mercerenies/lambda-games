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
module Lambda.Type.BuiltinsMap(
                               BuiltinsMap(..), lookup,
                               Builtin(..),
                               fromLookupMap, variableNamer
                              ) where

import Lambda.LookupMap (LookupMap)
import Lambda.LookupMap (lookup) as LookupMap
import Lambda.Type.Relation (Relation)
import Lambda.Type.Functions (Lambda)
import Lambda.Type.Typeclass (WithContexts)
import Lambda.Util.InfiniteList (InfiniteList)
import Lambda.Monad.Names (freshStrings)

import Prelude
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype)
import Data.String.Common (toLower)
import Control.Alternative (alt, empty)

newtype BuiltinsMap :: (Type -> Type) -> Type
newtype BuiltinsMap m = BuiltinsMap (LookupMap String (Builtin m))

newtype Builtin :: (Type -> Type) -> Type
newtype Builtin m = Builtin {
      relation :: Lambda m (WithContexts Relation),
      nameStream :: InfiniteList String
    }

derive instance Newtype (BuiltinsMap m) _

instance Semigroup (BuiltinsMap m) where
    append (BuiltinsMap lookupMap1) (BuiltinsMap lookupMap2) =
        BuiltinsMap (lookupMap1 `alt` lookupMap2)

instance Monoid (BuiltinsMap m) where
    mempty = BuiltinsMap empty

lookup :: forall m. String -> BuiltinsMap m -> Maybe (Builtin m)
lookup s (BuiltinsMap lookupMap) = LookupMap.lookup s lookupMap

fromLookupMap :: forall m. LookupMap String (Builtin m) -> BuiltinsMap m
fromLookupMap = BuiltinsMap

variableNamer :: forall m. BuiltinsMap m -> String -> InfiniteList String
variableNamer m s = lookup s m # maybe defaultStream (\(Builtin x) -> x.nameStream)
    where defaultStream = freshStrings $ toLower s
