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
module Lambda.LookupMap(
                        LookupMap(..), runLookupMap, lookup, singleton, fromMap
                       ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (lookup) as Map
import Control.MonadPlus (class MonadPlus, class Alternative, class Alt, class Plus, alt)

-- Basic data structure isomorphic to (s -> Maybe a), but with lifted
-- MonadPlus, etc. operators.
newtype LookupMap s a = LookupMap (s -> Maybe a)

derive instance Functor (LookupMap s)

instance Apply (LookupMap s) where
    apply (LookupMap ff) (LookupMap xx) = LookupMap \s -> ff s `apply` xx s

instance Applicative (LookupMap s) where
    pure x = LookupMap (const $ pure x)

instance Bind (LookupMap s) where
    bind (LookupMap aa) f =
        LookupMap \s -> do
            a <- aa s
            let LookupMap bb = f a
            bb s

instance Monad (LookupMap s)

instance Alt (LookupMap s) where
    alt (LookupMap aa) (LookupMap aa') = LookupMap \s -> aa s `alt` aa' s

instance Plus (LookupMap s) where
    empty = LookupMap (const Nothing)

instance Alternative (LookupMap s)

instance MonadPlus (LookupMap s)

runLookupMap :: forall s a. LookupMap s a -> s -> Maybe a
runLookupMap (LookupMap aa) = aa

lookup :: forall s a. s -> LookupMap s a -> Maybe a
lookup = flip runLookupMap

singleton :: forall s a. Eq s => s -> a -> LookupMap s a
singleton k v = LookupMap \k' -> if k == k' then Just v else Nothing

fromMap :: forall s a. Ord s => Map s a -> LookupMap s a
fromMap m = LookupMap \k -> Map.lookup k m
