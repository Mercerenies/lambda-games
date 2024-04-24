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
module Lambda.Recursion(
                        Algebra,
                        Mu(..), unMu,
                        WithTag(..), getTag, getTaggedValue,
                        cata
                       ) where

import Prelude

type Algebra :: (Type -> Type) -> Type -> Type
type Algebra f a = f a -> a

newtype Mu :: (Type -> Type) -> Type
newtype Mu f = Mu (f (Mu f))

data WithTag :: Type -> (Type -> Type) -> Type -> Type
data WithTag t f a = WithTag t (f a)

getTag :: forall t f a. WithTag t f a -> t
getTag (WithTag t _) = t

getTaggedValue :: forall t f a. WithTag t f a -> f a
getTaggedValue (WithTag _ fa) = fa

unMu :: forall f. Mu f -> f (Mu f)
unMu (Mu x) = x

cata :: forall f a. Functor f => Algebra f a -> Mu f -> a
cata alg = go
    where go (Mu x) = alg $ map go x
