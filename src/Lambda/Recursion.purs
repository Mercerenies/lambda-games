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
                        cata, ana
                       ) where

import Prelude

type Algebra :: (Type -> Type) -> Type -> Type
type Algebra f a = f a -> a

type Coalgebra :: (Type -> Type) -> Type -> Type
type Coalgebra f a = a -> f a

newtype Mu :: (Type -> Type) -> Type
newtype Mu f = Mu (f (Mu f))

unMu :: forall f. Mu f -> f (Mu f)
unMu (Mu x) = x

cata :: forall f a. Functor f => Algebra f a -> Mu f -> a
cata alg = go
    where go (Mu x) = alg $ map go x

ana :: forall f a. Functor f => Coalgebra f a -> a -> Mu f
ana coalg = go
    where go x = Mu $ map go $ coalg x
