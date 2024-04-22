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
module Lambda.Util.InfiniteList(
                          InfiniteList(),
                          cons, lazyCons,
                          repeat, unfoldrForever,
                          find, concat, concatMap,
                          head, tail, intersperse, take, prepend, cycle
                         ) where

import Lambda.Util (toList)

import Data.Lazy (Lazy, force)
import Data.Lazy (defer) as Lazy
import Data.List (List(..), (:))
import Data.NonEmpty(NonEmpty)
import Data.Tuple (Tuple(..))
import Data.Semigroup.Foldable (class Foldable1)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Control.Lazy (class Lazy, fix, defer)
import Control.Comonad (class Extend, class Comonad, extend)
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)
import Test.QuickCheck.Gen (sized)
import Prelude

newtype InfiniteList a = InfiniteList (Lazy (Tuple a (InfiniteList a)))

cons :: forall a. a -> InfiniteList a -> InfiniteList a
cons x xs = InfiniteList (Lazy.defer (\_ -> Tuple x xs))

lazyCons :: forall a. a -> Lazy (InfiniteList a) -> InfiniteList a
lazyCons x = InfiniteList <<< map (Tuple x)

unfoldrForever :: forall a b. (b -> Tuple a b) -> b -> InfiniteList a
unfoldrForever f seed = go seed
  where go b = InfiniteList (Lazy.defer \_ -> let Tuple a b' = f b in
                                              Tuple a (go b'))

repeat :: forall a. a -> InfiniteList a
repeat a = unfoldrForever (\_ -> Tuple a unit) unit

-- Always finds a match or hangs, since the list is infinite.
find :: forall a. (a -> Boolean) -> InfiniteList a -> a
find p (InfiniteList xs) =
    let Tuple x xs' = force xs in
    if p x then x else find p xs'

head :: forall a. InfiniteList a -> a
head (InfiniteList xs) = let Tuple x _ = force xs in x

tail :: forall a. InfiniteList a -> InfiniteList a
tail (InfiniteList xs) = let Tuple _ xs' = force xs in xs'

intersperse :: forall f a. Foldable1 f => f (InfiniteList a) -> InfiniteList a
intersperse = toList >>> go
    where go xss = prepend (map head xss) $ defer \_ -> go (map tail xss)

take :: forall a. Int -> InfiniteList a -> List a
take n (InfiniteList xs)
    | n <= 0 = Nil
    | otherwise = let Tuple x xs' = force xs in x : take (n - 1) xs'

prepend :: forall a. List a -> InfiniteList a -> InfiniteList a
prepend Nil ys = ys
prepend (x : xs) ys = cons x (prepend xs ys)

cycle :: forall a. NonEmpty List a -> InfiniteList a
cycle xs = let xs' = toList xs in fix (prepend xs')

commuteLazy :: forall a. Lazy (InfiniteList a) -> InfiniteList a
commuteLazy xs = InfiniteList $ join $ map (\(InfiniteList xs') -> xs') xs

concat :: forall a. InfiniteList (List a) -> InfiniteList a
concat (InfiniteList xss) =
    commuteLazy $ map (\(Tuple xs xss') -> prepend xs (concat xss')) xss

concatMap :: forall a b. (a -> List b) -> InfiniteList a -> InfiniteList b
concatMap f = concat <<< map f

instance Functor InfiniteList where
    map f (InfiniteList xs) = InfiniteList (map (\(Tuple y ys) -> Tuple (f y) (map f ys)) xs)

instance FunctorWithIndex Int InfiniteList where
    mapWithIndex f = go 0
        where go i (InfiniteList xs) = InfiniteList $ map (\(Tuple y ys) -> Tuple (f i y) (go (i + 1) ys)) xs

instance Apply InfiniteList where
    apply (InfiniteList ff) (InfiniteList xx) = InfiniteList ado
      Tuple f fs <- ff
      Tuple x xs <- xx
      in Tuple (f x) (apply fs xs)

instance Applicative InfiniteList where
    pure = repeat

instance Extend InfiniteList where
    extend f list = lazyCons (f list) (Lazy.defer \_ -> extend f (tail list))

instance Comonad InfiniteList where
    extract = head

instance Arbitrary a => Arbitrary (InfiniteList a) where
    -- Generate an arbitrary prefix, followed by an infinite sequence.
    arbitrary = ado
      prefix :: List a <- arbitrary
      cycled :: NonEmpty List a <- arbitrary
      in prepend prefix (cycle cycled) -- cycle argument must be nonempty

instance Coarbitrary a => Coarbitrary (InfiniteList a) where
    coarbitrary list gen = do
      prefix <- sized \n -> pure (take n list)
      coarbitrary prefix gen

instance Lazy (InfiniteList a) where
    defer f = InfiniteList (Lazy.defer \_ -> let InfiniteList tup = f unit in force tup)
