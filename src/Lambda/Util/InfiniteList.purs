
module Lambda.Util.InfiniteList(
                          InfiniteList(),
                          cons, lazyCons,
                          repeat, unfoldrForever,
                          find,
                          head, tail, take, prepend, prependLazy, cycle
                         ) where

import Data.Lazy (Lazy, defer, force)
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))
import Data.FunctorWithIndex (class FunctorWithIndex)
import Control.Lazy (fix)
import Control.Comonad (class Extend, class Comonad, (=>>), extend)
import Prelude

newtype InfiniteList a = InfiniteList (Lazy (Tuple a (InfiniteList a)))

cons :: forall a. a -> InfiniteList a -> InfiniteList a
cons x xs = InfiniteList (defer (\_ -> Tuple x xs))

lazyCons :: forall a. a -> Lazy (InfiniteList a) -> InfiniteList a
lazyCons x = InfiniteList <<< map (Tuple x)

unfoldrForever :: forall a b. (b -> Tuple a b) -> b -> InfiniteList a
unfoldrForever f seed = go seed
  where go b = InfiniteList (defer \_ -> let Tuple a b' = f b in
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

take :: forall a. Int -> InfiniteList a -> List a
take n (InfiniteList xs)
    | n <= 0 = Nil
    | otherwise = let Tuple x xs' = force xs in x : take (n - 1) xs'

prependLazy :: forall a. List a -> Lazy (InfiniteList a) -> Lazy (InfiniteList a)
prependLazy Nil ys = ys
prependLazy (x : xs) ys = prependLazy xs ys =>> lazyCons x

prepend :: forall a. List a -> InfiniteList a -> InfiniteList a
prepend Nil ys = ys
prepend (x : xs) ys = cons x (prepend xs ys)

cycle :: forall a. List a -> Lazy (InfiniteList a)
cycle xs = fix (prependLazy xs)

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
    extend f list = lazyCons (f list) (defer \_ -> extend f (tail list))

instance Comonad InfiniteList where
    extract = head
