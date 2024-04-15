
module Lambda.Util.InfiniteList(
                          InfiniteList(),
                          cons, lazyCons,
                          repeat,
                          unfoldrForever
                         ) where

import Data.Lazy(Lazy, defer, force)
import Data.Tuple(Tuple(..))
import Data.FunctorWithIndex(class FunctorWithIndex)
import Prelude

newtype InfiniteList a = InfiniteList (Lazy (Tuple a (InfiniteList a)))

cons :: forall a. a -> InfiniteList a -> InfiniteList a
cons x xs = InfiniteList (defer (\_ -> Tuple x xs))

lazyCons :: forall a. a -> Lazy (InfiniteList a) -> Lazy (InfiniteList a)
lazyCons x = map (cons x)

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
