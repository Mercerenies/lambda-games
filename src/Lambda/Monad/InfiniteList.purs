
module Lambda.Monad.InfiniteList(
                          InfiniteList(),
                          cons, lazyCons,
                          unfoldrForever
                         ) where

import Data.Lazy(Lazy, defer)
import Data.Tuple(Tuple(..))
import Prelude

data InfiniteList a = InfiniteList (Lazy (Tuple a (InfiniteList a)))

cons :: forall a. a -> InfiniteList a -> InfiniteList a
cons x xs = InfiniteList (defer (\_ -> Tuple x xs))

lazyCons :: forall a. a -> Lazy (InfiniteList a) -> Lazy (InfiniteList a)
lazyCons x = map (cons x)

unfoldrForever :: forall a b. (b -> Tuple a b) -> b -> InfiniteList a
unfoldrForever f seed = go seed
  where go b = InfiniteList (defer \_ -> let Tuple a b' = f b in
                                         Tuple a (go b'))
