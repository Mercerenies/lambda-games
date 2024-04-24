
module Lambda.Recursion(
                        Algebra,
                        Mu(..), unMu,
                        cata
                       ) where

import Prelude

type Algebra :: (Type -> Type) -> Type -> Type
type Algebra f a = f a -> a

newtype Mu :: (Type -> Type) -> Type
newtype Mu f = Mu (f (Mu f))

unMu :: forall f. Mu f -> f (Mu f)
unMu (Mu x) = x

cata :: forall f a. Functor f => Algebra f a -> Mu f -> a
cata alg = go
    where go (Mu x) = alg $ map go x
