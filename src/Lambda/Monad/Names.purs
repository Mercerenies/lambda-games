
module Lambda.Monad.Names(
                          NamesT,
                          withNameBound, askBindings,
                          freshStrings
                         ) where

import Lambda.Util.InfiniteList(InfiniteList, unfoldrForever, cons)

import Prelude
import Data.Tuple(Tuple(..))
import Data.List(List, (:))

-- It's really just a very specialized form of the reader monad.
newtype NamesT :: Type -> (Type -> Type) -> Type -> Type
newtype NamesT s m a = NamesT (List s -> m a)

instance Functor m => Functor (NamesT s m) where
    map f (NamesT g) = NamesT $ map f <<< g

instance Apply m => Apply (NamesT s m) where
    apply (NamesT f) (NamesT g) = NamesT \bindings -> f bindings <*> g bindings

instance Applicative m => Applicative (NamesT s m) where
    pure x = NamesT \_ -> pure x

instance Bind m => Bind (NamesT s m) where
    bind (NamesT g) f = NamesT \bindings -> do
                                     b <- g bindings
                                     let NamesT f' = f b
                                     f' bindings

instance Monad m => Monad (NamesT s m)

withNameBound :: forall s m a. s -> NamesT s m a -> NamesT s m a
withNameBound newName (NamesT ma) = NamesT \bindings -> ma (newName : bindings)

askBindings :: forall s m. Applicative m => NamesT s m (List s)
askBindings = NamesT pure

freshStrings :: String -> InfiniteList String
freshStrings s = cons s $ unfoldrForever (\n -> Tuple (s <> "_" <> show n) (n + 1)) 0

--withFreshName :: Monad m => NameStream s -> NamesT s m a -> NamesT s m a
--withFreshName freshNameStream (NamesT ma) = 
