
module Lambda.Monad.Names(
                          NamesT(), Names,
                          withNameBound, askBindings,
                          freshStrings, withName,
                          runNamesTWith, runNamesT,
                          runNamesWith, runNames
                         ) where

import Lambda.Util.InfiniteList(InfiniteList, unfoldrForever, cons, find)

import Prelude
import Data.Tuple (Tuple(..))
import Data.List (List(..), (:), notElem)
import Data.Identity (Identity(..))
import Safe.Coerce (coerce)

-- It's really just a very specialized form of the reader monad.
newtype NamesT :: Type -> (Type -> Type) -> Type -> Type
newtype NamesT s m a = NamesT (List s -> m a)

type Names s = NamesT s Identity

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

withFreshName :: forall s m a. Monad m => Eq s => InfiniteList s -> NamesT s m a -> NamesT s m a
withFreshName freshNameStream ma = do
  bindings <- askBindings
  let unboundName = find (\name -> name `notElem` bindings) freshNameStream
  withNameBound unboundName ma

withName :: forall m a. Monad m => String -> NamesT String m a -> NamesT String m a
withName baseName = withFreshName (freshStrings baseName)

runNamesTWith :: forall s m a. Monad m => List s -> NamesT s m a -> m a
runNamesTWith bindings (NamesT ma) = ma bindings

runNamesT :: forall s m a. Monad m => NamesT s m a -> m a
runNamesT = runNamesTWith Nil

runNamesWith :: forall s a. List s -> Names s a -> a
runNamesWith bindings = coerce <<< runNamesTWith bindings

runNames :: forall s a. Names s a -> a
runNames = coerce <<< runNamesT
