
module Lambda.Monad.Names(
                          NamesT(), Names,
                          class MonadNames, withNameBound, askBindings,
                          freshStrings, withFreshName, withName, withName2,
                          runNamesTWith, runNamesT,
                          runNamesWith, runNames
                         ) where

import Lambda.Util.InfiniteList(InfiniteList, unfoldrForever, cons, find)

import Prelude
import Data.Tuple (Tuple(..))
import Data.List (List(..), (:), notElem)
import Data.Identity (Identity(..))
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Error.Class (class MonadThrow, class MonadError, throwError, catchError)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Morph (class MFunctor, class MMonad, hoist)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Reader.Trans (ReaderT)
import Safe.Coerce (coerce)

class Monad m <= MonadNames s m | m -> s where
    withNameBound :: forall a. s -> m a -> m a
    askBindings :: m (List s)

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

instance MonadTrans (NamesT s) where
    lift ma = NamesT \_ -> ma

instance MFunctor (NamesT s) where
    hoist f (NamesT ma) = NamesT (f <<< ma)

instance MMonad (NamesT s) where
    embed f (NamesT mb) = NamesT \bindings -> let NamesT nb = f (mb bindings) in nb bindings

instance Monad m => MonadNames s (NamesT s m) where
    withNameBound newName (NamesT ma) = NamesT \bindings -> ma (newName : bindings)
    askBindings = NamesT pure

-- Instances that commute NamesT over classes.

instance (MonadThrow e m) => MonadThrow e (NamesT s m) where
    throwError e = lift $ throwError e

instance (MonadError e m) => MonadError e (NamesT s m) where
    catchError (NamesT ma) f = NamesT \bindings -> catchError (ma bindings) (errorHandler bindings)
      where errorHandler bindings err = let NamesT ma' = f err in ma' bindings

instance (MonadAsk r m) => MonadAsk r (NamesT s m) where
    ask = lift ask

instance (MonadReader r m) => MonadReader r (NamesT s m) where
    local f = hoist (local f)

-- Instances that commute MonadNames over types.

instance MonadNames s m => MonadNames s (ExceptT e m) where
    withNameBound s = hoist (withNameBound s)
    askBindings = lift askBindings

instance MonadNames s m => MonadNames s (ReaderT r m) where
    withNameBound s = hoist (withNameBound s)
    askBindings = lift askBindings

-- End of instances.

freshStrings :: String -> InfiniteList String
freshStrings s = cons s $ cons (s <> "'") $ unfoldrForever (\n -> Tuple (s <> show n) (n + 1)) 0

withFreshName :: forall s m a. MonadNames s m => Eq s => InfiniteList s -> (s -> m a) -> m a
withFreshName freshNameStream ma = do
  bindings <- askBindings
  let unboundName = find (\name -> name `notElem` bindings) freshNameStream
  withNameBound unboundName (ma unboundName)

withName :: forall m a. MonadNames String m => String -> (String -> m a) -> m a
withName baseName = withFreshName (freshStrings baseName)

withName2 :: forall m a. MonadNames String m => String -> (String -> String -> m a) -> m a
withName2 baseName ma = withName baseName $ \a -> withName baseName $ \a' -> ma a a'

runNamesTWith :: forall s m a. Monad m => List s -> NamesT s m a -> m a
runNamesTWith bindings (NamesT ma) = ma bindings

runNamesT :: forall s m a. Monad m => NamesT s m a -> m a
runNamesT = runNamesTWith Nil

runNamesWith :: forall s a. List s -> Names s a -> a
runNamesWith bindings = coerce <<< runNamesTWith bindings

runNames :: forall s a. Names s a -> a
runNames = coerce <<< runNamesT
