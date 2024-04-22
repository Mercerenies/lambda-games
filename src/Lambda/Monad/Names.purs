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
module Lambda.Monad.Names(
                          NamesT(), Names,
                          class MonadNames, withNameBound, askBindings,
                          freshStrings, interspersedStrings,
                          alphabetForever,
                          withFreshName, withFreshName2,
                          runNamesTWith, runNamesT,
                          runNamesWith, runNames
                         ) where

import Lambda.Util.InfiniteList (InfiniteList, intersperse, unfoldrForever, cons, find, prepend, concatMap)
import Lambda.Util (toList)

import Prelude
import Data.String.CodeUnits (toCharArray)
import Data.String.CodeUnits (singleton) as CodeUnits
import Data.Tuple (Tuple(..))
import Data.List (List(..), (:), notElem)
import Data.Identity (Identity(..))
import Data.Semigroup.Foldable (class Foldable1)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Error.Class (class MonadThrow, class MonadError, throwError, catchError)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Morph (class MFunctor, class MMonad, hoist)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Lazy (defer)
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

alphabet :: List String
alphabet = toList $ map CodeUnits.singleton $ toCharArray "abcdefghijklmnopqrstuvwxyz"

-- ["a", "b", "c", ..., "y", "z", "aa", "ab", "ac", ...]
alphabetForever :: InfiniteList String
alphabetForever =
    prepend (toList alphabet) $ concatMap (\s -> map (s <> _) alphabet) (defer \_ -> alphabetForever)

interspersedStrings :: forall f. Functor f => Foldable1 f => f String -> InfiniteList String
interspersedStrings = intersperse <<< map freshStrings

withFreshName :: forall s m a. MonadNames s m => Eq s => InfiniteList s -> (s -> m a) -> m a
withFreshName freshNameStream ma = do
  bindings <- askBindings
  let unboundName = find (\name -> name `notElem` bindings) freshNameStream
  withNameBound unboundName (ma unboundName)

withFreshName2 :: forall s m a. MonadNames s m => Eq s => InfiniteList s -> (s -> s -> m a) -> m a
withFreshName2 baseName ma = withFreshName baseName $ \a -> withFreshName baseName $ \a' -> ma a a'

runNamesTWith :: forall s m a. Monad m => List s -> NamesT s m a -> m a
runNamesTWith bindings (NamesT ma) = ma bindings

runNamesT :: forall s m a. Monad m => NamesT s m a -> m a
runNamesT = runNamesTWith Nil

runNamesWith :: forall s a. List s -> Names s a -> a
runNamesWith bindings = coerce <<< runNamesTWith bindings

runNames :: forall s a. Names s a -> a
runNames = coerce <<< runNamesT
