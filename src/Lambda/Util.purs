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
module Lambda.Util(
                   toList, toUnfoldable,
                   fromChars, guarded,
                   modifyError, repeatedly,
                   unsafeFromRight, unsafeFromJust
                  ) where

import Data.String.CodeUnits (fromCharArray)
import Data.List (List(..), (:))
import Data.List (toUnfoldable) as List
import Data.Foldable (class Foldable, foldr)
import Data.Unfoldable (class Unfoldable, replicate)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe, maybe')
import Control.Alternative (guard)
import Control.MonadPlus (class MonadPlus)
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Effect.Exception.Unsafe (unsafeThrow)
import Prelude

toList :: forall f a. Foldable f => f a -> List a
toList = foldr (:) Nil

toUnfoldable :: forall f f' a. Foldable f => Unfoldable f' => f a -> f' a
toUnfoldable = toList >>> List.toUnfoldable

fromChars :: forall f. Foldable f => f Char -> String
fromChars = toUnfoldable >>> fromCharArray

-- As Control.Alternative.guard but keep the value
guarded :: forall m a. MonadPlus m => (a -> Boolean) -> m a -> m a
guarded p ma = do
  a <- ma
  guard $ p a
  pure a

modifyError :: forall m e e' a. MonadError e' m => (e -> e') -> ExceptT e m a -> m a
modifyError f ema = do
  ma <- runExceptT ema
  case ma of
    Left e -> throwError (f e)
    Right a -> pure a

repeatedly :: forall c a. Category c => Int -> c a a -> c a a
repeatedly n f = foldr compose identity lst
    where lst :: List (c a a)
          lst = replicate n f

unsafeFromRight :: forall a. Either String a -> a
unsafeFromRight = either unsafeThrow identity

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust = maybe' (\_ -> unsafeThrow "unsafeFromJust: Nothing") identity
