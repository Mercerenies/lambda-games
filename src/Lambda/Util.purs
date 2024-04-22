
module Lambda.Util(
                   toList, toUnfoldable,
                   fromChars, guarded,
                   modifyError, repeatedly,
                   unsafeFromRight
                  ) where

import Data.String.CodeUnits (fromCharArray)
import Data.List (List(..), (:))
import Data.List (toUnfoldable) as List
import Data.Foldable (class Foldable, foldr)
import Data.Unfoldable (class Unfoldable, replicate)
import Data.Either (Either(..), either)
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
