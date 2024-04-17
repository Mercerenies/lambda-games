
module Lambda.Util(
                   toList, toUnfoldable,
                   fromChars, guarded
                  ) where

import Data.String.CodeUnits (fromCharArray)
import Data.List (List(..), (:))
import Data.List (toUnfoldable) as List
import Data.Foldable (class Foldable, foldr)
import Data.Unfoldable (class Unfoldable)
import Control.Alternative (guard)
import Control.MonadPlus (class MonadPlus)
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
