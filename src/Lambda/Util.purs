
module Lambda.Util(
                   toList, toUnfoldable,
                   fromChars
                  ) where

import Data.String.CodeUnits (fromCharArray)
import Data.List (List(..), (:))
import Data.List (toUnfoldable) as List
import Data.Foldable (class Foldable, foldr)
import Data.Unfoldable (class Unfoldable)
import Prelude

toList :: forall f a. Foldable f => f a -> List a
toList = foldr (:) Nil

toUnfoldable :: forall f f' a. Foldable f => Unfoldable f' => f a -> f' a
toUnfoldable = toList >>> List.toUnfoldable

fromChars :: forall f. Foldable f => f Char -> String
fromChars = toUnfoldable >>> fromCharArray
