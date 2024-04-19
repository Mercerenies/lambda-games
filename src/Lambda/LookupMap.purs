
module Lambda.LookupMap(
                        LookupMap(..), runLookupMap
                       ) where

import Prelude
import Data.Maybe (Maybe(..))
import Control.MonadPlus (class MonadPlus, class Alternative, class Alt, class Plus, alt)

-- Basic data structure isomorphic to (s -> Maybe a), but with lifted
-- MonadPlus, etc. operators.
newtype LookupMap s a = LookupMap (s -> Maybe a)

derive instance Functor (LookupMap s)

instance Apply (LookupMap s) where
    apply (LookupMap ff) (LookupMap xx) = LookupMap \s -> ff s `apply` xx s

instance Applicative (LookupMap s) where
    pure x = LookupMap (const $ pure x)

instance Bind (LookupMap s) where
    bind (LookupMap aa) f =
        LookupMap \s -> do
            a <- aa s
            let LookupMap bb = f a
            bb s

instance Monad (LookupMap s)

instance Alt (LookupMap s) where
    alt (LookupMap aa) (LookupMap aa') = LookupMap \s -> aa s `alt` aa' s

instance Plus (LookupMap s) where
    empty = LookupMap (const Nothing)

instance Alternative (LookupMap s)

instance MonadPlus (LookupMap s)

runLookupMap :: forall s a. LookupMap s a -> s -> Maybe a
runLookupMap (LookupMap aa) = aa