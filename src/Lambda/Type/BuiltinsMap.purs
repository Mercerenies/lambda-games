
module Lambda.Type.BuiltinsMap(
                               BuiltinsMap(..), lookup,
                               Builtin(..),
                               fromLookupMap
                              ) where

import Lambda.LookupMap (LookupMap)
import Lambda.LookupMap (lookup) as LookupMap
import Lambda.Type.Relation (Relation)
import Lambda.Type.Functions (Lambda)
import Lambda.Util.InfiniteList (InfiniteList)

import Prelude
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Control.Alternative (alt, empty)

newtype BuiltinsMap :: (Type -> Type) -> Type
newtype BuiltinsMap m = BuiltinsMap (LookupMap String (Builtin m))

newtype Builtin :: (Type -> Type) -> Type
newtype Builtin m = Builtin {
      relation :: Lambda m Relation,
      nameStream :: InfiniteList String
    }

derive instance Newtype (BuiltinsMap m) _

instance Semigroup (BuiltinsMap m) where
    append (BuiltinsMap lookupMap1) (BuiltinsMap lookupMap2) =
        BuiltinsMap (lookupMap1 `alt` lookupMap2)

instance Monoid (BuiltinsMap m) where
    mempty = BuiltinsMap empty

lookup :: forall m. String -> BuiltinsMap m -> Maybe (Builtin m)
lookup s (BuiltinsMap lookupMap) = LookupMap.lookup s lookupMap

fromLookupMap :: forall m. LookupMap String (Builtin m) -> BuiltinsMap m
fromLookupMap = BuiltinsMap
