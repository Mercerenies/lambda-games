
module Lambda.Type.BuiltinsMap(
                               BuiltinsMap(..), lookup
                              ) where

import Lambda.LookupMap (LookupMap)
import Lambda.LookupMap (lookup) as LookupMap
import Lambda.Type.Relation (Relation)
import Lambda.Type.Functions (Lambda)

import Prelude
import Data.Maybe (Maybe)
import Control.Alternative (alt, empty)

data BuiltinsMap m = BuiltinsMap {
      lookupMap :: LookupMap String (Lambda m Relation)
    }

instance Semigroup (BuiltinsMap m) where
    append (BuiltinsMap { lookupMap: lookupMap1 }) (BuiltinsMap { lookupMap: lookupMap2 }) =
        BuiltinsMap {
            lookupMap: lookupMap1 `alt` lookupMap2
        }

instance Monoid (BuiltinsMap m) where
    mempty = BuiltinsMap { lookupMap: empty }

lookup :: forall m. String -> BuiltinsMap m -> Maybe (Lambda m Relation)
lookup s (BuiltinsMap { lookupMap }) = LookupMap.lookup s lookupMap
