
module Lambda.Type.Tagged(
                          SimplyTagged(..), class TypeTagged, getTypeTag
                         ) where

import Lambda.Type (TType)

-- A value tagged with a type.
data SimplyTagged r = SimplyTagged TType r

class TypeTagged r where
    getTypeTag :: r -> TType

instance TypeTagged (SimplyTagged r) where
    getTypeTag (SimplyTagged t _) = t
