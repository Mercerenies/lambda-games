
module Lambda.Type.Kind(
                   TKind(..)
                  ) where

import Lambda.PrettyShow (class PrettyShow, parenthesizeIf)

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data TKind = Ty | KArrow TKind TKind

derive instance Eq TKind
derive instance Generic TKind _

instance Show TKind where
    show k = genericShow k

instance PrettyShow TKind where
    prettyShow = prettyShowPrec defaultPrecedence

defaultPrecedence :: Int
defaultPrecedence = 0

arrowRightPrecedence :: Int
arrowRightPrecedence = 1

arrowLeftPrecedence :: Int
arrowLeftPrecedence = 2

prettyShowPrec :: Int -> TKind -> String
prettyShowPrec _ Ty = "Type"
prettyShowPrec n (KArrow left right) =
    let left' = prettyShowPrec arrowLeftPrecedence left
        right' = prettyShowPrec arrowRightPrecedence right in
    parenthesizeIf (n >= arrowLeftPrecedence) $ left' <> " -> " <> right'
