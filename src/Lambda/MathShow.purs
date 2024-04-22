
module Lambda.MathShow(
                       class MathShow, Latex(..),
                       mathShow, escapeLatexText, texttt, parenthesizeIf
                      ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Common (replace)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Newtype (class Newtype)

-- Like Data.Show but for production of LaTeX markup.
class MathShow a where
  mathShow :: a -> Latex

newtype Latex = Latex String

derive instance Newtype Latex _
derive instance Eq Latex
derive instance Generic Latex _
derive newtype instance Semigroup Latex
derive newtype instance Monoid Latex

instance Show Latex where
    show x = genericShow x

escapeLatexText :: String -> Latex
escapeLatexText = replace (Pattern "\\") (Replacement "\\textbackslash") >>>
                  replace (Pattern "{") (Replacement "\\{") >>>
                  replace (Pattern "}") (Replacement "\\}" ) >>>
                  Latex

texttt :: String -> Latex
texttt s = Latex "\\texttt{" <> escapeLatexText s <> Latex "}"

parenthesizeIf :: Boolean -> Latex -> Latex
parenthesizeIf false v = v
parenthesizeIf true v = Latex "\\left(" <> v <> Latex "\\right)"
