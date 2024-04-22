
module Lambda.MathShow(
                       class MathShow,
                       mathShow, escapeLatexText, texttt
                      ) where

import Prelude

import Data.String.Common (replace)
import Data.String.Pattern (Pattern(..), Replacement(..))

-- Like Data.Show but for production of LaTeX markup.
class MathShow a where
  mathShow :: a -> String

escapeLatexText :: String -> String
escapeLatexText = replace (Pattern "\\") (Replacement "\\textbackslash") >>>
                  replace (Pattern "{") (Replacement "\\{") >>>
                  replace (Pattern "}") (Replacement "\\}" )

texttt :: String -> String
texttt s = "\\texttt{" <> escapeLatexText s <> "}"
