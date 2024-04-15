
module Lambda.Util(
                   parenthesizeIf
                  ) where

import Prelude

-- Helper for pretty-printing functions that need to conditionally
-- parenthesize values, depending on current operator precedence.
parenthesizeIf :: Boolean -> String -> String
parenthesizeIf false v = v
parenthesizeIf true v = "(" <> v <> ")"
