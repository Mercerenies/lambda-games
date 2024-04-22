-- Copyright 2024 Silvio Mayolo
--
-- Lambdagames is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Lambdagames. If not, see
-- <https://www.gnu.org/licenses/>.
module Lambda.PrettyShow(
                         class PrettyShow,
                         prettyShow,
                         parenthesizeIf
                        ) where

import Prelude

-- Like Data.Show but pretty-printing for user consumption, rather
-- than readable Purescript code.
class PrettyShow a where
  prettyShow :: a -> String

-- Helper for pretty-printing functions that need to conditionally
-- parenthesize values, depending on current operator precedence.
parenthesizeIf :: Boolean -> String -> String
parenthesizeIf false v = v
parenthesizeIf true v = "(" <> v <> ")"
