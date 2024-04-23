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
module Lambda.Type.Kind(
                   TKind(..),
                   GroundKind(..),
                   toKind, toGroundKind
                  ) where

import Lambda.PrettyShow (class PrettyShow, prettyShow, parenthesizeIf)

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))

data TKind = Ty GroundKind -- Ground kind (Type, Constraint, etc)
           | KArrow TKind TKind -- Arrow kind (a -> b)

data GroundKind = GType | GConstraint

derive instance Eq TKind
derive instance Generic TKind _
derive instance Eq GroundKind
derive instance Generic GroundKind _

instance Show TKind where
    show k = genericShow k

instance PrettyShow TKind where
    prettyShow = prettyShowPrec defaultPrecedence

instance Show GroundKind where
    show k = genericShow k

instance PrettyShow GroundKind where
    prettyShow x = prettyShow (Ty x)

toKind :: GroundKind -> TKind
toKind = Ty

toGroundKind :: TKind -> Maybe GroundKind
toGroundKind (Ty g) = Just g
toGroundKind _ = Nothing

defaultPrecedence :: Int
defaultPrecedence = 0

arrowRightPrecedence :: Int
arrowRightPrecedence = 1

arrowLeftPrecedence :: Int
arrowLeftPrecedence = 2

prettyShowPrec :: Int -> TKind -> String
prettyShowPrec _ (Ty GType) = "Type"
prettyShowPrec _ (Ty GConstraint) = "Constraint"
prettyShowPrec n (KArrow left right) =
    let left' = prettyShowPrec arrowLeftPrecedence left
        right' = prettyShowPrec arrowRightPrecedence right in
    parenthesizeIf (n >= arrowLeftPrecedence) $ left' <> " -> " <> right'
