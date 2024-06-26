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
module Lambda.Type(
                   TType(..),
                   substitute, freeVariables, isClosed, makeClosed,
                   functionNames, suggestedVariableName
                  ) where

import Lambda.PrettyShow (class PrettyShow, prettyShow, parenthesizeIf)
import Lambda.Util (first)
import Lambda.Util.InfiniteList (InfiniteList)
import Lambda.Monad.Names (interspersedStrings, freshStrings)

import Prelude
import Data.List (List(..), (:), singleton, null, filter, nub)
import Data.Tuple (Tuple(..))
import Data.Foldable (foldr, intercalate, length)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.NonEmpty ((:|))
import Control.Lazy (defer)
import Control.Apply (lift2)
import Control.Monad.Gen (sized, resize, oneOf, choose)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary, arbitrary, genericCoarbitrary)

data TType = TVar String
           | TGround String
           | TApp TType TType
           | TArrow TType TType
           | TContextArrow TType TType
           | TForall String TType

derive instance Eq TType
derive instance Generic TType _

instance showTType :: Show TType where
    show t = genericShow t

instance Arbitrary TType where
    arbitrary = resize (min 5) genTType

instance Coarbitrary TType where
    coarbitrary a gen = genericCoarbitrary a gen

substitute :: String -> TType -> TType -> TType
substitute x t (TVar y) | x == y = t
                        | otherwise = TVar y
substitute _ _ (TGround g) = TGround g
substitute x t (TArrow t1 t2) = TArrow (substitute x t t1) (substitute x t t2)
substitute x t (TApp t1 t2) = TApp (substitute x t t1) (substitute x t t2)
substitute x t (TContextArrow t1 t2) = TContextArrow (substitute x t t1) (substitute x t t2)
substitute x t (TForall y t1) | x == y = TForall y t1
                              | otherwise = TForall y (substitute x t t1)

freeVariables :: TType -> List String
freeVariables (TVar x) = singleton x
freeVariables (TGround _) = Nil
freeVariables (TArrow t1 t2) = nub $ freeVariables t1 <> freeVariables t2
freeVariables (TApp t1 t2) = nub $ freeVariables t1 <> freeVariables t2
freeVariables (TContextArrow t1 t2) = nub $ freeVariables t1 <> freeVariables t2
freeVariables (TForall x t) = filter (_ /= x) $ freeVariables t

isClosed :: TType -> Boolean
isClosed = null <<< freeVariables

makeClosed :: TType -> TType
makeClosed ttype =
    let free = freeVariables ttype in
    foldr TForall ttype free

instance PrettyShow TType where
    prettyShow = prettyShowPrec defaultPrecedence

defaultPrecedence :: Int
defaultPrecedence = 0

forallPrecedence :: Int
forallPrecedence = 1

arrowRightPrecedence :: Int
arrowRightPrecedence = 2

arrowLeftPrecedence :: Int
arrowLeftPrecedence = 3

appLeftPrecedence :: Int
appLeftPrecedence = 4

appRightPrecedence :: Int
appRightPrecedence = 5

prettyShowPrec :: Int -> TType -> String
prettyShowPrec _ (TVar x) = x
prettyShowPrec _ (TApp (TGround "List") right) =
    -- Special case to pretty-print list types
    "[" <> prettyShowPrec defaultPrecedence right <> "]"
prettyShowPrec _ (TGround "Tuple0") = "()"
prettyShowPrec _ (TApp (TApp (TGround "Tuple2") x) y) =
    let x' = prettyShowPrec defaultPrecedence x
        y' = prettyShowPrec defaultPrecedence y in
    "(" <> x' <> ", " <> y' <> ")"
prettyShowPrec _ (TApp (TApp (TApp (TGround "Tuple3") x) y) z) =
    let x' = prettyShowPrec defaultPrecedence x
        y' = prettyShowPrec defaultPrecedence y
        z' = prettyShowPrec defaultPrecedence z in
    "(" <> x' <> ", " <> y' <> ", " <> z' <> ")"
prettyShowPrec _ (TApp (TApp (TApp (TApp (TGround "Tuple4") x) y) z) u) =
    let x' = prettyShowPrec defaultPrecedence x
        y' = prettyShowPrec defaultPrecedence y
        z' = prettyShowPrec defaultPrecedence z
        u' = prettyShowPrec defaultPrecedence u in
    "(" <> x' <> ", " <> y' <> ", " <> z' <> ", " <> u' <> ")"
prettyShowPrec _ (TApp (TApp (TApp (TApp (TApp (TGround "Tuple5") x) y) z) u) v) =
    let x' = prettyShowPrec defaultPrecedence x
        y' = prettyShowPrec defaultPrecedence y
        z' = prettyShowPrec defaultPrecedence z
        u' = prettyShowPrec defaultPrecedence u
        v' = prettyShowPrec defaultPrecedence v in
    "(" <> x' <> ", " <> y' <> ", " <> z' <> ", " <> u' <> ", " <> v' <> ")"
prettyShowPrec _ (TGround x) = x
prettyShowPrec n (TApp left right) =
    let left' = prettyShowPrec appLeftPrecedence left
        right' = prettyShowPrec appRightPrecedence right in
    parenthesizeIf (n >= appRightPrecedence) $ left' <> " " <> right'
prettyShowPrec n (TArrow left right) =
    let left' = prettyShowPrec arrowLeftPrecedence left
        right' = prettyShowPrec arrowRightPrecedence right in
    parenthesizeIf (n >= arrowLeftPrecedence) $ left' <> " -> " <> right'
prettyShowPrec n (TContextArrow left right) =
    parenthesizeIf (n >= arrowLeftPrecedence) $ prettyShowContexts left right
prettyShowPrec n (TForall v x) =
    let x' = prettyShowPrec forallPrecedence x in
    parenthesizeIf (n >= arrowLeftPrecedence) $ "∀ " <> v <> ". " <> x'

prettyShowContexts :: TType -> TType -> String
prettyShowContexts left right =
    let Tuple allContexts right' = first (left :| _) $ collectContexts right
        leftHandSide = parenthesizeIf (length allContexts > 1) $ intercalate ", " $ map prettyShow allContexts
        rightHandSide = prettyShow right' in
    leftHandSide <> " => " <> rightHandSide

collectContexts :: TType -> Tuple (List TType) TType
collectContexts (TContextArrow left right) = first (left : _) $ collectContexts right
collectContexts t = Tuple Nil t

functionNames :: InfiniteList String
functionNames = interspersedStrings ("f" :| "g" : "h" : Nil)

-- A helpful variable name stream for a variable of the given type.
-- This is purely a heuristic meant to produce more user-friendly
-- output.
suggestedVariableName :: (String -> InfiniteList String) -> TType -> InfiniteList String
suggestedVariableName groundNames = go
    where go (TVar s) = freshStrings s
          go (TGround s) = groundNames s
          go (TApp left _) = go left
          go (TArrow _ _) = functionNames
          go (TContextArrow _ rhs) = go rhs
          go (TForall _ t) = go t

genTType :: Gen TType
genTType = genTType'
    where genTType'' size
              | size > 1 = resize (_ - 1) (oneOf (genArrow :| genContextArrow : genForall : Nil))
              | otherwise = genVar `choose` genGround
          genTType' = sized \size -> genTType'' size
          genArrow = defer \_ -> lift2 TArrow genTType' genTType'
          genContextArrow = defer \_ -> lift2 TContextArrow genTType' genTType'
          genForall = defer \_ -> lift2 TForall arbitrary genTType'
          genVar = defer \_ -> TVar <$> arbitrary
          genGround = defer \_ -> TGround <$> arbitrary
