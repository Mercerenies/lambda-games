
module Lambda.Type(
                   TType(..),
                   substitute, freeVariables, isClosed,
                   prettyShow,
                   suggestedVariableName
                  ) where

import Lambda.Util(parenthesizeIf)

import Prelude
import Data.List (List(..), delete, singleton, null)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Common (toLower)

data TType = TVar String
           | TGround String
           | TArrow TType TType
           | TContextArrow TType TType
           | TForall String TType

derive instance Eq TType
derive instance Generic TType _

instance showTType :: Show TType where
    show t = genericShow t

substitute :: String -> TType -> TType -> TType
substitute x t (TVar y) | x == y = t
                        | otherwise = TVar y
substitute _ _ (TGround g) = TGround g
substitute x t (TArrow t1 t2) = TArrow (substitute x t t1) (substitute x t t2)
substitute x t (TContextArrow t1 t2) = TContextArrow (substitute x t t1) (substitute x t t2)
substitute x t (TForall y t1) | x == y = TForall y t1
                              | otherwise = TForall y (substitute x t t1)

freeVariables :: TType -> List String
freeVariables (TVar x) = singleton x
freeVariables (TGround _) = Nil
freeVariables (TArrow t1 t2) = freeVariables t1 <> freeVariables t2
freeVariables (TContextArrow t1 t2) = freeVariables t1 <> freeVariables t2
freeVariables (TForall x t) = delete x $ freeVariables t

isClosed :: TType -> Boolean
isClosed = null <<< freeVariables

prettyShow :: TType -> String
prettyShow = prettyShowPrec defaultPrecedence

defaultPrecedence :: Int
defaultPrecedence = 0

forallPrecedence :: Int
forallPrecedence = 1

arrowRightPrecedence :: Int
arrowRightPrecedence = 2

arrowLeftPrecedence :: Int
arrowLeftPrecedence = 3

prettyShowPrec :: Int -> TType -> String
prettyShowPrec _ (TVar x) = x
prettyShowPrec _ (TGround x) = x
prettyShowPrec n (TArrow left right) =
    let left' = prettyShowPrec arrowLeftPrecedence left
        right' = prettyShowPrec arrowRightPrecedence right in
    parenthesizeIf (n >= arrowLeftPrecedence) $ left' <> " -> " <> right'
prettyShowPrec n (TContextArrow left right) =
    let left' = prettyShowPrec arrowLeftPrecedence left
        right' = prettyShowPrec arrowRightPrecedence right in
    parenthesizeIf (n >= arrowLeftPrecedence) $ left' <> " => " <> right'
prettyShowPrec n (TForall v x) =
    let x' = prettyShowPrec forallPrecedence x in
    parenthesizeIf (n >= arrowLeftPrecedence) $ "∀" <> v <> ". " <> x'

-- A helpful variable name for a variable of the given type. This is
-- purely a heuristic meant to produce more user-friendly output.
suggestedVariableName :: TType -> String
suggestedVariableName (TVar s) = s
suggestedVariableName (TGround s) = suggestedGroundVariableName s
suggestedVariableName (TArrow _ _) = "f" -- short for "function"
suggestedVariableName (TContextArrow _ rhs) = suggestedVariableName rhs
suggestedVariableName (TForall _ t) = suggestedVariableName t

suggestedGroundVariableName :: String -> String
suggestedGroundVariableName "Int" = "n"
suggestedGroundVariableName "Bool" = "b"
suggestedGroundVariableName "Float" = "f"
suggestedGroundVariableName "String" = "s"
suggestedGroundVariableName s = toLower s
