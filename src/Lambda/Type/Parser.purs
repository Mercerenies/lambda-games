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
module Lambda.Type.Parser(expressionParser, parseExpressionT, parseExpression) where

import Lambda.Type (TType(..))
import Lambda.Util (fromChars, guarded, unsafeFromJust)

import Prelude hiding (between)
import Parsing (ParserT, runParserT, ParseError, fail)
import Parsing.Combinators (many, try, (<?>), between, sepBy, sepEndBy1)
import Parsing.String (char, string, eof)
import Parsing.String.Basic (letter, digit, space, skipSpaces)
import Control.Apply (lift2)
import Control.Alt ((<|>))
import Control.Lazy (defer)
import Control.Monad.Rec.Class (class MonadRec)
import Data.List (List, (:), head)
import Data.List (singleton) as List
import Data.Array (notElem)
import Data.Foldable (foldr, foldl, length)
import Data.Semigroup.Foldable (foldl1)
import Data.Maybe (maybe)
import Data.String.CodePoints (codePointAt, codePointFromChar)
import Data.CodePoint.Unicode (isLower, isUpper)
import Data.Identity (Identity(..))
import Data.Either (Either)

identifier :: forall m. Monad m => ParserT String m String
identifier = try (guarded isNotKeyword body)
    where body = fromChars <$> lift2 (:) (letter <|> char '_') (many (char '_' <|> letter <|> digit))
          isNotKeyword s = s `notElem` ["forall", "_"]

varName :: forall m. Monad m => ParserT String m String
varName = try (guarded startsWithLowercase identifier) <?> "expected type variable"
    where startsWithLowercase = codePointAt 0 >>> maybe false isLower

var :: forall m. Monad m => ParserT String m TType
var = TVar <$> varName

groundTerm :: forall m. Monad m => ParserT String m TType
groundTerm = TGround <$> try (guarded startsWithUppercase identifier) <?> "expected ground type name"
    where startsWithUppercase = codePointAt 0 >>> maybe false (lift2 (||) isUpper (_ == underscore))
          underscore = codePointFromChar '_'

forallExpression :: forall m. Monad m => ParserT String m TType
forallExpression = ado
      forallQualifier
      skipSpaces
      xs <- many (varName <* skipSpaces)
      skipSpaces
      char '.'
      skipSpaces
      expr <- defer \_ -> expression
      in foldr TForall expr xs
    where forallQualifier = string "∀" <|> try (string "forall" <* space)

listExpression :: forall m. Monad m => ParserT String m TType
listExpression = TApp (TGround "List") <$>
                 between (char '[' *> skipSpaces) (skipSpaces <* char ']') (defer \_ -> expression)

tupleArgsToType :: forall m. Monad m => List TType -> ParserT String m TType
tupleArgsToType xs =
    case length xs of
      0 -> pure $ foldTupleApp "Tuple0" xs
      1 -> pure $ unsafeFromJust (head xs) -- safety: head must exist since length xs == 1
      2 -> pure $ foldTupleApp "Tuple2" xs
      3 -> pure $ foldTupleApp "Tuple3" xs
      4 -> pure $ foldTupleApp "Tuple4" xs
      5 -> pure $ foldTupleApp "Tuple5" xs
      _ -> fail "Sorry, only tuples of length up to 5 are currently supported"
  where foldTupleApp :: String -> List TType -> TType
        foldTupleApp s = foldl TApp (TGround s)

-- Unit tuples, individual parenthesized expressions, and proper
-- tuples (since they all parse the same way)
tupleLikeExpression :: forall m. Monad m => ParserT String m TType
tupleLikeExpression = do
    xs <- sepBy (defer \_ -> expression) (try $ skipSpaces *> char ',' <* skipSpaces)
    tupleArgsToType xs

commaSectionExpression :: forall m. Monad m => ParserT String m TType
commaSectionExpression = do
  _ <- try $ skipSpaces *> char ','
  commas <- many (try $ skipSpaces *> char ',')
  let commaCount = length commas + 1
  case commaCount of
    1 -> pure $ TGround "Tuple2"
    2 -> pure $ TGround "Tuple3"
    3 -> pure $ TGround "Tuple4"
    4 -> pure $ TGround "Tuple5"
    _ -> fail $ "Sorry, only tuples of length up to 5 are currently supported"

parenthesizedExpression :: forall m. Monad m => ParserT String m TType
parenthesizedExpression = commaSectionExpression <|> tupleLikeExpression

basicExpression :: forall m. Monad m => ParserT String m TType
basicExpression = var <|>
                  groundTerm <|>
                  forallExpression <|>
                  listExpression <|>
                  between (char '(' *> skipSpaces) (skipSpaces <* char ')') parenthesizedExpression

appExpression :: forall m. Monad m => ParserT String m TType
appExpression = foldl1 TApp <$> sepEndBy1 basicExpression skipSpaces

-- Regular arrows (->) parse their left-hand sides as tuples if there
-- are parens and commas. We could theoretically just let
-- parenthesizedExpression handle this case, but doing it here as well
-- lets us avoid having to have a massive lookahead on the parser to
-- identify the arrow type.
--
-- Context arrows (=>) unroll their left-hand side, so that (Eq a,
-- Monoid a) => a becomes Eq a => Monoid a => a. That is, we can still
-- write the syntax the Haskell way, but we represent contexts the
-- Purescript way, curried.
arrowType :: forall m. Monad m => ParserT String m (List TType -> TType -> ParserT String m TType)
arrowType = regularArrowType <$ (string "->" <|> string "→") <|>
            contextArrowType <$ (string "=>" <|> string "⇒")
    where regularArrowType xs rhs = (_ `TArrow` rhs) <$> tupleArgsToType xs
          contextArrowType xs rhs = pure $ foldr TContextArrow rhs xs

arrowLeftHandSide :: forall m. Monad m => ParserT String m (List TType)
arrowLeftHandSide = parenthesized <|> basic
    where parenthesized = between (char '(' *> skipSpaces) (skipSpaces <* char ')') $
                            sepBy (defer \_ -> expression) (try $ skipSpaces *> char ',' <* skipSpaces)
          basic = List.singleton <$> basicExpression

arrowExpression :: forall m. Monad m => ParserT String m TType
arrowExpression = do
      lhs <- arrowLeftHandSide
      skipSpaces
      ctor <- arrowType
      skipSpaces
      rhs <- defer \_ -> expression
      ctor lhs rhs

expression :: forall m. Monad m => ParserT String m TType
expression = try arrowExpression <|> appExpression

expressionParser :: forall m. Monad m => ParserT String m TType
expressionParser = between skipSpaces (skipSpaces *> eof) expression

parseExpressionT :: forall m. MonadRec m => String -> m (Either ParseError TType)
parseExpressionT s = runParserT s expressionParser

parseExpression :: String -> Either ParseError TType
parseExpression = parseExpressionT >>> unwrapId
    where unwrapId (Identity x) = x
