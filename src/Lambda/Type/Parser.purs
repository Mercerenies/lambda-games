
module Lambda.Type.Parser(expressionParser, parseExpressionT, parseExpression) where

import Lambda.Type (TType(..))
import Lambda.Util (fromChars, guarded)

import Prelude hiding (between)
import Parsing (ParserT, runParserT, ParseError)
import Parsing.Combinators (many, try, (<?>), between)
import Parsing.String (char, string, eof)
import Parsing.String.Basic (letter, digit, space, skipSpaces)
import Control.Apply (lift2)
import Control.Alt ((<|>))
import Control.Lazy (defer)
import Control.Monad.Rec.Class (class MonadRec)
import Data.List ((:))
import Data.Foldable (foldr)
import Data.Maybe (maybe)
import Data.String.CodePoints (codePointAt, codePointFromChar)
import Data.CodePoint.Unicode (isLower, isUpper)
import Data.Identity (Identity(..))
import Data.Either (Either)

identifier :: forall m. Monad m => ParserT String m String
identifier = fromChars <$> lift2 (:) (letter <|> char '_') (many (char '_' <|> letter <|> digit))

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

basicExpression :: forall m. Monad m => ParserT String m TType
basicExpression = var <|> groundTerm <|> forallExpression <|> between (char '(') (char ')') (defer \_ -> expression)

arrowExpression :: forall m. Monad m => ParserT String m TType
arrowExpression = ado
      lhs <- basicExpression
      skipSpaces
      ctor <- arrowType
      skipSpaces
      rhs <- defer \_ -> expression
      in ctor lhs rhs
    where arrowType = TArrow <$ (string "->" <|> string "→") <|>
                      TContextArrow <$ (string "=>" <|> string "⇒")

expression :: forall m. Monad m => ParserT String m TType
expression = try arrowExpression <|> basicExpression

expressionParser :: forall m. Monad m => ParserT String m TType
expressionParser = between skipSpaces (skipSpaces *> eof) expression

parseExpressionT :: forall m. MonadRec m => String -> m (Either ParseError TType)
parseExpressionT s = runParserT s expressionParser

parseExpression :: String -> Either ParseError TType
parseExpression = parseExpressionT >>> unwrapId
    where unwrapId (Identity x) = x
