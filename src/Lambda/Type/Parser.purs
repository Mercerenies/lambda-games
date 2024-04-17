
module Lambda.Type.Parser where

import Lambda.Type (TType(..))
import Lambda.Util (fromChars, guarded)

import Prelude
import Parsing (ParserT)
import Parsing.Combinators (many, try, (<?>))
import Parsing.String (char, string)
import Parsing.String.Basic (letter, digit, space, skipSpaces)
import Control.Apply (lift2)
import Control.Alt ((<|>))
import Data.List ((:))
import Data.Maybe (maybe)
import Data.String.CodePoints (codePointAt, codePointFromChar)
import Data.CodePoint.Unicode (isLower, isUpper)

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
      x <- varName
      skipSpaces
      char '.'
      skipSpaces
      expr <- expression
      in TForall x expr
    where forallQualifier = string "∀" <|> try (string "forall" <* space)

basicExpression :: forall m. Monad m => ParserT String m TType
basicExpression = var <|> groundTerm

expression :: forall m. Monad m => ParserT String m TType
expression = basicExpression
