
module Lambda.Type.Parser where

import Lambda.Type (TType(..))
import Lambda.Util (fromChars, guarded)

import Prelude
import Parsing (ParserT)
import Parsing.Combinators (many, try, (<?>))
import Parsing.String (char)
import Parsing.String.Basic (letter, digit)
import Control.Apply (lift2)
import Control.Alt ((<|>))
import Data.List ((:))
import Data.Maybe (maybe)
import Data.String.CodePoints (codePointAt, codePointFromChar)
import Data.CodePoint.Unicode (isLower, isUpper)

identifier :: forall m. Monad m => ParserT String m String
identifier = fromChars <$> lift2 (:) (letter <|> char '_') (many (char '_' <|> letter <|> digit))

var :: forall m. Monad m => ParserT String m TType
var = TVar <$> try (guarded startsWithLowercase identifier) <?> "expected type variable"
    where startsWithLowercase = codePointAt 0 >>> maybe false isLower

groundTerm :: forall m. Monad m => ParserT String m TType
groundTerm = TGround <$> try (guarded startsWithLowercase identifier) <?> "expected ground type name"
    where startsWithLowercase = codePointAt 0 >>> maybe false (lift2 (||) isUpper (_ == underscore))
          underscore = codePointFromChar '_'
