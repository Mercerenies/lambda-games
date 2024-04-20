module Main where

import Prelude

import Lambda.Type (makeClosed)
import Lambda.Type.Parser (parseExpression)
import Lambda.Type.Free (describeFreeTheoremWith)
import Lambda.Type.Builtins (allBuiltins)
import Lambda.Predicate.Simplify (simplify)
import Effect (Effect)
import Effect.Console (log)
import Node.ReadLine (Interface, createConsoleInterface, noCompletion, prompt, setPrompt, lineH)
import Node.EventEmitter (on_)
import Data.Either (Either(..))
import Data.Bifunctor (lmap)

main :: Effect Unit
main = do
  interface <- makeReadlineInterface
  prompt interface

makeReadlineInterface :: Effect Interface
makeReadlineInterface = do
  interface <- createConsoleInterface noCompletion
  setPrompt "> " interface
  interface # on_ lineH \input -> do
    case parseAndDescribe input of
      Left err -> log $ "Error: " <> err
      Right desc -> log desc
    prompt interface
  pure interface

parseAndDescribe :: String -> Either String String
parseAndDescribe input = do
  ttype <- lmap show $ parseExpression input
  let ttype' = makeClosed ttype
  lmap show $ describeFreeTheoremWith simplify allBuiltins ttype'
