
module Main(main) where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Lambda.Module (parseAndDescribe)
import Node.ReadLine (Interface, createConsoleInterface, noCompletion, prompt, setPrompt, lineH)
import Node.EventEmitter (on_)
import Data.Either (Either(..))

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
