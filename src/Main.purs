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
  showLicenseNotice
  interface <- makeReadlineInterface
  prompt interface

showLicenseNotice :: Effect Unit
showLicenseNotice = do
  log "Lambdagames Copyright 2024 Silvio Mayolo"
  log "This program comes with ABSOLUTELY NO WARRANTY."
  log "This is free software, and you are welcome to redistribute it"
  log "under certain conditions; see COPYING for details."

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
