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
module Lambda.Type.LambdaContext(
                                 LambdaContextT(..),
                                 unLambdaContextT, runLambdaContextT
                                ) where

import Lambda.Monad.Names (NamesT, runNamesTWith, class MonadNames)
import Lambda.Type.LambdaContext.FreeTheoremEnv (FreeTheoremEnv)

import Data.List (List)
import Data.Newtype (class Newtype)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Prelude

-- The type that Lambda functions (at the type level) must run in (for
-- underlying monad m).
newtype LambdaContextT m a =
    LambdaContextT (ReaderT (FreeTheoremEnv (LambdaContextT m)) (NamesT String m) a)

derive instance Newtype (LambdaContextT m a) _
derive newtype instance Functor m => Functor (LambdaContextT m)
derive newtype instance Apply m => Apply (LambdaContextT m)
derive newtype instance Applicative m => Applicative (LambdaContextT m)
derive newtype instance Bind m => Bind (LambdaContextT m)
derive newtype instance Monad m => Monad (LambdaContextT m)
derive newtype instance MonadThrow e m => MonadThrow e (LambdaContextT m)
derive newtype instance MonadError e m => MonadError e (LambdaContextT m)
derive newtype instance Monad m => MonadAsk (FreeTheoremEnv (LambdaContextT m)) (LambdaContextT m)
derive newtype instance Monad m => MonadReader (FreeTheoremEnv (LambdaContextT m)) (LambdaContextT m)
derive newtype instance Monad m => MonadNames String (LambdaContextT m)

unLambdaContextT :: forall m a. LambdaContextT m a -> ReaderT (FreeTheoremEnv (LambdaContextT m)) (NamesT String m) a
unLambdaContextT (LambdaContextT x) = x

runLambdaContextT :: forall m a. Monad m =>
                     LambdaContextT m a -> FreeTheoremEnv (LambdaContextT m) -> List String -> m a
runLambdaContextT (LambdaContextT x) r reservedNames = runNamesTWith reservedNames (runReaderT x r)
