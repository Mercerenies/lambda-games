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
module Lambda.Type.Functions(
                             Lambda(..), LambdaFunction,
                             getKind, assertKind, expectFunction, expectGround
                            ) where

import Lambda.Type.Kind (TKind(..))
import Lambda.Type.Error (class FromKindError, kindError)

import Prelude
import Control.Monad.Error.Class (class MonadError, throwError)

-- Very simple lambda calculus built on top of the type system, so we
-- can define type-level operators.
data Lambda m r = Ground r | Function { domain :: TKind, codomain :: TKind, body :: LambdaFunction m r }

type LambdaFunction m r = Lambda m r -> m (Lambda m r)

getKind :: forall m r. Lambda m r -> TKind
getKind (Ground _) = Ty
getKind (Function { domain, codomain }) = domain `KArrow` codomain

assertKind :: forall e m. FromKindError e => MonadError e m => TKind -> TKind -> m Unit
assertKind expected actual
    | expected == actual = pure unit
    | otherwise = throwError $ kindError { expected, actual }

expectFunction :: forall e m r. FromKindError e => MonadError e m => TKind -> TKind -> Lambda m r -> m (LambdaFunction m r)
expectFunction domain codomain =
    case _ of
      Ground _ -> throwError $ kindError {
                    expected: domain `KArrow` codomain,
                    actual: Ty
                  }
      f @ (Function { body }) -> body <$ assertKind (getKind f) (domain `KArrow` codomain)

expectGround :: forall e m r. FromKindError e => MonadError e m => Lambda m r -> m r
expectGround (Ground r) = pure r
expectGround f = throwError $ kindError {
                    expected: Ty,
                    actual: getKind f
                  }
