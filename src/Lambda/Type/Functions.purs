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
                             LambdaF(..), TaggedLambdaF(..), KleisliEndo, Lambda, TaggedLambda,
                             class LambdaTag, tagApply, getLambdaTag, getLambdaFromTagged,
                             class GroundKindInferrable, class NeverConstraint, getGroundKind,
                             getKind, assertKind, expectFunction, expectGround
                            ) where

import Lambda.Type (TType(..))
import Lambda.Type.Kind (TKind(..), GroundKind)
import Lambda.Type.Error (class FromKindError, kindError)
import Lambda.Recursion (Mu)

import Prelude
import Control.Monad.Error.Class (class MonadError, throwError)

-- Very simple lambda calculus built on top of the type system, so we
-- can define type-level operators.
data LambdaF m r a = Ground r | Function { domain :: TKind, codomain :: TKind, body :: KleisliEndo m a }

data TaggedLambdaF t m r a = TaggedLambdaF t (LambdaF m r a)

type Lambda m r = LambdaF m r (Mu (LambdaF m r))

type TaggedLambda t m r = TaggedLambdaF t m r (Mu (TaggedLambdaF t m r))

type KleisliEndo m a = a -> m a

class GroundKindInferrable a where
    getGroundKind :: a -> GroundKind

-- Typeclass with no methods. If a type implements NeverConstraint,
-- then getGroundKind must NEVER return 'Ty GConstraint' for that
-- type. This assertion is NOT checked.
class GroundKindInferrable a <= NeverConstraint a

-- Types that can be used as a tag for a TaggedLambda. For a type to
-- fully support TaggedLambda, it must have an "apply" operation that
-- results from applying one to another. The usual example of this is
-- TType, which has TApp.
class LambdaTag t where
    tagApply :: t -> t -> t

instance LambdaTag TType where
    tagApply = TApp

getLambdaTag :: forall t m r a. TaggedLambdaF t m r a -> t
getLambdaTag (TaggedLambdaF t _) = t

getLambdaFromTagged :: forall t m r a. LambdaTag t => TaggedLambdaF t m r a -> LambdaF m r a
getLambdaFromTagged (TaggedLambdaF _ lam) = lam

getKind :: forall m r a. GroundKindInferrable r => LambdaF m r a -> TKind
getKind (Ground g) = Ty $ getGroundKind g
getKind (Function { domain, codomain }) = domain `KArrow` codomain

assertKind :: forall e m. FromKindError e => MonadError e m => TKind -> TKind -> m Unit
assertKind expected actual
    | expected == actual = pure unit
    | otherwise = throwError $ kindError { expected, actual }

expectFunction :: forall e m r a. FromKindError e => MonadError e m => GroundKindInferrable r =>
                  TKind -> TKind -> LambdaF m r a -> m (KleisliEndo m a)
expectFunction domain codomain =
    case _ of
      Ground g -> throwError $ kindError {
                    expected: domain `KArrow` codomain,
                    actual: Ty $ getGroundKind g
                  }
      f @ (Function { body }) -> body <$ assertKind (getKind f) (domain `KArrow` codomain)

expectGround :: forall e m r a. FromKindError e => MonadError e m => GroundKindInferrable r =>
                GroundKind -> LambdaF m r a -> m r
expectGround expectedKind (Ground r) = r <$ assertKind (Ty expectedKind) (Ty $ getGroundKind r)
expectGround expectedKind f = throwError $ kindError {
                                expected: Ty expectedKind,
                                actual: getKind f
                              }
