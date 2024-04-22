
module Lambda.Type.Functions.Factory(
                                     lambda0, lambda1, lambda2,
                                     Mono, runMono, mono,
                                     class MonoLambda, monoLambda, monoKind
                                    ) where

-- Helpers for producing Lambdas of various function types.

import Lambda.Type.Functions (Lambda(..), expectGround)
import Lambda.Type.Error (class FromKindError)
import Lambda.Type.Kind (TKind(..))

import Prelude
import Data.Identity (Identity(..))
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Control.Monad.Error.Class (class MonadError)

-- Helper for producing lambdas of kind Type
lambda0 :: forall e m r. FromKindError e => MonadError e m => r -> Lambda m r
lambda0 f = monoLambda $ mono f

-- Helper for producing lambdas of kind (Type -> Type)
lambda1 :: forall e m r. FromKindError e => MonadError e m => (r -> r) -> Lambda m r
lambda1 f = monoLambda \a -> mono (f a)

-- Helper for producing lambdas of kind (Type -> Type -> Type)
lambda2 :: forall e m r. FromKindError e => MonadError e m => (r -> r -> r) -> Lambda m r
lambda2 f = monoLambda \a b -> mono (f a b)

-- Helper for producing lambdas which take arbitrary numbers of
-- arguments, all of which are of kind Type. e.g. (Type -> Type),
-- (Type -> Type -> Type), etc.

newtype Mono r = Mono (Identity r) -- Identity lets us derive Monad and its superclasses, as opposed to just r.

derive newtype instance Functor Mono
derive newtype instance Apply Mono
derive newtype instance Applicative Mono
derive newtype instance Bind Mono
derive newtype instance Monad Mono

runMono :: forall r. Mono r -> r
runMono = coerce

mono :: forall r. r -> Mono r
mono = coerce

class MonoLambda x r | x -> r where
    monoLambda :: forall e m. FromKindError e => MonadError e m => x -> Lambda m r
    monoKind :: Proxy x -> TKind

instance MonoLambda (Mono r) r where
    monoLambda = Ground <<< runMono
    monoKind _ = Ty

instance MonoLambda x r => MonoLambda (r -> x) r where
    monoLambda f = Function {
                     domain: Ty,
                     codomain: monoKind (Proxy :: Proxy x),
                     body: \a -> (monoLambda <<< f) <$> expectGround a
                   }
    monoKind _ = Ty `KArrow` monoKind (Proxy :: Proxy x)