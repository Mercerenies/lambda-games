
module Lambda.Term.Simplify(
                            postOrderTraverseM, postOrderTraverse,
                            simplify, simplifyReversedApp, simplifyEtaAbstraction
                           ) where

import Lambda.Term (Term(..), freeVariables)

import Prelude
import Safe.Coerce (coerce)
import Data.Identity (Identity(..))
import Data.Set (member) as Set

postOrderTraverseM :: forall m. Monad m => (Term -> m Term) -> Term -> m Term
postOrderTraverseM f = go
    where go x = recurse x >>= f
          recurse (Var s) = pure (Var s)
          recurse (App t1 t2) = App <$> go t1 <*> go t2
          recurse (TypeApp t1 t2) = TypeApp <$> go t1 <*> go t2
          recurse (Subscript t1 t2) = Subscript <$> go t1 <*> go t2
          recurse (OperatorSectionLeft s t) = OperatorSectionLeft s <$> go t
          recurse (OperatorSectionRight t s) = OperatorSectionRight <$> go t <*> pure s
          recurse (Fn x body) = Fn x <$> go body

postOrderTraverse :: (Term -> Term) -> Term -> Term
postOrderTraverse f = coerce <<< postOrderTraverseM (Identity <<< f)

simplify :: Term -> Term
simplify = simplifyReversedApp >>> simplifyEtaAbstraction

-- This oddly specific simplification will eliminate the unnecessary
-- ($ a) operator section (in favor of simpler syntax) when a function
-- is being applied at the top-level, as opposed to inside of a nested
-- data structure like a list.
simplifyReversedApp :: Term -> Term
simplifyReversedApp = postOrderTraverse $ case _ of
    App (OperatorSectionLeft "$" a) f -> App f a
    other -> other

simplifyEtaAbstraction :: Term -> Term
simplifyEtaAbstraction = postOrderTraverse $ case _ of
    Fn x (App f (Var x')) | x == x' && not (x `Set.member` freeVariables f) -> f
    other -> other
