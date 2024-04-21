
module Lambda.Predicate.Simplify(
                                 postOrderTraverseM, postOrderTraverse,
                                 simplify, simplifyConstrainedEquality, simplifyTerms,
                                 alphaRenameQuantified
                                ) where

-- Miscellaneous simplifications that can be applied to predicates.

import Lambda.Predicate (Predicate(..), substitute)
import Lambda.Term (Term(..))
import Lambda.Term.Simplify (simplify) as TermSimplify

import Control.Apply (lift2)
import Safe.Coerce (coerce)
import Data.Identity (Identity(..))
import Prelude

postOrderTraverseM :: forall m. Monad m => (Predicate -> m Predicate) -> Predicate -> m Predicate
postOrderTraverseM f = go
    where go x = recurse x >>= f
          recurse (Operator op a b) = pure (Operator op a b)
          recurse (Implies lhs rhs) = lift2 Implies (go lhs) (go rhs)
          recurse (And lhs rhs) = lift2 And (go lhs) (go rhs)
          recurse (Or lhs rhs) = lift2 Or (go lhs) (go rhs)
          recurse (Forall s ttype pred) = Forall s ttype <$> go pred

postOrderTraverse :: (Predicate -> Predicate) -> Predicate -> Predicate
postOrderTraverse f = coerce <<< postOrderTraverseM (Identity <<< f)

simplify :: Predicate -> Predicate
simplify = simplifyConstrainedEquality >>> simplifyTerms TermSimplify.simplify

simplifyConstrainedEquality :: Predicate -> Predicate
simplifyConstrainedEquality = postOrderTraverse go
    where go (Forall v _ (Implies (Operator "=" (Var v') value) result)) | v == v' = substitute v value result
          go (Forall v _ (Implies (Operator "=" value (Var v')) result)) | v == v' = substitute v value result
          go pred = pred

simplifyTerms :: (Term -> Term) -> Predicate -> Predicate
simplifyTerms termSimplifier = postOrderTraverse go
    where go (Operator op a b) = Operator op (termSimplifier a) (termSimplifier b)
          go x = x

-- Note: This function does NOT check whether or not it's shadowing
-- any other names. It's the caller's responsibility to make sure the
-- rename is sensible.
alphaRenameQuantified :: String -> String -> Predicate -> Predicate
alphaRenameQuantified old new = postOrderTraverse go
    where go (Forall v ttype body) | v == old = Forall new ttype (substitute v (Var new) body)
          go x = x
