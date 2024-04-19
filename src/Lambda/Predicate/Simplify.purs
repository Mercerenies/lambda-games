
module Lambda.Predicate.Simplify(
                                 postOrderTraverseM, postOrderTraverse,
                                 simplify, simplifyConstrainedEquality
                                ) where

-- Miscellaneous simplifications that can be applied to predicates.

import Lambda.Predicate (Predicate(..), substitute)
import Lambda.Term (Term(..))

import Control.Apply (lift2)
import Safe.Coerce (coerce)
import Data.Identity (Identity(..))
import Prelude

postOrderTraverseM :: forall m. Monad m => (Predicate -> m Predicate) -> Predicate -> m Predicate
postOrderTraverseM f = go
    where go x = recurse x >>= f
          recurse (Equals a b) = pure (Equals a b)
          recurse (Implies lhs rhs) = lift2 Implies (go lhs) (go rhs)
          recurse (And lhs rhs) = lift2 And (go lhs) (go rhs)
          recurse (Forall s ttype pred) = Forall s ttype <$> go pred

postOrderTraverse :: (Predicate -> Predicate) -> Predicate -> Predicate
postOrderTraverse f = coerce <<< postOrderTraverseM (Identity <<< f)

simplify :: Predicate -> Predicate
simplify = simplifyConstrainedEquality

simplifyConstrainedEquality :: Predicate -> Predicate
simplifyConstrainedEquality = postOrderTraverse go
    where go (Forall v _ (Implies (Equals (Var v') value) result)) | v == v' = substitute v value result
          go (Forall v _ (Implies (Equals value (Var v')) result)) | v == v' = substitute v value result
          go pred = pred
