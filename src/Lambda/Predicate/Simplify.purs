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
          recurse (Equals a b) = pure (Equals a b)
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
    where go (Forall v _ (Implies (Equals (Var v') value) result)) | v == v' = substitute v value result
          go (Forall v _ (Implies (Equals value (Var v')) result)) | v == v' = substitute v value result
          go pred = pred

simplifyTerms :: (Term -> Term) -> Predicate -> Predicate
simplifyTerms termSimplifier = postOrderTraverse go
    where go (Equals a b) = Equals (termSimplifier a) (termSimplifier b)
          go x = x

-- Note: This function does NOT check whether or not it's shadowing
-- any other names. It's the caller's responsibility to make sure the
-- rename is sensible.
alphaRenameQuantified :: String -> String -> Predicate -> Predicate
alphaRenameQuantified old new = postOrderTraverse go
    where go (Forall v ttype body) | v == old = Forall new ttype (substitute v (Var new) body)
          go x = x
