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
module Lambda.Term.Simplify(
                            postOrderTraverseM, postOrderTraverse,
                            simplify, simplifyReversedApp, simplifyEtaAbstraction,
                            simplifyIdentityApp, simplifyFmapId, simplifySplitId,
                            simplifyChoiceSplitId
                           ) where

import Lambda.Term (Term(..), freeVariables, Pattern(..))

import Prelude
import Safe.Coerce (coerce)
import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (length, all)
import Data.Traversable (traverse)
import Data.List (zip)
import Data.Set (member) as Set

postOrderTraverseM :: forall m. Monad m => (Term -> m Term) -> Term -> m Term
postOrderTraverseM f = go
    where go x = recurse x >>= f
          recurse (Var s) = pure (Var s)
          recurse (App t1 t2) = App <$> go t1 <*> go t2
          recurse (OperatorFunction o) = pure (OperatorFunction o)
          recurse (OperatorSectionLeft s t) = OperatorSectionLeft s <$> go t
          recurse (OperatorSectionRight t s) = OperatorSectionRight <$> go t <*> pure s
          recurse (OperatorApp a o b) = OperatorApp <$> go a <*> pure o <*> go b
          recurse (Fn x body) = Fn x <$> go body
          recurse (PatternFn p body) = PatternFn p <$> go body
          recurse (TupleTerm ts) = TupleTerm <$> traverse go ts

postOrderTraverse :: (Term -> Term) -> Term -> Term
postOrderTraverse f = coerce <<< postOrderTraverseM (Identity <<< f)

simplify :: Term -> Term
simplify = simplifyReversedApp >>> simplifyEtaAbstraction >>> simplifyFmapId >>>
           simplifySplitId >>> simplifyChoiceSplitId >>>simplifyIdentityApp

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

simplifyIdentityApp :: Term -> Term
simplifyIdentityApp = postOrderTraverse $ case _ of
    App func body | isIdentityFunction func -> body
    other -> other

simplifyFmapId :: Term -> Term
simplifyFmapId = postOrderTraverse $ case _ of
    App (Var "fmap") func | isIdentityFunction func -> func
    other -> other

simplifySplitId :: Term -> Term
simplifySplitId = postOrderTraverse $ case _ of
    OperatorApp f "***" g
        | isIdentityFunction f && isIdentityFunction g -> f
        | isIdentityFunction f -> Var "second" `App` g
        | isIdentityFunction g -> Var "first" `App` f
    other -> other

simplifyChoiceSplitId :: Term -> Term
simplifyChoiceSplitId = postOrderTraverse $ case _ of
    OperatorApp f "+++" g
        | isIdentityFunction f && isIdentityFunction g -> f
        | isIdentityFunction f -> Var "right" `App` g
        | isIdentityFunction g -> Var "left" `App` f
    other -> other

isIdentityFunction :: Term -> Boolean
isIdentityFunction (Fn x (Var x')) | x == x' = true
isIdentityFunction (PatternFn pat body) = matches pat body
    where matches (VarPattern p) (Var s) = p == s
          matches (TuplePattern ps) (TupleTerm xs) =
              (length ps :: Int) == length xs && all (\(Tuple p x) -> matches p x) (zip ps xs)
          matches _ _ = false
isIdentityFunction _ = false
