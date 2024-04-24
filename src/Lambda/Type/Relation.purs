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
module Lambda.Type.Relation(
                            PredicateZipper, runPredicateZipper,
                            TermHole, Relation, TaggedRelation(..),
                            runRelation, identityRelation, getTagType, runTaggedRelation,
                            rImplies, rForall,
                            mapTerms,
                            describeRelation,
                            allVariablesWith, allVariables, allQuantifiedVariables,
                            substituteVar,
                            postOrderTraverseM, postOrderTraverse, alphaRenameQuantified,
                            renameConflicts,
                            zipRelationsWith, zipRelationsWith3, zipRelationsWith4, zipRelationsWith5
                           ) where

import Lambda.Util (unsafeFromRight, toList)
import Lambda.Predicate (Predicate(..))
import Lambda.Predicate (allVariables, allQuantifiedVariables, substitute) as Predicate
import Lambda.Predicate.Simplify (alphaRenameQuantified) as PredicateSimplify
import Lambda.Term (Term(..))
import Lambda.Term (allVariables, substitute) as Term
import Lambda.Type (TType(..))
import Lambda.Type (substitute) as Type
import Lambda.Type.Kind (GroundKind(..))
import Lambda.Type.Functions (class GroundKindInferrable, class NeverConstraint)
import Lambda.PrettyShow (prettyShow)
import Lambda.Monad.Names (freshStrings)
import Lambda.Util.InfiniteList (find) as InfiniteList

import Prelude
import Data.Bifunctor (class Bifunctor, bimap)
import Data.List (List(..), reverse, (:), zip)
import Data.Tuple (Tuple(..))
import Control.Biapply (class Biapply, biapply, bilift2, bilift3, (<<*>>))
import Control.Biapplicative (class Biapplicative, bipure)
import Safe.Coerce (coerce)
import Data.Identity (Identity(..))
import Data.Set (Set)
import Data.Set (insert, delete, intersection, member) as Set
import Data.Foldable (class Foldable, foldl, foldMap)
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (noFlags)

-- A Predicate with holes of types a1 and a2 in place of the terms.
data PredicateZipper a b = PEquals a b |
                           PImplies Predicate (PredicateZipper a b) | -- Note: Left-hand is still just Predicate for now
                           PForall String TType (PredicateZipper a b)

type TermHole = Term -> Term

-- A relation is defined to be a predicate quantified by two terms.
-- You can think of it as (Term -> Term -> Predicate) but more
-- restricted so we can introspect on the values.
type Relation = PredicateZipper TermHole TermHole

-- A Relation tagged with its original type.
data TaggedRelation = TaggedRelation TType Relation

-- All PredicateZippers are assumed to be of kind Type.
instance GroundKindInferrable (PredicateZipper a b) where
    getGroundKind _ = GType

instance NeverConstraint (PredicateZipper a b)

-- Note: The Bifunctor, Biapply, and Biapplicative instances might
-- look a little weird, but they do obey the corresponding typeclass
-- laws. Proof of conformance is available (in Agda) at
-- https://gist.github.com/Mercerenies/9c839c412a8b9324fb1c3085a2b41111

instance Bifunctor PredicateZipper where
    bimap f g (PEquals a b) = PEquals (f a) (g b)
    bimap f g (PImplies lhs rhs) = PImplies lhs (bimap f g rhs)
    bimap f g (PForall name ttype rhs) = PForall name ttype (bimap f g rhs)

instance Biapply PredicateZipper where
    biapply (PEquals f g) (PEquals a b) = PEquals (f a) (g b)
    biapply (PImplies lhs rhs) y = PImplies lhs (biapply rhs y)
    biapply (PForall name ttype rhs) y = PForall name ttype (biapply rhs y)
    biapply x (PImplies lhs rhs) = PImplies lhs (biapply x rhs)
    biapply x (PForall name ttype rhs) = PForall name ttype (biapply x rhs)

instance Biapplicative PredicateZipper where
    bipure = PEquals

runPredicateZipper :: forall a b. PredicateZipper a b -> (a -> Term) -> (b -> Term) -> Predicate
runPredicateZipper p f g = go p
    where go (PEquals a b) = Equals (f a) (g b)
          go (PImplies lhs rhs) = Implies lhs (go rhs)
          go (PForall name ttype rhs) = Forall name ttype (go rhs)

runRelation :: Relation -> Term -> Term -> Predicate
runRelation r a b = runPredicateZipper r (_ $ a) (_ $ b)

runTaggedRelation :: TaggedRelation -> Term -> Term -> Predicate
runTaggedRelation (TaggedRelation _ r) = runRelation r

getTagType :: TaggedRelation -> TType
getTagType (TaggedRelation ttype _) = ttype

identityRelation :: forall a b. PredicateZipper (a -> a) (b -> b)
identityRelation = PEquals identity identity

rImplies :: forall a b. Predicate -> PredicateZipper a b -> PredicateZipper a b
rImplies = PImplies

rForall :: forall a b. String -> TType -> PredicateZipper a b -> PredicateZipper a b
rForall = PForall

mapTerms :: forall a a' b b' c d. (a' -> a) -> (b' -> b) -> PredicateZipper (a -> c) (b -> d) -> PredicateZipper (a' -> c) (b' -> d)
mapTerms f g = bimap (_ <<< f) (_ <<< g)

describeRelation :: Relation -> Term -> Term -> String
describeRelation r left right = prettyShow (runRelation r left right)

-- (TODO Include variables in types here too)
allVariablesWith :: forall a b. (a -> Set String) -> (b -> Set String) -> (Predicate -> Set String) ->
                    PredicateZipper a b -> Set String
allVariablesWith fa fb fpred = go
    where go (PEquals a b) = fa a <> fb b
          go (PImplies lhs rhs) = fpred lhs <> go rhs
          go (PForall name _ rhs) = Set.insert name (go rhs)

allVariables :: Relation -> Set String
allVariables = allVariablesWith termVars termVars Predicate.allVariables
    where -- For term variables, we simply substitute `TVar "_"` in
          -- and check the resulting term. Our parser forbids the use
          -- of `TVar "_"` as a variable name, so we can safely remove
          -- it from the resutl.
          termVars termFunction = Set.delete "_" $ Term.allVariables (termFunction (Var "_"))

allQuantifiedVariables :: forall a b. PredicateZipper a b -> Set String
allQuantifiedVariables = allVariablesWith (const mempty) (const mempty) Predicate.allQuantifiedVariables

substituteVar :: String -> String -> Relation -> Relation
substituteVar x t = go
    where go (PEquals a b) = PEquals (Term.substitute x (Var t) <<< a) (Term.substitute x (Var t) <<< b)
          go (PImplies lhs rhs) = PImplies (Predicate.substitute x (Var t) lhs) (go rhs)
          go (PForall name ttype rhs)
              | name == x = PForall name ttype rhs
              | otherwise = PForall name (Type.substitute x (TVar t) ttype) $ go rhs

postOrderTraverseM :: forall a b m. Monad m => (PredicateZipper a b -> m (PredicateZipper a b)) ->
                      PredicateZipper a b -> m (PredicateZipper a b)
postOrderTraverseM f = go
    where go x = recurse x >>= f
          recurse (PEquals a b) = pure (PEquals a b)
          recurse (PImplies lhs rhs) = PImplies lhs <$> go rhs
          recurse (PForall name ttype rhs) = PForall name ttype <$> go rhs

postOrderTraverse :: forall a b. (PredicateZipper a b -> PredicateZipper a b) ->
                     PredicateZipper a b -> PredicateZipper a b
postOrderTraverse f = coerce <<< postOrderTraverseM (Identity <<< f)

-- Note: This function does NOT check whether or not it's shadowing
-- any other names. It's the caller's responsibility to make sure the
-- rename is sensible.
alphaRenameQuantified :: String -> String -> Relation -> Relation
alphaRenameQuantified old new = go
    where go (PForall v ttype body) | v == old = PForall new ttype $ substituteVar old new body
          go (PImplies lhs rhs) = PImplies (PredicateSimplify.alphaRenameQuantified old new lhs) rhs
          go x = x

-- Identifies any quantified variables in the final relation which are
-- also quantified in any of the left-hand relations, and then renames
-- them to something that does not appear, in any capacity, in any of
-- the relations.
renameConflicts :: forall f. Foldable f => f Relation -> Relation -> Relation
renameConflicts conflictingRelations relation =
      foldl (\r (Tuple oldVar newVar) -> alphaRenameQuantified oldVar newVar r) relation newVars
    where conflicts = toList $ foldMap allQuantifiedVariables conflictingRelations `Set.intersection` allQuantifiedVariables relation
          newVars = zip conflicts $ chooseNewVariables allVars conflicts
          allVars = foldMap allVariables conflictingRelations <> allVariables relation

chooseNewVariables :: Set String -> List String -> List String
chooseNewVariables usedVars xs = reverse $ chooseNewVariablesAcc usedVars xs Nil

chooseNewVariablesAcc :: Set String -> List String -> List String -> List String
chooseNewVariablesAcc _ Nil acc = acc
chooseNewVariablesAcc usedVars (x : xs) ys =
    let baseVarName = baseName x
        nameCandidates = freshStrings baseVarName
        newVarName = InfiniteList.find (\v -> not (v `Set.member` usedVars)) nameCandidates
    in chooseNewVariablesAcc (Set.insert newVarName usedVars) xs (newVarName : ys)

baseName :: String -> String
baseName s =
    let trailingDigitsRe = unsafeFromRight $ regex """\d+$""" noFlags in
    replace trailingDigitsRe "" s

zipRelationsWith :: (TermHole -> TermHole -> TermHole) -> (TermHole -> TermHole -> TermHole) ->
                    Relation -> Relation -> Relation
zipRelationsWith leftMap rightMap a b =
    let b' = renameConflicts [a] b in
    bilift2 leftMap rightMap a b'

zipRelationsWith3 :: (TermHole -> TermHole -> TermHole -> TermHole) ->
                     (TermHole -> TermHole -> TermHole -> TermHole) ->
                     Relation -> Relation -> Relation -> Relation
zipRelationsWith3 leftMap rightMap a b c =
    let b' = renameConflicts [a] b
        c' = renameConflicts [a, b'] c in
    bilift3 leftMap rightMap a b' c'

zipRelationsWith4 :: (TermHole -> TermHole -> TermHole -> TermHole -> TermHole) ->
                     (TermHole -> TermHole -> TermHole -> TermHole -> TermHole) ->
                     Relation -> Relation -> Relation -> Relation -> Relation
zipRelationsWith4 leftMap rightMap a b c d =
    let b' = renameConflicts [a] b
        c' = renameConflicts [a, b'] c
        d' = renameConflicts [a, b', c'] d in
    bipure leftMap rightMap <<*>> a <<*>> b' <<*>> c' <<*>> d'

-- Don't judge me, these go up to 5 arguments to support our tuple types.
zipRelationsWith5 :: (TermHole -> TermHole -> TermHole -> TermHole -> TermHole -> TermHole) ->
                     (TermHole -> TermHole -> TermHole -> TermHole -> TermHole -> TermHole) ->
                     Relation -> Relation -> Relation -> Relation -> Relation -> Relation
zipRelationsWith5 leftMap rightMap a b c d e =
    let b' = renameConflicts [a] b
        c' = renameConflicts [a, b'] c
        d' = renameConflicts [a, b', c'] d
        e' = renameConflicts [a, b', c', d'] e in
    bipure leftMap rightMap <<*>> a <<*>> b' <<*>> c' <<*>> d' <<*>> e'
