
module Lambda.Type.Relation(
                            Relation(..), identityRelation,
                            relationify, relationifyWithBindings,
                            describeRelation, describeFreeTheorem
                           ) where

import Lambda.Type (TType(..), suggestedVariableName)
import Lambda.Predicate (Predicate(..))
import Lambda.Monad.Names (NamesT, withName, withFreshName, freshStrings, withName2, runNamesT)
import Lambda.Util.InfiniteList (InfiniteList, intersperse)
import Lambda.Term (Term(..))
import Lambda.PrettyShow (prettyShow)

import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..), (:))
import Data.NonEmpty ((:|))
import Data.Foldable (lookup)
import Data.Either (Either(..))
import Control.Monad.Trans.Class (lift)
import Prelude
import Effect.Exception.Unsafe (unsafeThrow)

newtype Relation = Relation (Term -> Term -> Predicate)

identityRelation :: Relation
identityRelation = Relation Equals

functionNames :: InfiniteList String
functionNames = intersperse (freshStrings "f" :| freshStrings "g" : freshStrings "h" : Nil)

-- Lift a closed type into a relation.
relationify :: TType -> NamesT String (Either String) Relation
relationify = relationifyWithBindings Nil

relationifyWithBindings :: List (Tuple String Relation) -> TType -> NamesT String (Either String) Relation
relationifyWithBindings bindings (TVar x)
    | Just rel <- lookup x bindings = pure rel
    | otherwise = lift $ Left $ "Unbound type variable: " <> x
relationifyWithBindings _ (TGround _) =
    -- For now, assume all ground types are just the identity and
    -- don't have more complex relations. We will update this later.
    pure identityRelation
relationifyWithBindings bindings (TArrow a b) = do
  withName2 (suggestedVariableName a) $ \a1 a2 -> do
    Relation ra <- relationifyWithBindings bindings a
    Relation rb <- relationifyWithBindings bindings b
    pure $ Relation \left right -> Forall a1 a $ Forall a2 a $
                                     ra (Var a1) (Var a2) `Implies` rb (App left (Var a1)) (App right (Var a2))
relationifyWithBindings _ (TContextArrow _ _) =
    unsafeThrow "Not supported yet"
relationifyWithBindings bindings (TForall x body) = do
  withName2 x \x1 x2 -> do
    withFreshName functionNames \f -> do
      let rel = Relation \left right -> Equals (App (Var f) left) right
      Relation innerRelation <- relationifyWithBindings (Tuple x rel : bindings) body
      pure $ Relation \left right -> Forall x1 (TVar "Type") $ Forall x2 (TVar "Type") $
                                       Forall f (TVar x1 `TArrow` TVar x2) $
                                         innerRelation (TypeApp left (Var x1)) (TypeApp right (Var x2))

describeRelation :: Relation -> Term -> Term -> NamesT String (Either String) String
describeRelation (Relation r) left right =
    pure $ prettyShow (r left right)

describeFreeTheorem :: TType -> Either String String
describeFreeTheorem t = runNamesT do
  withName (suggestedVariableName t) $ \a -> do
    r <- relationify t
    description <- describeRelation r (Var a) (Var a)
    pure $ a <> " ~ " <> a <> " if " <> description

-- Clean up the output a lot (simplify ∀ (b : A). something = b => ... to just directly using 'something' in place of b
-- Add product types, possibly sum types
-- Context types
-- Explore mu-recursive types
