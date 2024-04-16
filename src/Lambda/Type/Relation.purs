
module Lambda.Type.Relation(
                            Relation(..), identityRelation,
                            relationify, relationifyWithBindings,
                            describeRelation, describeFreeTheorem
                           ) where

import Lambda.Type (TType(..), suggestedVariableName)
import Lambda.Predicate (Predicate(..))
import Lambda.Monad.Names (NamesT, withName, withName2, runNamesT)
import Lambda.Term (Term(..))
import Lambda.PrettyShow (prettyShow)

import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.List (List(..), (:))
import Data.Foldable (lookup)
import Data.Either (Either(..))
import Control.Monad.Trans.Class (lift)
import Prelude
import Effect.Exception.Unsafe (unsafeThrow)

data Relation =
    Relation (Term -> Term -> Predicate) |
    -- A relation on functions which holds if it maps related elements
    -- to related elements.
    FunctionRelation { domainType :: TType, domainRelation :: Relation, codomainRelation :: Relation } |
    -- A relation that holds for all sub-relations.
    ForallRelation { argumentName :: String, resultRelation :: Relation -> NamesT String (Either String) Relation } |
    -- A quantified relation, written as a function for convenience.
    QuantifiedVarRelation String

identityRelation :: Relation
identityRelation = Relation Equals

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
relationifyWithBindings bindings (TArrow a b) = ado
     a' <- relationifyWithBindings bindings a
     b' <- relationifyWithBindings bindings b
     in FunctionRelation { domainType: a, domainRelation: a', codomainRelation: b' }
relationifyWithBindings _ (TContextArrow _ _) =
    unsafeThrow "Not supported yet"
relationifyWithBindings bindings (TForall x body) =
     pure $ ForallRelation {
                argumentName: x,
                resultRelation: \r -> relationifyWithBindings ((Tuple x r) : bindings) body
              }

describeRelation :: Relation -> Term -> Term -> NamesT String (Either String) String
describeRelation (Relation r) left right =
    pure $ prettyShow (r left right)
describeRelation (FunctionRelation { domainType, domainRelation, codomainRelation }) left right =
    withName2 (suggestedVariableName domainType) $ \a a' -> ado
        domainDesc <- describeRelation domainRelation (Var a) (Var a')
        codomainDesc <- describeRelation codomainRelation (App left (Var a)) (App right (Var a'))
        in "∀ " <> a <> ", " <> a' <> " ∈ " <> prettyShow domainType <>
           " such that [" <> domainDesc <> "]: " <> codomainDesc
describeRelation (ForallRelation { argumentName, resultRelation }) left right =
    withName argumentName $ \r ->
      withName2 "A" $ \a a' -> do
        rightHandRelation <- resultRelation (QuantifiedVarRelation r)
        rightHandDesc <- describeRelation rightHandRelation (TypeApp left (Var a)) (TypeApp right (Var a'))
        pure $ "∀ " <> a <> ", " <> a' <> " ∈ Type: ∀ " <> r <> " ∈ (" <> a <> " → " <> a' <> "): " <>
               rightHandDesc
describeRelation (QuantifiedVarRelation r) left right =
    pure $ prettyShow (App (Var r) left) <> " = " <> prettyShow right

describeFreeTheorem :: TType -> Either String String
describeFreeTheorem t = runNamesT do
  r <- relationify t
  withName (suggestedVariableName t) $ \a ->
    describeRelation r (Var a) (Var a)

-- Clean up the variable names
-- Add product types, possibly sum types
-- Context types
-- Explore mu-recursive types
