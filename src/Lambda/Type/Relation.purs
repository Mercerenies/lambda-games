
module Lambda.Type.Relation(
                            Relation(..),
                            relationify, relationifyWithBindings,
                            describeRelation, describeFreeTheorem
                           ) where

import Lambda.Type (TType(..), suggestedVariableName)
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
    -- The identity relation on a type, which is true if (and only if)
    -- the two values being compared are actually equal.
    IdentityRelation TType |
    -- A relation on functions which holds if it maps related elements
    -- to related elements.
    FunctionRelation { domainType :: TType, domainRelation :: Relation, codomainRelation :: Relation } |
    -- A relation that holds for all sub-relations.
    ForallRelation { argumentName :: String, resultRelation :: Relation -> Either String Relation } |
    -- A quantified relation, written as a function for convenience.
    QuantifiedVarRelation String

-- Lift a closed type into a relation.
relationify :: TType -> Either String Relation
relationify = relationifyWithBindings Nil

relationifyWithBindings :: List (Tuple String Relation) -> TType -> Either String Relation
relationifyWithBindings bindings (TVar x)
    | Just rel <- lookup x bindings = Right rel
    | otherwise = Left $ "Unbound type variable: " <> x
relationifyWithBindings _ (TGround x) =
    -- For now, assume all ground types are just the identity and
    -- don't have more complex relations. We will update this later.
    Right $ IdentityRelation (TGround x)
relationifyWithBindings bindings (TArrow a b) = ado
     a' <- relationifyWithBindings bindings a
     b' <- relationifyWithBindings bindings b
     in FunctionRelation { domainType: a, domainRelation: a', codomainRelation: b' }
relationifyWithBindings _ (TContextArrow _ _) =
    unsafeThrow "Not supported yet"
relationifyWithBindings bindings (TForall x body) =
     Right $ ForallRelation {
                 argumentName: x,
                 resultRelation: \r -> relationifyWithBindings ((Tuple x r) : bindings) body
               }

describeRelation :: Relation -> Term -> Term -> NamesT String (Either String) String
describeRelation (IdentityRelation _) left right =
    pure $ prettyShow left <> " = " <> prettyShow right
describeRelation (FunctionRelation { domainType, domainRelation, codomainRelation }) left right =
    withName2 (suggestedVariableName domainType) $ \a a' -> ado
        domainDesc <- describeRelation domainRelation (Var a) (Var a')
        codomainDesc <- describeRelation codomainRelation (App left (Var a)) (App right (Var a'))
        in "∀ " <> a <> ", " <> a' <> " ∈ " <> prettyShow domainType <>
           " such that [" <> domainDesc <> "]: " <> codomainDesc
describeRelation (ForallRelation { argumentName, resultRelation }) left right =
    withName argumentName $ \r ->
      withName2 "A" $ \a a' -> do
        rightHandRelation <- lift $ resultRelation (QuantifiedVarRelation r)
        rightHandDesc <- describeRelation rightHandRelation (TypeApp left (Var a)) (TypeApp right (Var a'))
        pure $ "∀ " <> a <> ", " <> a' <> " ∈ Type: ∀ " <> r <> " ∈ (" <> a <> " → " <> a' <> "): " <>
               rightHandDesc
describeRelation (QuantifiedVarRelation r) left right =
    pure $ prettyShow (App (Var r) left) <> " = " <> prettyShow right

describeFreeTheorem :: TType -> Either String String
describeFreeTheorem t = runNamesT do
  r <- lift $ relationify t
  withName (suggestedVariableName t) $ \a ->
    describeRelation r (Var a) (Var a)

-- Clean up the variable names
-- Add product types, possibly sum types
-- Context types
-- Explore mu-recursive types
