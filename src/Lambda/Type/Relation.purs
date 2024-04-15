
module Lambda.Type.Relation(
                            Relation(..),
                            relationify, relationifyWithBindings
                           ) where

import Lambda.Type(TType(..))
--import Lambda.Term(Term(..), prettyShow)

import Data.Tuple(Tuple(..))
import Data.Maybe(Maybe(..))
import Data.List(List(..), (:))
import Data.Foldable(lookup)
import Data.Either(Either(..))
import Prelude
import Effect.Exception.Unsafe(unsafeThrow)

data Relation =
    -- The identity relation on a type, which is true if (and only if)
    -- the two values being compared are actually equal.
    IdentityRelation TType |
    -- A relation on functions which holds if it maps related elements
    -- to related elements.
    FunctionRelation { domainType :: TType, domainRelation :: Relation, codomainRelation :: Relation } |
    -- A relation that holds for all sub-relations.
    ForallRelation { argumentName :: String, resultRelation :: Relation -> Either String Relation }

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

{-
describeRelation :: Relation -> Term -> Term -> Either String String
describeRelation (IdentityRelation _) left right =
    Right $ prettyShow left <> " = " <> prettyShow right
-}
