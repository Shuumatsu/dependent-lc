{-# LANGUAGE LambdaCase #-}

module TypeChecker where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Core

-- The type checking algorithm can fail,
-- use the function throwError :: String -> Result a to report an error.
type Result a = Either String a

-- the big-Gamma
type Context = [(Name, Info)]

data Info
  = -- adding a type identifier
    HasKind Kind
  | -- adding a term identifier
    HasType Type
  deriving (Show)

data Kind = Star deriving (Show)

-- check the well-formedness of types
kind :: Context -> Type -> Kind -> Result ()
kind ctx (BaseType name) Star = case lookup name ctx of
  Just (HasKind Star) -> return ()
  Nothing -> throwError "unknown type identifier"
kind ctx (Fun argty retty) Star = kind ctx argty Star >> kind ctx retty Star

-- for annotated terms, variables, and applications we can easily determine the type.

-- We do not try to infer the types of
-- for lambda-bound variables. We do not try to infer the types, we perform only type checking.
--      the type checker will never encounter a bound variable

-- Our type checker does not perform unification.

infer :: Int -> Context -> InferableTerm -> Either String Type
-- check annotated terms against their type annotation, and then return the type
infer depth ctx (Ann e t) = do
  kind ctx t Star
  check depth ctx e t
  return t
--  type of a variable can be looked up in the environment
infer depth ctx (Free x) = case lookup x ctx of
  Just (HasType t) -> return t
  Nothing -> throwError "unknown identifier"
infer depth ctx (e :@: e') =
  infer depth ctx e >>= \case
    Fun argty retty -> do
      -- check the argument against the function’s domain
      check depth ctx e' argty
      -- return the range as the result type.
      return retty
    _ -> throwError "illegal application"

check :: Int -> Context -> CheckableTerm -> Type -> Either String ()
check depth ctx (Inf e) expected = do
  inferred <- infer depth ctx e
  unless (inferred == expected) (throwError "type mismatch")
-- in ctx, to check if `\x -> e` is typed `argty -> retty`,
-- check if in ((x :: argty) : ctx), `e` is typed `retty`
check depth ctx (Lam e) (Fun argty retty) =
  check
    (depth + 1)
    -- `Local depth` is always a fresh name that we can associate with the bound variable
    ((Local depth, HasType argty) : ctx)
    -- because we are turning a bound variable into a free variable,
    -- we have to perform the corresponding substitution on the body.
    (substc 0 (Free (Local depth)) e)
    retty
check depth ctx _ _ = throwError "type mismatch"

substi :: Int -> InferableTerm -> InferableTerm -> InferableTerm
substi depth inferable (Ann e ty) = Ann (substc depth inferable e) ty
substi depth inferable (Bound j) = if depth == j then inferable else Bound j
substi depth inferable (Free y) = Free y
substi depth inferable (e :@: e') = substi depth inferable e :@: substc depth inferable e'

substc :: Int -> InferableTerm -> CheckableTerm -> CheckableTerm
substc depth r (Inf e) = Inf (substi depth r e)
substc depth r (Lam e) = Lam (substc (depth + 1) r e)
