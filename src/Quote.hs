module Quote (quote) where

import Core

-- because we use the higher-order abstract syntax to represent `Value`
-- we cannot simply derive `Show` and `Eq` as we did for the other types.

quote :: Int -> Value -> CheckableTerm
-- When `quote` moves underneath a binder,
-- we introduce a temporary name for the bound variable.
-- use the constructor `Quote` that takes an argument of type `Int` here
-- to ensure that the newly created names do not clash with other names
quote depth (VLam f) = Lam (quote (depth + 1) (f (vfree (Quote depth))))
quote depth (VNeutral n) = Inf (neutralQuote depth n)

neutralQuote :: Int -> Neutral -> InferableTerm
neutralQuote depth (NFree x) = boundfree depth x
neutralQuote depth (NApp n v) = neutralQuote depth n :@: quote depth v

-- checks if it is a `Quote` and thus a bound variable, or a free name
boundfree :: Int -> Name -> InferableTerm
-- the `k` is defined as the binder's depth,
-- but in fact it should be the distance between the var and its binder,
-- e.g., `quote (const x y = x)` in our implementation:
--       `x = Quote (depth of binder = 0)`
--       but in fact it should be `Bound (distance to binder = 1)`
-- so it should be curr_depth - k - 1
boundfree depth (Quote k) = Bound (depth - k -1)
boundfree depth x = Free x