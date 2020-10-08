module Eval where

import Core

-- Substitution is handled by passing around an environment of values.
-- Since bound variables are represented as integers,
-- the environment is just a list of values
-- where the i-th position corresponds to the value of variable i.
type Env = [Value]

evali :: InferableTerm -> Env -> Value
evali (Ann e _) env = evalc e env
evali (Free x) env = vfree x
evali (Bound i) env = env !! i
evali (e :@: e') env = vapp (evali e env) (evalc e' env)

vapp :: Value -> Value -> Value
vapp (VLam f) v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)

evalc :: CheckableTerm -> Env -> Value
evalc (Inf i) env = evali i env
-- introduce a Haskell function and
-- add the bound variable x to the 0th position of env while evaluating the body.
evalc (Lam e) env = VLam (\x -> evalc e (x : env))
