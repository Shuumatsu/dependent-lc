module Core where

-- There are four kinds of terms:
--     terms with an explicit type annotation
--     variables
--     applications
--     lambda abstractions

-- a value is either
--     a neutral term: variable, application
--     a lambda abstraction

data Type
  = BaseType Name
  | Fun Type Type
  deriving (Show, Eq)

data InferableTerm
  = Ann CheckableTerm Type
  | -- indicating how many binders occur between its binder and the occurrence.
    Bound Int
  | Free Name
  | -- The infix constructor :@: denotes application.
    InferableTerm :@: CheckableTerm
  deriving (Show, Eq)

-- Inferable terms are embedded in the checkable terms
-- via the constructor Inf,
-- and lambda abstractions
data CheckableTerm
  = Inf InferableTerm
  | -- will not introduce an explicit variable due to our use of de Bruijn indices
    -- so only body, no arg here
    Lam CheckableTerm
  deriving (Show, Eq)

data Name
  = -- refer to global entities using strings
    Global String
  | -- When passing a binder in an algorithm,
    -- we have to convert a bound variable into a free variable temporarily,
    -- and use Local for that.
    Local Int
  | -- for the same reason of Local, during quoting, we will use the Quote constructor.
    Quote Int
  deriving (Show, Eq)

-- we use higher-order abstract syntax:
--     represent function values as Haskell functions of type `Value -> Value`.
-- For instance, the term const – when evaluated – results in the value
-- VLam (λx -> VLam (λy -> x)).
data Value = VLam (Value -> Value) | VNeutral Neutral

-- A neutral term is either a
--     variable (NFree),
--     or an application of a neutral term to a value (NApp).
data Neutral = NFree Name | NApp Neutral Value

-- creates the value corresponding to a free variable:
vfree :: Name -> Value
vfree n = VNeutral (NFree n)