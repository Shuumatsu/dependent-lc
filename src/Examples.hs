module Examples where

import Core
import TypeChecker

id0 = Lam (Inf (Bound 0))

const0 = Lam (Lam (Inf (Bound 1)))

tfree a = BaseType (Global a)

free x = Inf (Free (Global x))

term1 =
  Ann
    id0
    (Fun (tfree "a") (tfree "a"))
    :@: free "y"

term2 =
  Ann
    const0
    ( Fun
        (Fun (tfree "b") (tfree "b"))
        ( Fun
            (tfree "a")
            (Fun (tfree "b") (tfree "b"))
        )
    )
    :@: id0
    :@: free "y"

env1 =
  [ (Global "y", HasType (tfree "a")),
    (Global "a", HasKind Star)
  ]

env2 = [(Global "b", HasKind Star)] ++ env1