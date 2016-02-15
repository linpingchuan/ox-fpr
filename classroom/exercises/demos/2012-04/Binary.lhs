> {-# LANGUAGE UnicodeSyntax #-}
> module Bin
> where
> import Unicode ()

> data Bin  =  Nil
>           |  Zero Bin
>           |  One  Bin
>           deriving (Show)

Semantics of binary numerals.

> integer  ∷  Bin → Integer
> integer Nil        =  0
> integer (Zero bs)  =  0 + 2 * integer bs
> integer (One  bs)  =  1 + 2 * integer bs

> ten  ∷  Bin
> ten  =  Zero (One (Zero (One Nil)))

Incrementing.

> incr  ∷  Bin → Bin
> incr Nil        =  One Nil
> incr (Zero bs)  =  One bs
> incr (One  bs)  =  Zero (incr bs)

Testing.

> propertyIncr  ∷  Bin -> Bool
> propertyIncr bs  =  integer (incr bs) == 1 + integer bs

> bins  ∷  [Bin]
> bins  =  Nil : [ b bs | bs <- bins, b <- [Zero, One] ]

> counterExamples  ∷  [Bin]
> counterExamples  =  [ bs | bs <- bins, not (propertyIncr bs) ]

∷ ⇒ ∀ → ← ⋯ ∨ ∧ ≤ ≥
