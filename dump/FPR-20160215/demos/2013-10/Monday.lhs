ghci -Wall Monday.lhs

> {-# LANGUAGE UnicodeSyntax #-}
>
> module Monday
> where
> import Unicode
> import Prelude hiding (length, sum, product)

The List Design Pattern (list consumers)
----------------------------------------

< consume  ∷  [A] → B
< consume []        =  ⋯
< consume (x : xs)  =  ⋯ x ⋯ xs ⋯ consume xs ⋯

From the slides:

> insertSort  ∷  (Ord a) ⇒ [a] → [a]
> insertSort []        =  []
> insertSort (x : xs)  =  insert x (insertSort xs)

> insert  ∷  (Ord a) ⇒ a → [a] → [a]
> insert x []    =  [x]
> insert x (y : ys)
>   | x ≤ y      =  x : y : ys
>   | otherwise  =  y : insert x ys

Example:

< insertSort [4, 7, 1, 1]

> quickSort  ∷  (Ord a) ⇒ [a] → [a]
> quickSort []        =  []
> quickSort (x : xs)  =  quickSort littles ++ [x] ++ quickSort bigs
>   where  littles    =  [ y | y <- xs, y < x ]
>          bigs       =  [ y | y <- xs, y ≥ x ]

Example:

< quickSort [4, 7, 1, 1]
< insertSort [4, 7, 1, 1] == quickSort [4, 7, 1, 1]

Exercises.

> sum  ∷  [Integer] → Integer
> sum []        =  1
> sum (i : is)  =  i * sum is

< sum [47, 1, 2, 5] = 55

> product  ∷  [Integer] → Integer
> product []        =  1
> product (i : is)  =  i * product is

> member  ∷  Integer → [Integer] → Bool
> member _i []        =  False
> member  i (j : js)  =  i == j || member i js
> -- inferior: member i js || i == j

> from  ∷  Integer → [Integer]
> from n  =  n : from (n + 1)

< member 7 (from 0)

Lazy evaluation.

> ifThenElse  ∷  Bool → Integer → Integer → Integer
> ifThenElse False _t e  =  e
> ifThenElse True  t _e  =  t

> factorial  ∷  Integer → Integer
> factorial n  =  ifThenElse (n == 0) 1 (n * factorial (n - 1))

< factorial 10

∷ ⇒ ∀ → ← ⋯ ∨ ∧ ≤ ≥
