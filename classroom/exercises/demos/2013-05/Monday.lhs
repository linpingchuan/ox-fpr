ghci -Wall Monday.lhs

> {-# LANGUAGE UnicodeSyntax #-}
>
> module Monday
> where
> import Unicode
> import Prelude hiding (length, sum)

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

Exercises.

> sum ∷ [Integer] → Integer
> sum []        =  0
> sum (i : is)  =  i + sum is

< sum [47, 1, 2, 5] = 55

> member  ∷  Integer → [Integer] → Bool
> member _a []       =  False
> member a (b : bs)  =  a == b || member a bs

Lazy evaluation.

> ifThenElse ∷ Bool → Integer → Integer → Integer
> ifThenElse False _t e = e
> ifThenElse True  t _e = t

> factorial ∷ Integer → Integer
> factorial n = ifThenElse (n == 0) 1 (n * factorial (n - 1))

∷ ⇒ ∀ → ← ⋯ ∨ ∧ ≤ ≥
