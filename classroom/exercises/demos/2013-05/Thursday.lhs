> {-# LANGUAGE UnicodeSyntax #-}
> module Thursday
> where
> import Unicode ()
> import Prelude hiding (lookup)


Random-access lists
-------------------

Interface.

> cons      ∷  a → Sequ a → Sequ a
> fromList  ∷  [a] → Sequ a
> (!)       ∷  Sequ a → Int → a
> lookup    ∷  Sequ a → Int → Maybe a
> size      ∷  Sequ a → Int

let n = 99999 :: Int
let x = map square [n, n - 1 .. 0]
:set +s
sum x
sum [ x !! i | i <- [0 .. n] ]
let s = fromList x
size s
s ! n
sum [ s ! i | i <- [0 .. n] ]

> square  ∷  Int → Int
> square x  =  x * x

Implementation.

> data Sequ a
>   =  Nil 
>   |  Zero  (Sequ (a, a))
>   |  One a (Sequ (a, a))
>   deriving (Show)

> cons a Nil        =  One a Nil
> cons a (Zero s)   =  One a s
> cons a (One b s)  =  Zero (cons (a, b) s)

> fromList  =  foldr cons Nil

> Nil     ! _  =  error "(!): index out of bounds"
> Zero s  ! n  =  sel (n `mod` 2) (s ! (n `div` 2))
> One a _ ! 0  =  a
> One _ s ! n  =  Zero s ! (n - 1)

> sel  ∷  Int → (a, a) → a
> sel 0 (a, _)  =  a
> sel 1 (_, b)  =  b

< s ! n = case lookup s n of
<         Nothing  →  error "(!): index out of bounds"
<         Just a   →  a

> lookup Nil       _  =  Nothing
> lookup (Zero s)  n  =  fmap (sel (n `mod` 2)) (lookup s (n `div` 2))
> lookup (One a _) 0  =  Just a
> lookup (One _ s) n  =  lookup (Zero s) (n - 1)

> size Nil        =  0
> size (Zero s)   =      2 * size s
> size (One _ s)  =  1 + 2 * size s

∷ ⇒ ∀ → ← ⋯ ∨ ∧ ≤ ≥
