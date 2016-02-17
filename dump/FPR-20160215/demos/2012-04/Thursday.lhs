ghci -Wall Thursday.lhs

> {-# LANGUAGE UnicodeSyntax, RankNTypes #-}
>
> module Thursday
> where
> import Unicode
> import Prelude hiding (not, (&&), (||))

> type Boolean  =  ∀ a . a → a → a

> false, true  ∷  Boolean
> false  =  \ _ y → y
> true   =  \ x _ → x

> ifthenelse  ∷  Boolean → a → a → a
> ifthenelse b t e  =  b t e

> not  ∷  Boolean → Boolean
> not b  =  ifthenelse b false true

> (&&), (||)  ∷  Boolean → Boolean → Boolean
> x && y  =  ifthenelse x y false
> x || y  =  ifthenelse x true y

> convert  ∷  Bool → Boolean
> convert False  =  false
> convert True   =  true

> leq  ∷  (Ord a) ⇒ a → a → Boolean
> leq a b  =  convert (a ≤ b)

> factorial  ∷  Integer → Integer
> factorial n  =  ifthenelse (leq n 0) 1 (n * factorial (n - 1))

> member  ∷  Integer → [Integer] → Boolean
> member _ []        =  false
> member n (x : xs)  =  convert (n == x) || member n xs

∷ ⇒ ∀ → ← ⋯ ∨ ∧ ≤ ≥