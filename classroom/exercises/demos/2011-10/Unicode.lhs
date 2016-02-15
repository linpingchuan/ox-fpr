> {-# LANGUAGE UnicodeSyntax #-}
>
> module Unicode
> where

> infix 4 ≤, ≥
> (≤), (≥) ∷ (Ord a) ⇒ a → a → Bool
> a ≤ b  =  a <= b
> a ≥ b  =  a >= b
               
∷ ⇒ ∀ → ← ⋯ ≤ ≥