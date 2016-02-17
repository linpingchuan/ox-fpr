ghci -Wall Tuesday.lhs

> {-# LANGUAGE UnicodeSyntax #-}
>
> module Tuesday
> where
> import Prelude hiding ((++), concat, last)

The List Design Pattern (list consumers)
----------------------------------------

< consume ∷ [A] → B
< consume []        =  ⋯
< consume (x : xs)  =  ⋯ x ⋯ xs ⋯ consume xs ⋯

Warm-up
-------

> (++) ∷ [a] → [a] → [a]
> []       ++ y  =  y
> (a : as) ++ y  =  a : (as ++ y)

> last ∷ [a] → a
> last []             =  error "last: empty list"
> last [a]            =  a
> last (_a : b : xs)  =  last (b : xs)

> concat ∷ [[a]] → [a]
> concat []          =  []
> concat (xs : xss)  =  xs ++ concat xss

Substring
---------

< x ∈ y   <=>   ∃ s, t . s ++ x ++ t = y

Cul-de-sac:

< (∈) ∷ String → String → Bool
< [] ∈ y        =  True
< (a : as) ∈ y  =  ... as ∈ y ... 

< substring ∷ String → String → Bool
< substring [] y        =  True
< substring (a : as) y  =  ... substring as y ... 

This works ...

> (∈) ∷ String → String → Bool
> x ∈ []        =  null x
> x ∈ (b : bs)  =  x ≼ (b : bs) || x ∈ bs

> substring ∷ String → String → Bool
> substring x []        =  null x  
> substring x (b : bs)  =  prefix x (b : bs) || substring x bs

< x ≼ y   <=>   ∃ t . x ++ t = y

> (≼) ∷ String → String → Bool
> [] ≼ _y  =  True
> (_ : _) ≼ []         =  False
> (a : as) ≼ (b : bs)  =  (a == b) && (as ≼ bs)

> prefix ∷ String → String → Bool
> prefix [] _y              =  True
> prefix (_ : _) []         =  False
> prefix (a : as) (b : bs)  =  a == b && prefix as bs
