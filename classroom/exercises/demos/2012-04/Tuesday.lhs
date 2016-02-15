ghci -Wall Tuesday.lhs

> {-# LANGUAGE UnicodeSyntax #-}
>
> module Tuesday
> where
> import Unicode
> import Prelude hiding (map, sum, concat)


Map-reduce
----------

> mapReduce  ∷  m → (m → m → m) → (a → m) → ([a] → m)
> mapReduce ε (⊗) f  =  reduce ε (⊗) . map f

> map  ∷  (a → b) → ([a] → [b])
> map _ []        =  []
> map f (x : xs)  =  f x : map f xs

> reduce  ∷  m → (m → m → m) → ([m] → m)
> reduce ε (⊗)  =  f
>   where f []        =  ε
>         f (x : xs)  =  x ⊗ f xs

Applications.

> search  ∷  String → ([String] → Bool)
> search s  =  reduce False (∨) . map (\ x → x == s)

search "lisa" ["anja", "lisa", "florian", "ralf"]

> best  ∷  String → ([String] → Int)
> best s  =  reduce minBound max . map (rank s)

> rank  ∷  String → String → Int
> rank _ _  =  1  -- Google's secret

> best'  ∷  String → ([String] → (String, Int))
> best' s  =  reduce minBound' max' . map (\ x → (x, rank s x))

> minBound'  ∷  (String, Int)
> minBound'  =  ("<<not found>>", minBound)

> max'  ∷  (String, Int) → (String, Int) → (String, Int)
> max' (s, m) (t, n)
>   | m >= n     =  (s, m)
>   | otherwise  =  (t ,n)

Evaluation of polynomials.

> g  ∷  Integer → Integer
> g (x)  =  1 + 7 * x + 9 * x ^ 2 + 0 * x ^ 3 + 1 * x ^ 4

Sequential evaluation.

> eval  ∷  Integer → [Integer] → Integer
> eval _ []        =  0
> eval x (a : as)  =  a + eval x as * x

Parallel evaluation using map-reduce.

> peval  ∷  Integer → [Integer] → (Integer, Integer)
> peval x  =  reduce ε (⊗) . map (\ a → (x, a))
>   where ε  =  (1, 0)
>         (y, a) ⊗ (z, b)  =  (y * z, a + y * b)

g 17
eval 17 [1, 7, 9, 0, 1]
peval 17 [1, 7, 9, 0, 1]


Enumeration types
-----------------
       
> data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
>  deriving (Show, Eq, Enum)

[Mon .. Sun]


List comprehensions
-------------------

> concat  ∷  [[a]] → [a]
> concat xss  =  [ x | xs <- xss, x <- xs ]

cross [Mon .. Sun] [0 .. 2]

> cross  ∷  [a] → [b] → [(a, b)]
> cross xs ys  =  [ (x, y)  | x <- xs, y <- ys ]

without list comprehensions:

> cross1  ∷  [a] → [b] → [(a, b)]
> cross1 []       _   =  []
> cross1 (x : xs) ys  =  distr x ys ++ cross1 xs ys

> distr ∷ a → [b] → [(a, b)]
> distr _ []        = []
> distr x (y : ys)  = (x, y) : distr x ys

> cross2  ∷  [a] → [b] → [(a, b)]
> cross2 []       _   =  []
> cross2 (x : xs) ys  =  map (\ y → (x, y)) ys ++ cross2 xs ys

> cross3  ∷  [a] → [b] → [(a, b)]
> cross3 xs ys  =  concat (map (\ x → map (\ y → (x, y)) ys) xs)

∷ ⇒ ∀ → ← ⋯ ∨ ∧ ≤ ≥