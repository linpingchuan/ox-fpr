ghci -Wall Tuesday.lhs

> {-# LANGUAGE UnicodeSyntax #-}
>
> module Tuesday
> where
> import Unicode
> import Prelude hiding (map)
> import Data.List (isInfixOf)

Map-reduce
----------

> map  ∷  (a → b) → ([a] → [b])
> map f = fs
>   where fs []        =  []
>         fs (x : xs)  =  f x : fs xs

< map _f []        =  []
< map f  (x : xs)  =  f x : map f xs

> reduce  ∷  m → (m → m → m) → ([m] → m)
> reduce ε (⊗) = crush
>   where crush []        =  ε
>         crush (d : ds)  =  d ⊗ crush ds

> mapReduce  ∷  m → (m → m → m) → (a → m) → ([a] → m)
> mapReduce ε (⊗) f  =  reduce ε (⊗) . map f

Applications
------------

Examples.

< member "lisa" ["anja", "lisa", "florian", "ralf"]
< search "is" ["anja", "lisa", "florian", "ralf"]
< hits "a" ["anja", "lisa", "florian", "ralf"]

Exact search.

> member  ∷  String → ([String] → Bool)
> member s = mapReduce False (||) (\ x -> x == s)

member "lisa" ["anja", "lisa", "florian", "ralf"]

Substring search.

> search  ∷  String → ([String] → Bool)
> search s  =  mapReduce False (∨) (\ x → s `isInfixOf` x)

search "is" ["anja", "lisa", "florian", "ralf"]

Number of hits.

> hits  ∷  String → ([String] → Integer)
> hits s  =  mapReduce 0 (+) (\ x → if s `isInfixOf` x then 1 else 0)

hits "a" ["anja", "lisa", "florian", "ralf"]

Ranking webpages.

> type Rank  =  Int
>
> best  ∷  String → ([String] → Rank)
> best s  =  reduce minBound max . map (rank s)

> rank  ∷  String → String → Rank
> rank _ _  =  1  -- Google's secret

Satellite data.

> type RankedPage  =  (String, Rank)

> best'  ∷  String → ([String] → RankedPage)
> best' s  =  reduce minBound' max' . map (\ x → (x, rank s x))

> minBound'  ∷  RankedPage
> minBound'  =  ("<<not found>>", minBound)

> max'  ∷  RankedPage → RankedPage → RankedPage
> max' (s, m) (t, n)
>   | m >= n     =  (s, m)
>   | otherwise  =  (t ,n)

Carry-lookahead
---------------

> base, maxDigit ∷ Int
> base  =  10
> maxDigit  =  base - 1

> data Bit = O | I deriving (Eq, Show)


Using functions.

> carry  ∷  [(Int, Int)] → (Bit → Bit)
> carry  =  mapReduce id (·) kpg

< carry [(4, 7), (1, 1), (0, 8), (1, 5), (9, 5), (7, 2), (8, 9), (0, 1)] O

> kpg  ∷  (Int, Int) → (Bit → Bit)
> kpg (i, j)
>   | i + j <  maxDigit  =  const O  -- kill
>   | i + j == maxDigit  =  id       -- propagate
>   | i + j >  maxDigit  =  const I  -- generate


Using an enumeration type.

> data KPG = K | P | G deriving (Eq, Show)

> carry'  ∷  [(Int, Int)] → KPG
> carry'  =  mapReduce P (○) kpg'

< carry' [(4, 7), (1, 1), (0, 8), (1, 5), (9, 5), (7, 2), (8, 9), (0, 1)]
< carry' [(1, 8), (7, 2), (5, 4), (3, 6)]

> kpg'  ∷  (Int, Int) → KPG
> kpg' (i, j)
>   | i + j <  maxDigit  =  K
>   | i + j == maxDigit  =  P
>   | i + j >  maxDigit  =  G

> infixr ○
> (○)  ∷  KPG → KPG → KPG   
> K ○ _f  =  K
> P ○ f   =  f
> G ○ _f  =  G

Variations of "largest"
-----------------------

> largest  ∷  (Bounded a, Ord a) ⇒ [a] → a
> largest []        =  minBound
> largest (i : is)  =  i `max` largest is

> largest'  ∷  (Ord a) ⇒ [a] → Maybe a
> largest' []        =  Nothing
> largest' (i : is)  =  case largest' is of
>                       Nothing  →  Just i
>                       Just m   →  Just (i `max` m)

∷ ⇒ ∀ → ← ⋯ ∨ ∧ ≤ ≥ ·
