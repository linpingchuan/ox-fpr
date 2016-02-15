ghci -Wall Tuesday.lhs

> {-# LANGUAGE UnicodeSyntax #-}
>
> module Tuesday
> where
> import Unicode
> import Prelude hiding (fst, map, concat)
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

> hits'  ∷  Char → ([String] → Integer)
> hits' c  =  mapReduce 0 (+) (mapReduce 0 (+) (\ x → if c == x then 1 else 0))

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

Carry-lookahead.

> data Bit = O | I deriving (Eq, Show)

> carry  ∷  [(Bit, Bit)] → (Bit → Bit)

< carry [(I, O), (I, I), (O, O), (I, O)] O

Using functions.

> carry  =  mapReduce id (·) kpg

> kpg  ∷  (Bit, Bit) → Bit → Bit
> kpg (O, O)  =  const O
> kpg (O, I)  =  id
> kpg (I, O)  =  id
> kpg (I, I)  =  const I

Using an enumeration type.

> data KPG = K | P | G deriving (Eq, Show)

> carry'  ∷  [(Bit, Bit)] → KPG
> carry'  =  mapReduce P (○) kpg'

> kpg'  ∷  (Bit, Bit) → KPG
> kpg' (O, O)  =  K
> kpg' (O, I)  =  P
> kpg' (I, O)  =  P
> kpg' (I, I)  =  G

> infixr ○
> (○)  ∷  KPG → KPG → KPG   
> K ○ _f  =  K
> P ○ f   =  f
> G ○ _f  =  G

Relating the two implementations.

> apply  ∷  KPG → Bit → Bit
> apply K  =  const O
> apply P  =  id
> apply G  =  const I

< apply P  =  id
< apply c · apply d  =  apply (c ○ d)
< carry bs  =  apply (carry' bs)

The function `apply' is a monoid homomorphism.

∷ ⇒ ∀ → ← ⋯ ∨ ∧ ≤ ≥ ·
