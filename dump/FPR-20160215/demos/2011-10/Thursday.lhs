ghci -Wall Wednesday.lhs

> {-# LANGUAGE UnicodeSyntax, NPlusKPatterns #-}
>
> module Thursday
> where
> import Unicode
> import Monday (insert, insertSort, quickSort)
> import Wednesday (mergeSort)
> import qualified List

Hacking session
---------------

        Testing shows the presence, 
        not the absence of bugs.
                 Edsger W. Dijkstra

  Beware of bugs in the above code; 
  I have only proved it correct,
  not tried it.
                    Donald E. Knuth

Generators and properties
-------------------------

< type Prop ∷ * → *
< type Gen  ∷ * → *

> infixr 4  ⊗, ∧
> infixr 2  ∪
> infixr 1  -->, ==>

> (-->)   ∷ Gen a → Prop b → Prop (a → b)
> (==>)   ∷ Gen a → (a → Prop b) → Prop (a → b)

> (∧)     ∷ Prop a → Prop a → Prop a
> like    ∷ (Eq b) ⇒ (a → b) → a → Prop b
> ordered ∷ (Ord a) ⇒ Prop [a]

> val     ∷ [a] → Gen a
> (∪)     ∷ Gen a → Gen a → Gen a
> (⊗)     ∷ Gen a → Gen b → Gen (a, b)
> perms   ∷ [a] → Gen [a]

Examples
--------

> lists  ∷  Gen [Integer]
> lists  =   val [[], [4,7,1,1], [999,998..0]] ∪ perms [1..8]

> sorter  ∷  Prop ([Integer] → [Integer])
> sorter  =   lists --> ordered

(val [[], [4,7,1,1], [999,998..0]] --> ordered) insertSort
(perms [1..8] --> ordered) insertSort
(lists --> ordered) insertSort
(lists ==> \ x y -> List.sort x == y) insertSort
(lists ==> like List.sort) insertSort
all sorter [insertSort, quickSort, mergeSort]
(val [1..9] --> val [[1..9]] --> ordered) insert

Consider the function isqrt, which calculates the integer square root.

> unknown ∷ Integer → Integer
> unknown n = loop 0 3 1
>   where loop i k s | s ≤ n     = loop (i + 1) (k + 2) (s + k)
>                    | otherwise = i

It is not immediately obvious that this definition is correct.

> spec  ∷  Prop (Integer → Integer)
> spec  =  val [0..9999] ==> \ n r → r * r ≤ n && n < (r + 1) * (r + 1)

spec isqrt

Laws of generators and properties
---------------------------------

  (g1 --> p) ∧ (g2 --> p)  =  g1 ∪ g2 --> p
  (g --> p1) ∧ (g --> p2)  =  g --> p1 ∧ p2
  (g1 ⊗ g2 --> p) f       =  (g1 --> g2 --> p) (curry f)


Implementation
--------------

Properties
- - - - -

> type Prop a  =  a → Bool

> g --> p  =  \ f → all p (map f (enum g))

> g ==> p  =  \ f → and [ p x (f x) | x ← enum g ]

> p1 ∧ p2  =  \ x → p1 x && p2 x

> like f  =  \ x y → f x == y

> ordered []                  =  True
> ordered [_a]                =  True
> ordered (a1 : as@(a2 : _))  =  a1 ≤ a2 && ordered as

Test data generators
- - - - - - - - - -

> newtype Gen a  =  Gen { enum :: [a] }

> val as  =  Gen as

> g1 ∪ g2  =  Gen (enum g1 ++ enum g2)

> g1 ⊗ g2  =  Gen [ (a1, a2) | a1 ← enum g1, a2 ← enum g2 ]

> perms x  =  Gen (permutations x)

> permutations ∷ [a] → [[a]]
> permutations []        =  [[]]
> permutations (a : as)  =  [ cs | bs ← permutations as, cs ← inserts a bs ]

> inserts ∷ a → [a] → [[a]]
> inserts a []        =  [[a]]
> inserts a (b : bs)  =  (a : b : bs) : [ b : cs | cs ← inserts a bs ]

∷ ⇒ ∀ → ← ⋯ ≤ ≥ ∈ ≼
