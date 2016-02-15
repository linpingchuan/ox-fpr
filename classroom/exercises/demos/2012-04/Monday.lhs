ghci -Wall Monday.lhs

> {-# LANGUAGE UnicodeSyntax #-}
>
> module Monday
> where
> import Unicode
> import Prelude hiding (length)


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


> length  ∷  [a] → Integer
> length []        =  0
> length (_ : xs)  =  1 + length xs

> member  ∷  Integer → [Integer] → Bool
> member _ []        =  False
> member n (x : xs)  =  n == x || member n xs


The Tree Design Pattern (tree consumers)
----------------------------------------

< consume  ∷  Tree A → B
< consume Empty         =  ⋯
< consume (Node l x r)  =  ⋯ l ⋯ consume l ⋯ x ⋯ r ⋯ consume r ⋯

Binary search trees.

> data Tree a = Empty | Node (Tree a) a (Tree a)
>   deriving (Show, Eq, Ord)

> leaf  ∷  a → Tree a
> leaf x  =  Node Empty x Empty

> aTree  ∷  Tree String
> aTree  =  Node (Node (leaf "Ada") "C#" (leaf "D"))
>                "Haskell"
>                (Node Empty "Java" (leaf "Smalltalk"))

> inorder  ∷  Tree a → [a]
> inorder Empty         =  []
> inorder (Node l x r)  =  inorder l ++ [x] ++ inorder r

> insertTree  ∷  (Ord a) ⇒ a → Tree a → Tree a
> insertTree k Empty  = leaf k
> insertTree k (Node l x r)
>   | k ≤ x      =  Node (insertTree k l) x r
>   | otherwise  =  Node l x (insertTree k r)

Examples:

< inorder aTree
< insertTree "Scala" aTree


Misc
----

> twice  ∷  (a → a) → (a → a)
> twice f x  =  f ( f x)

∷ ⇒ ∀ → ← ⋯ ∨ ∧ ≤ ≥