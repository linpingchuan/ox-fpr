ghci -Wall Monday.lhs

> {-# LANGUAGE UnicodeSyntax #-}
>
> module Monday
> where
> import Unicode

The List Design Pattern (list consumers)
----------------------------------------

< consume ∷ [A] → B
< consume []        =  ⋯
< consume (x : xs)  =  ⋯ x ⋯ xs ⋯ consume xs ⋯

Insertion sort
--------------

From the slides:

> insertSort ∷ (Ord a) ⇒ [a] → [a]
> insertSort []       =  []
> insertSort (a : x)  =  insert a (insertSort x)
>
> insert ∷ (Ord a) ⇒ a → [a] → [a]
> insert a []    =  [a]
> insert a (b : x)
>   | a ≤ b      =  a : b : x
>   | otherwise  =  b : insert a x

> total ∷ [Integer] → Integer
> total []        =  0
> total (i : is)  =  i + total is

> member ∷ Integer → [Integer] → Bool
> member _ []       =  False
> member a (b : bs)  =  a == b || member a bs

> quickSort ∷ (Ord a) ⇒ [a] → [a]
> quickSort []       =  []
> quickSort (a : x)  =  quickSort littles ++ [a] ++ quickSort bigs
>   where  littles   =  [ b | b <- x, b < a ]
>          bigs      =  [ b | b <- x, b ≥ a ]

The Tree Design Pattern (tree consumers)
----------------------------------------

< consume ∷ Tree A → B
< consume Empty         =  ⋯
< consume (Node l a r)  =  ⋯ l ⋯ consume l ⋯ a ⋯ r ⋯ consume r ⋯

Binary search trees
-------------------

> data Tree a = Empty | Node (Tree a) a (Tree a)
>   deriving (Show, Eq, Ord)

> leaf ∷ a → Tree a
> leaf a  =  Node Empty a Empty

> inorder ∷ Tree a → [a]
> inorder Empty         =  []
> inorder (Node l a r)  =  inorder l ++ [a] ++ inorder r

> insertTree ∷ (Ord a) ⇒ a → Tree a → Tree a
> insertTree k Empty  = leaf k
> insertTree k (Node l a r)
>   | k ≤ a      =  Node (insertTree k l) a r
>   | otherwise  =  Node l a (insertTree k r)