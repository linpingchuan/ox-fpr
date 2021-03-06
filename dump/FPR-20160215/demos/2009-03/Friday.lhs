> module Friday
> where
> import Prelude hiding (reverse)

Mergesort
---------

Idea: capture the recursion scheme of merge sort (divide and conquer)
using a data type, sometimes called leaf trees.

> data Tree a
>   = Empty
>   | Leaf a
>   | Fork (Tree a) (Tree a)

Some test data and a test data generator.

> sampletree :: Tree Integer
> sampletree = Fork (Fork (Leaf 3) (Leaf 2)) (Leaf 1)

> skewed :: Integer -> Tree Integer
> skewed 0       = Empty
> skewed (n + 1) = Fork (Leaf n) (skewed n)

Phase 2 (Conquer):

> treesort :: (Ord a) => Tree a -> [a]
> treesort Empty      = []
> treesort (Leaf a)   = [a]
> treesort (Fork l r) = treesort l `merge` treesort r        

> merge :: (Ord a) => [a] -> [a] -> [a]
> merge [] bs = bs
> merge as [] = as
> merge (a : as) (b : bs)
>   | a <= b    = a : merge as (b : bs)
>   | otherwise = b : merge (a : as) bs 

Phase 2 (Divide):

The construction of a tree as an `unfold':

> treeify :: [a] -> Tree a
> treeify as
>   | n == 0    = Empty
>   | n == 1    = Leaf (head as)
>   | otherwise = Fork (treeify (take m as)) (treeify (drop m as))
>   where n = length as 
>         m = n `div` 2 

> mergesort :: (Ord a) => [a] -> [a]
> mergesort = treesort . treeify

Exercise: Can you implement `insertion sort' just by using a different
tree construction function?

The construction of a tree as a `fold':

> treeify' :: [a] -> Tree a
> treeify' []       = Empty
> treeify' (a : as) = insert a (treeify' as)

> treeify'' :: [a] -> Tree a
> treeify'' = foldr insert Empty

> insert :: a -> Tree a -> Tree a
> insert a Empty      = Leaf a
> insert a (Leaf b)   = Fork (Leaf a) (Leaf b)
> insert a (Fork l r) = Fork r (insert a l)

NB. `treeify'' construct a so-called Braun tree. Each node in a Braun
tree satisfies the following invariant: the size of the right subtree
is equal to or one more than the size of the left subtree.

NB. `merge sort' is an O(n log n) algorithm; the running time is
dominated by the second phase. Both `treeify' and `treeify'' have a
running time of O(n log n); for the former this can be improved to
O(n) using the tupling technique.

Program derivation
------------------

See also Tuesday.

> reverse :: [a] -> [a]
> reverse []     = []
> reverse (a:as) = reverse as ++ [a]

Specification:
    reverseCat x y = reverse x ++ y

Derivation. Case x = []:

  reverseCat [] y
    { specification }
= reverse [] ++ y
    {definition of `reverse'}
=  [] ++ y
    {definition of `++'}
=  y

Case x = a : as:

  reverseCat (a : as) y
    { specification }
= reverse (a : as) ++ y
    { definition of `reverse' }
= (reverse as ++ [a]) ++ y
    { associativity of `++' }
= reverse as ++ ([a] ++ y)
    { specification }
= reverseCat as (a:y)

We have derived the following implementation,

> reverseCat :: [a] -> [a] -> [a]
> reverseCat []       y = y
> reverseCat (a : as) y = reverseCat as (a : y)

which is the `civil servant' stack-reversal.

Implementing `reverse' in terms of `reverseCat':

  reverse x 
     { as ++ [] = as }
= reverse x ++ []
    { specification }
= reverseCat x []

Consequently,

> reverse' :: [a] -> [a]
> reverse' x = reverseCat x []