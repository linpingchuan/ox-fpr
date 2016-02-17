ghci -Wall Wednesday.lhs

> module Wednesday
> where

Mergesort
---------

> data Tree a  =  Empty | Leaf a | Fork (Tree a) (Tree a)
>   deriving (Show)

Insertion sort.

> insertionSort  ::  (Ord a) => [a] -> [a]
> insertionSort  =   flatten . ibuild

> ibuild :: [a] -> Tree a
> ibuild []        =  Empty
> ibuild (a : as)  =  Fork (Leaf a) (ibuild as)

Merge sort.

> mergeSort  ::  (Ord a) => [a] -> [a]
> mergeSort  =   flatten . mbuild

> mbuild :: [a] -> Tree a
> mbuild []        =  Empty
> mbuild (a : as)  =  add a (mbuild as)

> add :: a -> Tree a -> Tree a
> add a Empty       =  Leaf a
> add a (Leaf b)    =  Fork (Leaf a) (Leaf b)
> add a (Fork l r)  =  Fork (add a r) l

Flattening a tree producing an ordered list.

> flatten :: (Ord a) => Tree a -> [a]
> flatten Empty       =  []
> flatten (Leaf a)    =  [a]
> flatten (Fork l r)  =  merge (flatten l) (flatten r)

Merging two ordered lists to yield an ordered list.

> merge :: (Ord a) => [a] -> [a] -> [a]
> merge [] bs    =  bs
> merge as []    =  as
> merge (a : as) (b : bs)
>   | a <= b     =  a : merge      as (b : bs)
>   | otherwise  =  b : merge (a : as)     bs

The height of a tree.

> height :: Tree a -> Integer
> height Empty       =  0
> height (Leaf _a)   =  1
> height (Fork l r)  =  1 + (height l) `max` (height r)
