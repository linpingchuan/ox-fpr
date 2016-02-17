ghci -Wall Monday.lhs

> module Monday
> where

The List Design Pattern (list consumers)
----------------------------------------

< consume :: [A] -> B
< consume []       = ...
< consume (x : xs) = ... x ... xs ... consume xs ...

Insertion sort
--------------

From the slides:

> insertsort :: (Ord a) => [a] -> [a]
> insertsort []      = []
> insertsort (a : x) = insert a (insertsort x)
>
> insert :: (Ord a) => a -> [a] -> [a]
> insert a []   = [a]
> insert a (b : x)
>   | a <= b    = a : b : x
>   | otherwise = b : insert a x

Cook your own conditional.

> ifthenelse :: Bool -> a -> a -> a
> ifthenelse False _t e = e
> ifthenelse True  t _e = t

> factorial :: Integer -> Integer
> factorial n = ifthenelse (n == 0) 1 (n * factorial (n - 1))

> factorial' :: Integer -> Integer
> factorial' n = product [1 .. n]

The Tree Design Pattern (tree consumers)
----------------------------------------

< consume :: Tree A -> B
< consume Empty        = ...
< consume (Node l a r) = ... l ... consume l ... a ... r ... consume r ...

Binary search trees
-------------------

> data Tree a = Empty | Node (Tree a) a (Tree a)
>   deriving (Show)

`member1' works for arbitrary trees; its running-time is proportional
to the size of the tree.

> member1 :: (Eq a) => a -> Tree a -> Bool
> member1 _k Empty       =  False
> member1 k (Node l a r) =  k == a || member1 k l || member1 k r

`member2' assumes that the binary tree is a search tree; its
running-time proportional to the height of the tree.

> member2 :: (Ord a) => a -> Tree a -> Bool
> member2 _k Empty = False
> member2 k (Node l a r)
>   | k <  a  =  member2 k l
>   | k == a  =  True
>   | k >  a  =  member2 k r

Haskell only has functions that take exactly one argument.
The single argument is a pair:

> add :: (Integer, Integer) -> Integer
> add (x, y)  =  x + y

> dup :: a -> (a, a)
> dup x  =  (x, x)

The single result is a function:

> add' :: Integer -> (Integer -> Integer)
> add' x = \ y -> x + y

























