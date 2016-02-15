ghci -Wall Monday.lhs

> module Monday
> where

The List Design Pattern (list consumers)
----------------------------------------

< consume :: [A] -> B
< consume []        =  ...
< consume (x : xs)  =  ... x ... xs ... consume xs ...

Insertion sort
--------------

From the slides:

> insertsort :: (Ord a) => [a] -> [a]
> insertsort []       =  []
> insertsort (a : x)  =  insert a (insertsort x)
>
> insert :: (Ord a) => a -> [a] -> [a]
> insert a []    =  [a]
> insert a (b : x)
>   | a <= b     =  a : b : x
>   | otherwise  =  b : insert a x

> member :: Integer -> [Integer] -> Bool
> member _a []        =  False
> member  a (b : bs)  =  ourownor (a == b) (member a bs)

> ourownor :: Bool -> Bool -> Bool
> ourownor False  b  =  b
> ourownor True  _b  =  True

> fromto :: Integer -> Integer -> [Integer]
> fromto m n  =  if m > n then [] else m : fromto (m + 1) n

The Tree Design Pattern (tree consumers)
----------------------------------------

< consume :: Tree A -> B
< consume Empty         =  ...
< consume (Node l a r)  =  ... l ... consume l ... a ... r ... consume r ...

Binary search trees
-------------------

> data Tree a = Empty | Node (Tree a) a (Tree a)
>   deriving (Show)

> leaf :: a -> Tree a
> leaf a  =  Node Empty a Empty
