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

Quicksort
---------

From the slides:

> quicksort :: (Ord a) => [a] -> [a]
> quicksort []      = []
> quicksort (a : x) = quicksort littles ++ [a] ++ quicksort bigs
>   where  littles  = [ b | b <- x, b <  a ]
>          bigs     = [ b | b <- x, b >= a ]

Ex. List membership.

  member "Ralf" ["Anja", "Lisa", "Ralf", "Florian"] == True
  member "Ralf" ["Anja", "Lisa", "Ralph", "Florian"] == False

> member :: (Eq a) => a -> [a] -> Bool
> member _a []      = False
> member a (b : bs) = a == b ||  member a bs

Ex. Smallest element of a list.

> smallest1 :: [Int] -> Int
> smallest1 []       = maxBound
> smallest1 (a : as) = min a (smallest1 as)

> smallest2 :: [Integer] -> Integer
> smallest2 []       = error "I refuse"
> smallest2 [a]      = a
> smallest2 (a : as) = min a (smallest2 as)

> data Result = None | Some Integer
>   deriving (Show)

> smallest3 :: [Integer] -> Result
> smallest3 []       = None
> smallest3 (a : as) = min' a (smallest3 as)

> min' :: Integer -> Result -> Result
> min' m None   =  Some m
> min' m (Some n) = Some (min m n)

Lazy evaluation
---------------

> from :: (Num a) => a -> [a]
> from n = n : from (n + 1)

Cook your own conditional.

> ifthenelse :: Bool -> a -> a -> a
> ifthenelse False _t e = e
> ifthenelse True  t _e = t

> fac :: (Num a) => a -> a
> fac n = ifthenelse (n == 0) 1 (n * fac (n - 1))

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
> member1 _k Empty       = False
> member1 k (Node l a r) = k == a || member1 k l || member1 k r

`member2' assumes that the binary tree is a search tree; its
running-time proportional to the height of the tree.

> member2 :: (Ord a) => a -> Tree a -> Bool
> member2 _k Empty = False
> member2 k (Node l a r)
>   | k <  a = member2 k l
>   | k == a = True
>   | k >  a = member2 k r






















> inorder :: Tree a -> [a]
> inorder Empty         =  []
> inorder (Node l a r)  =  inorder l ++ [a] ++ inorder r
>
> insert' :: (Ord a) => Tree a -> a -> Tree a
> insert' Empty k  = Node Empty k Empty
> insert' (Node l a r) k
>   | k <= a      = Node (insert' l k) a r
>   | otherwise   = Node l a (insert' r k)

