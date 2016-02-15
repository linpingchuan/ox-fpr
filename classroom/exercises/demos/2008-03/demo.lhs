Monday.
-------

> hi :: String
> hi = "Hello, world!"

> fact :: (Integral a) => a -> a
> fact 0       = 1
> fact (n + 1) = (n + 1) * fact n

Tuesday.
--------

> inserts :: a -> [a] -> [[a]]
> inserts a [] 
>   = [[a]]
> inserts a (b : bs) 
>   = (a : b : bs) : [ b : cs | cs <- inserts a bs ]

> perms :: [a] -> [[a]]
> perms [] 
>   =  [[]]
> perms (a : as)
>   =  [ cs | bs <- perms as, cs <- inserts a bs ]

Wednesday.
----------

Mergesort.

> data Tree a = Empty | Leaf a | Fork (Tree a) (Tree a)
>   deriving (Eq, Ord, Show)

> mergeSort :: (Ord a) => Tree a -> [a]
> mergeSort Empty      = []
> mergeSort (Leaf a)   = [a]
> mergeSort (Fork t u) = merge (mergeSort t) (mergeSort u)

> merge :: (Ord a) => [a] -> [a] -> [a]
> merge [] bs = bs
> merge as [] = as
> merge (a : as) (b : bs)
>   | a <= b    = a : merge     as (b : bs)
>   | otherwise = b : merge (a: as)     bs

Binary numbers.

> data Bit = O | I
>   deriving (Eq, Ord, Show)

> inc ::[Bit] -> [Bit]
> inc [] = [I]
> inc (O : ds) = I : ds
> inc (I : ds) = O : inc ds

Thursday.
---------

> data List a = Nil | Zero (List (a, a)) | One a (List (a, a))
>   deriving (Show)

> cons :: a -> List a -> List a
> cons a Nil        = One a Nil
> cons a (Zero  xs) = One a xs
> cons a (One b xs) = Zero (cons (a, b) xs)

> index :: Integer -> List a -> Maybe a
> index _n Nil              = Nothing
> index n (Zero xs)
>   | n `mod` 2 == 0        = fmap fst (index (n `div` 2) xs)
>   | otherwise             = fmap snd (index (n `div` 2) xs)
> index 0 (One a _xs)       = Just a
> index (n + 1) (One _a xs) = index n (Zero xs)

> twice :: (a -> a) -> (a -> a)
> twice f = f . f

> reverseCat :: [a] -> [a] -> [a]
> reverseCat [] ys       = ys
> reverseCat (x : xs) ys = reverseCat xs (x : ys)

Friday.
-------

> inorderCat :: Tree a -> [a] -> [a]
> inorderCat Empty      = id
> inorderCat (Leaf a)   = (a :)
> inorderCat (Fork l r) = inorderCat l . inorderCat r

> tree :: [a] -> Tree a
> tree xs = fst (build (length xs) xs)

> build :: Int -> [a] -> (Tree a, [a])
> build 0 xs = (Empty, xs)
> build 1 xs = (Leaf (head xs), tail xs)
> build n xs = (Fork l r, zs)
>   where m       = n `div` 2
>         (l, ys) = build m       xs
>         (r, zs) = build (n - m) ys

