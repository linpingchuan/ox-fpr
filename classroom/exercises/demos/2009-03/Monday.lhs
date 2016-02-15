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

> insertsort :: [Integer] -> [Integer]
> insertsort []      = []
> insertsort (a : x) = insert a (insertsort x)

> insert :: Integer -> [Integer] -> [Integer]
> insert a []   = [a]
> insert a (b : x)
>   | a <= b    = a : b : x
>   | otherwise = b : insert a x

List membership (different variants)
------------------------------------

Using `if-then-else':

> member1 :: Integer -> [Integer] -> Bool
> member1 a []       = False
> member1 a (x : xs) = if a == x then True else member1 a xs

Using `guards':

> member2 :: Integer -> [Integer] -> Bool
> member2 a []       = False
> member2 a (x : xs)
>   | a == x    = True
>   | otherwise = member2 a xs

Using `disjunction':

> member3 :: Integer -> [Integer] -> Bool
> member3 a []       = False
> member3 a (x : xs) = a == x || member3 a xs

Smallest element of a list
--------------------------

> smallest :: [Integer] -> Integer
> smallest []       = error "smallest []"
> smallest [x]      = x
> smallest (x : xs) = min x (smallest xs)

More list processing
--------------------

> append :: [a] -> [a] -> [a]
> append [] y       = y
> append (x : xs) y = x : append xs y

> puzzle1 :: [a] -> [a]
> puzzle1 []       = []
> puzzle1 (x : xs) = puzzle1 xs ++ [x]

> identity :: a -> a
> identity x = x

Evaluation order
----------------

> cond :: Bool -> a -> a -> a
> cond True  t e = t
> cond False t e = e

> factorial :: Integer -> Integer
> factorial n = cond (n == 0) 1 (n * factorial (n - 1))

> three :: Integer -> Integer
> three x = 3

> infinity :: Integer
> infinity = 1 + infinity

> square :: Integer -> Integer
> square x = x * x

< repeat x = x : repeat x

< take 0 x             = []
< take (n + 1) []      = []
< take (n + 1) (a : x) = a : take n x

Higher-order functions
----------------------

> puzzle2 :: ([Char] -> [Char]) -> [Char]
> puzzle2 f = f "hello" ++ "world"

Try `puzzle2 reverse'.

Input and output
----------------

< return :: a -> IO a
< (>>=)  :: IO a -> (a -> IO b) -> IO b
< (>>)   :: IO a -> IO b -> IO b

> sequ :: [IO a] -> IO ()
> sequ []       = return ()
> sequ (x : xs) = x >> sequ xs

> main :: IO ()
> main = sequ [ print i | i <- [0 .. 99] ]