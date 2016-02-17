These model answers include, where appropriate, alternative
definitions using list comprehensions and maps and folds.


QUESTION 1

> prod :: [Int] -> Int
> prod []     = 1
> prod (x:xs) = x * prod xs

Notice that the base case (for the empty list) is 1, not 0 as for the
function "sum". The relationship between 1 and * is that 1 is the unit
of *, that is, 1 * x = x * 1 = x for any x.  What happens if you make
the base case 0?

This can be defined elegantly using "foldr":

> prod' :: [Int] -> Int
> prod' xs = foldr (*) 1 xs

or again, using partial application,

> prod'' :: [Int] -> Int
> prod'' = foldr (*) 1


QUESTION 2

You might write "allTrue" as follows:

> allTrue :: [Bool] -> Bool
> allTrue [] = True
> allTrue (x:xs)
>   | x         = allTrue xs
>   | otherwise = False

but a little thought reveals that this is simply using guards to
achieve the effect of an "&&":

> allTrue' :: [Bool] -> Bool
> allTrue' []     = True
> allTrue' (x:xs) = x && allTrue' xs

and so is equivalent to a definition using "foldr":

> allTrue'' :: [Bool] -> Bool
> allTrue'' = foldr (&&) True

Notice that the base case is "True", the unit of "&&".


QUESTION 3

Similarly, 

> allFalse :: [Bool] -> Bool
> allFalse []     = True
> allFalse (x:xs) = not x && allFalse xs

or equivalently

> allFalse' :: [Bool] -> Bool
> allFalse' = foldr step True
>   where step x b = not x && b

or again, more concisely, 

> allFalse'' :: [Bool] -> Bool
> allFalse'' = allTrue . map not


QUESTION 4

> decAll :: [Int] -> [Int]
> decAll []     = []
> decAll (x:xs) = (x-1) : decAll xs

or equivalently 

> decAll' :: [Int] -> [Int]
> decAll' = map pred

(here, "pred" is the built-in predecessor function).

It can also be written using a list comprehension:

> decAll'' :: [Int] -> [Int]
> decAll'' xs = [ x-1 | x <- xs ]


QUESTION 5

> convertIntBool :: [Int] -> [Bool]
> convertIntBool [] = []
> convertIntBool (0:xs) = False : convertIntBool xs
> convertIntBool (_:xs) = True : convertIntBool xs

Instead of pattern-matching, we could combine the last two equations:

> convertIntBool' :: [Int] -> [Bool]
> convertIntBool' [] = []
> convertIntBool' (x:xs) = (x/=0) : convertIntBool' xs

so revealing that this is an instance of map:

> convertIntBool'' :: [Int] -> [Bool]
> convertIntBool'' = map (/=0)

This too can be written using a list comprehension:

> convertIntBool''' :: [Int] -> [Bool]
> convertIntBool''' xs = [ x/=0 | x <- xs ]


QUESTION 6

> pairUp :: [Int] -> [Char] -> [(Int,Char)]
> pairUp [] _          = []
> pairUp _ []          = []
> pairUp (x:xs) (y:ys) = (x,y) : pairUp xs ys

The first two equations can be combined, if we place them after the third:

> pairUp' :: [Int] -> [Char] -> [(Int,Char)]
> pairUp' (x:xs) (y:ys) = (x,y) : pairUp' xs ys
> pairUp' _ _           = []

I don't know of any much simpler way of writing this function.


QUESTION 7

> takePrefix :: Int -> [a] -> [a]
> takePrefix 0 xs     = []
> takePrefix n []     = []
> takePrefix n (x:xs) = x : takePrefix (n-1) xs

Notice that, like "pairUp", this analyzes both arguments in
parallel. Again, we can combine the first two equations (since they
have a common right-hand side), provided we place them after the
third:

> takePrefix' :: Int -> [a] -> [a]
> takePrefix' n (x:xs) = x : takePrefix' (n-1) xs
> takePrefix' _ _      = []


QUESTION 8

> dropPrefix :: Int -> [a] -> [a]
> dropPrefix 0 xs     = xs
> dropPrefix n []     = []
> dropPrefix n (x:xs) = dropPrefix (n-1) xs

This time, there is no benefit to be gained from rearranging the equations.


QUESTION 9

> member :: Eq a => [a] -> a -> Bool
> member [] y            = False
> member (x:xs) y | x==y = True
> member (_:xs) y        = member xs y

or, more simply,

> member' :: Eq a => [a] -> a -> Bool
> member' [] y     = False
> member' (x:xs) y = (x==y) || member xs y

or again,

> member'' :: Eq a => [a] -> a -> Bool
> member'' xs y = foldr (||) False (map (y==) xs)


QUESTION 10

This is rather like "pairUp" in structure. Perhaps the obvious definition is

> equals :: Eq a => [a] -> [a] -> Bool
> equals [] []          = True
> equals [] (y:ys)      = False
> equals (x:xs) []      = False
> equals (x:xs) (y:ys)  = (x==y) && equals xs ys

The middle two equations can be combined, if we place them at the end:

> equals' :: Eq a => [a] -> [a] -> Bool
> equals' [] []          = True
> equals' (x:xs) (y:ys)  = (x==y) && equals' xs ys
> equals' _ _            = False


QUESTION 11

> select :: [a] -> Int -> a
> select (x:xs) 0 = x
> select (x:xs) n = select xs (n-1)

Notice that this function is undefined on the empty list, and more
generally when the requested index is beyond the end of the list.


QUESTION 12

> largest :: [Int] -> Int
> largest [x]    = x
> largest (x:xs) = larger (x, largest xs)

> larger :: (Int,Int) -> Int
> larger (x,y) = if x>y then x else y

Notice that here, the base case is for singleton lists rather than
empty lists; it isn't clear what to return as the largest element of
the empty list. Therefore, the function is undefined for the empty
list, as neither equation matches; and the second is only applied when
the first desn't match, so is applied only to lists of length two or
more.

This can be defined using the built-in "foldr1", a fold for non-empty
lists. Instead of "larger" we can use the built-in "max", which is
curried:

> largest' :: [Int] -> Int
> largest' = foldr1 max


QUESTION 13

Similarly,

> smallest :: [Int] -> Int
> smallest [x]    = x
> smallest (x:xs) = smaller (x, smallest xs)

> smaller :: (Int,Int) -> Int
> smaller (x,y) = if x<y then x else y

> smallest' :: [Int] -> Int
> smallest' = foldr1 min
