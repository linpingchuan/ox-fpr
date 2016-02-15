ghci -Wall Tuesday.lhs

> module Tuesday
> where

Hacking session
---------------

> mysum :: [Integer] -> Integer
> mysum []        =  0
> mysum (a : as)  =  a + mysum as

> myproduct :: [Integer] -> Integer
> myproduct []        =  1
> myproduct (a : as)  =  a * myproduct as

Smallest element of a list of bounded values (minBound, maxBound).

> smallestBounded :: (Bounded a, Ord a) => [a] -> a
> smallestBounded []        =  maxBound
> smallestBounded (a : as)  =  a `min` smallestBounded as

Smallest element of a list, as a partial function.
Assumption: I am not called with the empty list.

> smallest :: [Integer] -> Integer
> smallest []        =  error "smallest [] undefined"
> smallest [a]       =  a
> smallest (a : as)  =  a `min` smallest as

Smallest element of a list, as a total function.

> data Exception a  =  Raise String | Return a
>  deriving (Show)

> smallest'  :: [Integer] -> Exception Integer
> smallest'  =  foldr min' (Raise "smallest [] undefined")

> min' :: Integer -> Exception Integer -> Exception Integer
> min' a (Raise _s)  =  Return a
> min' a (Return b)  =  Return (a `min` b)

Booleans as numbers.

> instance Num Bool where
>  (+)  =  (||)
>  (*)  =  (&&)
>  fromInteger 0  =  False
>  fromInteger _  =  True