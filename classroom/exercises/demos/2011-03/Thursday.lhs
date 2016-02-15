ghci -Wall Thursday

> module Thursday
> where
> import Prelude hiding (reverse)
> import Monday (insert, insertsort)
> import Wednesday (mergeSort)
> import List (sort)

Hacking session
---------------

        Testing shows the presence, 
        not the absence of bugs.
                 Edsger W. Dijkstra

  Beware of bugs in the above code; 
  I have only proved it correct,
  not tried it.
                    Donald E. Knuth

Input generators and properties
-------------------------------

< type Prop :: * -> *
< type Inp  :: * -> *

> infixr 4  <*>, /\
> infixr 2  \/
> infixr 1  -->, ==>

> (-->)   :: Inp a -> Prop b -> Prop (a -> b)
> (==>)   :: Inp a -> (a -> Prop b) -> Prop (a -> b)

> (/\)    :: Prop a -> Prop a -> Prop a
> like    :: (Eq b) => (a -> b) -> a -> Prop b
> ordered :: (Ord a) => Prop [a]

> (\/)    :: Inp a -> Inp a -> Inp a
> (<*>)   :: Inp a -> Inp b -> Inp (a, b)
> perms   :: [a] -> Inp [a]

Examples
--------

([[], [4,7,1,1], [999,998..0]] --> ordered) insertsort
(perms [1..8] --> ordered) insertsort
([1..9] --> [[1..9]] --> ordered) insert

> lists :: Inp [Integer]
> lists = [[], [4,7,1,1], [999,998..0]] \/ perms [1..8]

> sorter :: Prop ([Integer] -> [Integer])
> sorter = lists --> ordered

(lists --> ordered) insertsort
(lists ==> \ x y -> sort x == y) insertsort
(lists ==> like sort) insertsort
all sorter [insertsort, mergeSort]

Consider the function isqrt, which calculates the integer square root.

> isqrt :: Integer -> Integer
> isqrt n = loop 0 3 1
>   where loop i k s | s <= n    = loop (i + 1) (k + 2) (s + k)
>                    | otherwise = i

> spec:: Prop (Integer -> Integer)
> spec = [0..9999] ==> \ n r -> r * r <= n && n < (r + 1) * (r + 1)

It is not immediately obvious that this definition is correct.

spec isqrt

Properties of generators and properties
---------------------------------------

  (g1 --> p) /\ (g2 --> p)  =  g1 \/ g2 --> p
  (g --> p1) /\ (g --> p2)  =  g --> p1 /\ p2
  (g1 <*> g2 --> p) f       =  (g1 --> g2 --> p) (curry f)

Implementation
--------------

> type Prop a  =  a -> Bool
> type Inp a   =  [a]

> g --> p  =  \ f -> all p (map f g)
> g ==> p  =  \ f -> and [ p x (f x) | x <- g ]

> like f  =  \ x y -> f x == y

> g1 <*> g2  =  [ (a1, a2) | a1 <- g1, a2 <- g2 ]

> g1 \/ g2 = g1 ++ g2

> p1 /\ p2 = \ x -> p1 x && p2 x

> ordered []                 = True
> ordered [_a]               = True
> ordered (a1 : as@(a2 : _)) = a1 <= a2 && ordered as

> inserts :: a -> [a] -> [[a]]
> inserts a []       = [[a]]
> inserts a (b : bs) = (a : b : bs) : [ b : cs | cs <- inserts a bs ]

> perms []       = [[]]
> perms (a : as) = [ cs | bs <- perms as, cs <- inserts a bs ]
