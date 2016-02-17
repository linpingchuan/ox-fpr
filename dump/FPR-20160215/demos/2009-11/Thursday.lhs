ghc --make -O2 Thursday.lhs
ghci -Wall Thursday

> module Thursday
> where
> import Prelude hiding (reverse)
> import qualified Monday
> import Wednesday (insertsort, selectsort)
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

Generators and properties
-------------------------

< type Prop :: * -> *
< type Gen  :: * -> *

> infixr 4  <*>, /\
> infixr 2  \/
> infixr 1  -->, ==>

> (-->)   :: Gen a -> Prop b -> Prop (a -> b)
> (==>)   :: Gen a -> (a -> Prop b) -> Prop (a -> b)

> (/\)    :: Prop a -> Prop a -> Prop a
> like    :: (Eq b) => (a -> b) -> a -> Prop b
> ordered :: (Ord a) => Prop [a]

> (\/)    :: Gen a -> Gen a -> Gen a
> (<*>)   :: Gen a -> Gen b -> Gen (a, b)
> perms   :: [a] -> Gen [a]

Examples
--------

> lists :: Gen [Integer]
> lists = [[], [4,7,1,1], [999,998..0]] \/ perms [1..8]

> sorter :: Prop ([Integer] -> [Integer])
> sorter = lists --> ordered

([[], [4,7,1,1], [999,998..0]] --> ordered) insertsort
(perms [1..8] --> ordered) insertsort
(lists --> ordered) insertsort
(lists ==> \ x y -> sort x == y) insertsort
(lists ==> like sort) insertsort
all sorter [insertsort, selectsort]
([1..9] --> [[1..9]] --> ordered) Monday.insert

Consider the function isqrt, which calculates the integer square root.

> isqrt :: Integer -> Integer
> isqrt n = loop 0 3 1
>   where loop i k s | s <= n    = loop (i + 1) (k + 2) (s + k)
>                    | otherwise = i

> spec:: Prop (Integer -> Integer)
> spec = [0..99999] ==> \ n r -> r * r <= n && n < (r + 1) * (r + 1)

It is not immediately obvious that this definition is correct.

Properties of generators and properties
---------------------------------------

  (g1 --> p) /\ (g2 --> p)  =  g1 \/ g2 --> p
  (g --> p1) /\ (g --> p2)  =  g --> p1 /\ p2
  (g1 <*> g2 --> p) f       =  (g1 --> g2 --> p) (curry f)

Implementation
--------------

> type Prop a = a -> Bool
> type Gen a = [a]

> g --> p = \ f -> all p (map f g)
> g ==> p = \ f -> and [ p x (f x) | x <- g ]

> like f = \ x y -> f x == y

> g1 <*> g2 = [ (a1, a2) | a1 <- g1, a2 <- g2 ]

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

List reversal
-------------
Program derivation
------------------

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
