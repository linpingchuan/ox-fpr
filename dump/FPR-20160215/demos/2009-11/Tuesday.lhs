ghci Tuesday.lhs

> module Tuesday
> where
> import Prelude -- hiding ((||))

Hacking session
---------------

Determine whether a string occurs in a text.

sublist "fun" "I love functional programming!" == True
sublist "fun" "functional" == True

sublist s t <=> exists x y . x ++ s ++ y = t

Failed attempt:

 sublist  [] s      = True
 sublist (a : as) s = ... sublist as s ...

Successful attempt:

> sublist :: (Eq a) => [a] -> [a] -> Bool
> sublist s []       = null s
> sublist s (a : as) = prefix s (a : as) || sublist s as

prefix  "fun" "functional" == True

> prefix :: (Eq a) => [a] -> [a] -> Bool
> prefix []         _bs      = True
> prefix (_a : _as) []       = False
> prefix (a : as)   (b : bs) = a == b && prefix as bs

Type classes
------------

Turning lists into numbers (non-deterministic computations).

> instance (Num a) => Num [a] where
>   x + y         = [ a + b | a <- x, b <- y ]
>   x * y         = [ a * b | a <- x, b <- y ]
>   abs x         = [ abs a | a <- x ]
>   signum x      = [ signum a | a <- x ]
>   fromInteger n = [fromInteger n]

1 + [2, 3, 5, 7]
[0 .. 2] + [0 .. 5]
[0 .. 3] * [0 .. 4]
