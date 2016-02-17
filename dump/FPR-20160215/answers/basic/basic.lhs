QUESTION 1

Function "quad" raises its argument to the fourth power:

> quad :: Integer -> Integer
> quad x = square (square x)

It uses function "square" from the lectures:

> square :: Integer -> Integer
> square x  = x * x


Function "larger" returns the larger of its two arguments:

> larger :: (Integer,Integer) -> Integer
> larger (x,y) = if x>y then x else y

or alternatively,

  larger :: (Integer,Integer) -> Integer
  larger (x,y) 
    | x>y       = x
    | otherwise = y

(Actually, the type in both cases is the more general 

  Ord a => (a,a) -> a

as you can verify by omitting the type declaration and asking for the
inferred type with ":t larger".  This means that it works for any
ordered type a, not just for Integers. We will say more about this
overloading mechanism later.)

Function "area" computes the area of a circle, given its radius:

> area :: Float -> Float
> area r = pi * r * r


QUESTION 2

Applicative-order reductions first:

    first (42, double (add 1 2))
  = first (42, double (1+2))
  = first (42, double 3)
  = first (42, 3+3)
  = first (42, 6)
  = 42

    first (42, double (add 1 infinity))
  = first (42, double (1 + infinity))
  = first (42, double (1 + (1 + infinity)))
  = ...

    first (infinity, double (add 1 2))
  = first (1+infinity, double (add 1 2))
  = ...

    add (cond (True, 42, 1+infinity)) 4
  = add (cond (True, 42, 1+(1+infinity))) 4
  = ...

    twice (double, add 1 2)
  = twice (double, 1+2)
  = twice (double, 3)
  = double (double 3)
  = double (3+3)
  = double 6
  = 6+6
  = 12

    twice (add 1, 0)
  = add 1 (add 1 0)
  = add 1 (1+0)
  = add 1 1
  = 1+1
  = 2

And normal-order reductions:

    first (42, double (add 1 2))
  = 42
terminating straight away, rather than in several steps

    first (42, double (add 1 infinity))
  = 42
terminating straight away, rather than diverging

    first (infinity, double (add 1 2))
  = infinity
  = 1+infinity
  = ...
not terminating - normal-order isn't magic!

    add (cond (True, 42, 1+infinity)) 4
  = cond (True, 42, 1+infinity) + 4
  = (if True then 42 else 1+infinity) + 4
  = 42 + 4
  = 46
terminating

    twice (double, add 1 2)
  = double (double (add 1 2))
  = double (add 1 2) + double (add 1 2)
  = (add 1 2 + add 1 2) + double (add 1 2)
  = ((1+2) + add 1 2) + double (add 1 2)
  = (3 + add 1 2) + double (add 1 2)
  = (3 + (1+2)) + double (add 1 2)
  = (3 + 3) + double (add 1 2)
  = 6 + double (add 1 2)
  = 6 + (add 1 2 + add 1 2)
  = ... { repeated steps }
  = 6 + 6
  = 12
still terminating, but takes a lot longer

    twice (add 1, 0)
  = add 1 (add 1 0)
  = 1 + add 1 0
  = 1 + (1+0)
  = 1 + 1
  = 2
not very different


QUESTION 3

Using the definition,

> fact 0     = 1
> fact (n+1) = (n+1) * fact n

we have, with normal-order reduction,

  fact 3 
= 3 * fact 2
= 3 * (2 * fact 1)
= 3 * (2 * (1 * fact 0))
= 3 * (2 * (1 * 1))
= 3 * (2 * 1)
= 3 * 2
= 6

How would it differ with applicative-order reduction?
