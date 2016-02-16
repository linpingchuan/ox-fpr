1 Basic definitions
===================

1.a. define function square

> square :: Integer -> Integer
> square x = x * x

now define a function quad in terms of square

> quad :: Integer -> Integer
> quad x = square (square x)

1.b. define function larger

> larger :: (Integer, Integer) -> Integer
> larger (x,y) = if (x > y) then x else y

1.c. compute area of circle

> areaOfCircle :: Integer -> Double
> areaOfCircle r = pi * fromInteger (square r)

2.a. first 42 (double (add 1 2))

Applicative order reduction

= first 42 (double (1 + 2))
= first 42 (double (3))
= first 42 (3 + 3)
= first 42 (3 + 3)       -- this is slightly inaccurate. refer to model solution
= 42

Normal order reduction

= first 42 (double (1 + 2))
= 42

2.b. first 42 (double (add 1 infinity))

Applicative order reduction
(Verified with JG, the model solution is not accurate. The one below is!)

= first 42 (double (add 1 (1+infinity)))
= first 42 (double (add 1 (1+(1+infinity))))
= ...

Normal order reduction

= first 42 (double (1 + infinity))
= first 42 ((1 + infinity) + (1 + infinity))
= 42

2.c. first infinity (double (add 1 2))

Applicative order reduction - WRONG. refer to model solution.

= first infinity (double (1 + 2))
= first infinity (double 3)
= first infinity (3 + 3)
= first infinity 6
= first (1 + infinity) 6
= ...

Normal order reduction

= first infinity (double (1 + 2))
= first infinity ((1 + 2) + (1 + 2))
= first (1 + infinity) ((1 + 2) + (1 + 2))
= ...

2.d. add (cond True 42 (1 + infinity)) 4

Applicative order reduction

= add (cond True 42 (1 + (1 + infinity))) 4
= ...

Normal order reduction

= add (if True then 42 else (1 + infinity)) 4
= add 42 4
= 46

2.e. twice double (add 1 2)

Applicative order reduction

= twice double (1 + 2)
= twice double 3
= double (double 3)
= double (3 + 3)
= double 6
= 6 + 6
= 12

Normal order reduction

= double (double (1 + 2))
= double ((1 + 2) + (1 + 2))
= ((1 + 2) + (1 + 2)) + ((1 + 2) + (1 + 2))

2.f. twice (add 1) 0

Applicative order reduction

= add 1 (add 1 0)
= add 1 (1 + 0)
= 1 + 1
= 2

Normal order reduction

= add 1 (add 1 0)
= add 1 (1 + 0)
= 1 + (1 + 0)
= 2

3. factorial function reduction

fact 3
= 3 * fact 2
= 3 * 2 * fact 1
= 3 * 2 * 1 * fact 0
= 3 * 2 * 1 * 1
= 6[\
