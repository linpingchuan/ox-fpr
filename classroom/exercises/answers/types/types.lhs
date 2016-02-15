> import Char

QUESTION 1

The definition of || from the standard prelude is

  False || x = x
  True || x  = True

Note that this is non-strict in the second argument, so that (for example)

  (x==0) || (y/x > 1)

is well-defined even when x=0.


QUESTION 2

Using conditional expressions, we could define

  x && y = if x then y else False
  x || y = if x then True else y

Notice that these are different from

  x && y = if y then x else False
  x || y = if y then True else x

because of strictness - that is, && and || are not commutative.
(Can you give contexts in which these definitions give different results?)

Using conditional (guarded) definitions, we could write equivalently

  x && y
    | x         = y
    | otherwise = False

  x || y
    | x         = True
    | otherwise = y

which again are different from the similar programs in which the guard
depends on y instead of x.


QUESTION 3

A pattern-matching definition of "charToNum" is simple but tedious:

> charToNum :: Char -> Integer
> charToNum '0' = 0
> charToNum '1' = 1
> charToNum '2' = 2
> charToNum '3' = 3
> charToNum '4' = 4
> charToNum '5' = 5
> charToNum '6' = 6
> charToNum '7' = 7
> charToNum '8' = 8
> charToNum '9' = 9

A better definition uses the function "ord":

> charToNum' c
>   | '0' <= c && c <= '9' = ord c - ord '0'


QUESTION 4

The simple definition of "showDate" uses these three components:

> showDay :: Integer -> String
> showDay d = show d

> showYear :: Integer -> String
> showYear y = show y

> showMonth :: Integer -> String
> showMonth 1 = "January"
> showMonth 2 = "February"
> showMonth 3 = "March"
> showMonth 4 = "April"
> showMonth 5 = "May"
> showMonth 6 = "June"
> showMonth 7 = "July"
> showMonth 8 = "August"
> showMonth 9 = "September"
> showMonth 10 = "October"
> showMonth 11 = "November"
> showMonth 12 = "December"

> showDate :: Integer -> Integer -> Integer -> String
> showDate d m y = showDay d ++ " " ++ showMonth m ++ " " ++ showYear y

(There are more elegant ways of defining showMonth.)

More elaborately, we could display the day number as an ordinal:

> showDayOrdinal :: Integer -> String
> showDayOrdinal d = showDay d ++ suffix (d `div` 10) (d `mod` 10)
>   where 
>     suffix 1 _ = "th"
>     suffix _ 1 = "st"
>     suffix _ 2 = "nd"
>     suffix _ 3 = "rd"
>     suffix _ _ = "th"


