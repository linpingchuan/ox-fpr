2 Basic types
=============

Hide stuff

> import Data.Char
> import Prelude hiding ((&&), (||))

2.1 Define (||)

> (||) :: Bool -> Bool -> Bool
> True || _ = True
> False || x = x

define (&&)

> (&&) :: Bool -> Bool -> Bool
> True && x = x
> False && _ = False

2.2 Condition expression

> (&&&) :: Bool -> Bool -> Bool
> x &&& y = if x then y else False

Guarded equations

> (&&&&) :: Bool -> Bool -> Bool
> x &&&& y
>   | x = y
>   | y = x

2.3 charToNum

> charToNum :: Char -> Int
> charToNum c = ord c - ord '0'

2.4 showDate -- WRONG

> showDayWithOrdinal :: Integer -> String
> showDayWithOrdinal x
>   | x `mod` 10 == 1 = show x ++ "st"
>   | x `mod` 10 == 2 = "2nd"
>   | x == 3 = "3rd"
>   | otherwise = show x ++ "th"
> showDate :: Integer -> Integer -> Integer -> String
> showDate d m y = showDayWithOrdinal d ++
>                  " " ++
>                  (month!!fromInteger (m-1)) ++
>                  " " ++
>                  show y
>   where month = ["January", "February", "March", "April", "May", "June",
>                  "July", "August", "September", "October", "November",
>                  "December"]
