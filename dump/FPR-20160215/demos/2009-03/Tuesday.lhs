> module Tuesday
> where
> import Prelude hiding ((||))

Parallelizing programs
----------------------

> factorial :: Integer -> Integer
> factorial n = if n == 0 then 1 else n * factorial (n - 1)

Specification:
    pfactorial l r = product [l .. r]

> pfactorial :: Integer -> Integer -> Integer
> pfactorial l r
>   | l >  r = 1
>   | l == r = l
>   | l <  r = pfactorial l n * pfactorial (n + 1) r
>   where n = div (l + r) 2

Hacking session
---------------

Let's help the world of finance.

Interface.

> data Transaction 
>   = Deposit    Integer 
>   | Withdrawal Integer deriving (Show)

> balance   :: [Transaction] -> Integer
> statement :: [Transaction] -> IO ()

Test data.

> account :: [Transaction]
> account =
>   [Deposit     500,
>    Deposit    1500,
>    Withdrawal  300,
>    Withdrawal  200,
>    Withdrawal  300,
>    Deposit    1500]

> intersperse :: a -> [a] -> [a]
> intersperse _   []              =  []
> intersperse _   [x]             =  [x]
> intersperse sep (x1 : x2 : xs)  =  x1 : sep : intersperse sep (x2 : xs)

> statement' :: [Transaction] -> IO ()
> statement' = putStr . concat . intersperse "\n" . map showTransaction

Implementation.

> val :: Transaction -> Integer
> val (Deposit    n) =   n
> val (Withdrawal n) = - n

> balance = sum . map val

> sum' :: [Integer] -> Integer
> sum' [] = 0
> sum' (x : xs) = x + sum' xs

> showTransaction :: Transaction -> String
> showTransaction (Deposit    n) = "Deposit   : " ++ showGBP n
> showTransaction (Withdrawal n) = "Withdrawal: " ++ showGBP n

> showGBP :: Integer -> String
> showGBP n = rjustify 7 (show n) ++ " GBP"

> rjustify :: Int -> String -> String
> rjustify n s = replicate (n - length s) ' ' ++ s

> statement = sequence_ . map (putStrLn . showTransaction)

Lazy evaluation
---------------

> (||) :: Bool -> Bool -> Bool

> False ||  b = b
> True  || _b = True

< False || False = False
< False || True  = True
< True  || False = True
< True  || True  = True

> member :: (Eq a) => a -> [a] -> Bool
> member _ []       = False
> member a (x : xs) = (a == x) || (member a xs)

foldr
-----

> compose :: [a -> a] -> (a -> a)
> compose = foldr (.) id

Efficiency
----------

> reverse1 :: [a] -> [a]
> reverse1 []      = []
> reverse1 (a : x) = reverse1 x ++ [a]

> reverse2 :: [a] -> [a]
> reverse2 x = reverseCat x []

> reverseCat :: [a] -> [a] -> [a]
> reverseCat []      y = y
> reverseCat (a : x) y = reverseCat x (a : y)
