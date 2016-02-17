ghc -c -O2 Thursday.lhs
ghci Thursday.lhs +RTS -K64M

> module Thursday
> where
> import Prelude hiding (lookup)
> import Ratio

Random-access lists
-------------------

> data Sequ a
>   = Nil 
>   | Zero  (Sequ (a, a))
>   | One a (Sequ (a, a))
>   deriving (Show)

> cons     :: a -> Sequ a -> Sequ a
> fromList :: [a] -> Sequ a
> (!)      :: Sequ a -> Int -> a
> lookup   :: Sequ a -> Int -> Maybe a
> size     :: Sequ a -> Int

sum [ [0..99999]!!i | i <-[0..99999] ]
let s = fromList (map square [0..99999])
size s
s!99999
sum [ s!i | i <-[0..99999] ]

> square :: Integer -> Integer
> square x = x * x

> cons a Nil       = One a Nil
> cons a (Zero s)  = One a s
> cons a (One b s) = Zero (cons (a, b) s)

> fromList = foldr cons Nil

> Nil     ! n       = error "(!): index too large"
> Zero s  ! n       = sel (n `mod` 2) (s ! (n `div` 2))
> One a s ! 0       = a
> One a s ! (n + 1) = Zero s ! n

> sel :: Int -> (a, a) -> a
> sel 0 (a, b) = a
> sel 1 (a, b) = b

< s ! n = case lookup s n of
<         Nothing -> error "(!): index too large"
<         Just a  -> a

> lookup Nil       n       = Nothing
> lookup (Zero s)  n       = fmap (sel (n `mod` 2)) (lookup s (n `div` 2))
> lookup (One a s) 0       = Just a
> lookup (One a s) (n + 1) = lookup (Zero s) n

> size Nil       = 0
> size (Zero s)  =     2 * size s
> size (One _ s) = 1 + 2 * size s

Help your manager
-----------------

> type Time = Integer

> data Task
>   = Phone   Time
>   | Program Time
>   | Idle    Time
>   deriving (Show, Eq, Ord)

> data Tempo a
>   = Prim a
>   | Tempo a :+: Tempo a
>   | Tempo a :=: Tempo a
>   deriving (Show, Eq, Ord)

> phone, program, idle :: Time -> Tempo Task
> phone   t = Prim (Phone   t)
> program t = Prim (Program t)
> idle    t = Prim (Idle    t)

> monday, monday' :: Tempo Task
> monday
>   = phone 12 :+: ((phone 8 :+: idle 22) :=: program 30)
> monday'
>   = phone 12 :+: (phone 8 :=: program 8) :+: program 22

> dur :: Task -> Time
> dur (Phone   t) = t
> dur (Program t) = t
> dur (Idle    t) = t

> duration :: Tempo Task -> Time
> duration (Prim a)    = dur a
> duration (m1 :+: m2) = duration m1 + duration m2
> duration (m1 :=: m2) = duration m1 `max` duration m2

> cst :: Task -> Ratio Time
> cst (Phone   t) = t % 1
> cst (Program t) = 2 * t % 1
> cst (Idle    t) = 0

> cost :: Tempo Task -> Rational
> cost (Prim a)    = cst a
> cost (m1 :+: m2) = cost m1 + cost m2
> cost (m1 :=: m2) = 0.5 * cost m1 + 0.5* cost m2
