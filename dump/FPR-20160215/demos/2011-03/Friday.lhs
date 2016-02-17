ghc -Wall Friday.lhs

> {-# LANGUAGE NPlusKPatterns #-}
> module Friday
> where
> import Prelude hiding (lookup)

Random-access lists
-------------------

Interface
---------

> empty    :: Sequ a
> cons     :: a -> Sequ a -> Sequ a
> fromList :: [a] -> Sequ a
> (!)      :: Sequ a -> Int -> a
> lookup   :: Sequ a -> Int -> Maybe a
> size     :: Sequ a -> Int

Examples
--------

:set +s
let x = [0 .. 99999]
sum [ x!!i | i <- [0 .. 99999] ]
let s = fromList x
size s
s!99999
sum [ s!i | i <-[0 .. 99999] ]

Implementation
--------------

> data Sequ a
>   =  Nil 
>   |  Zero  (Sequ (a, a))
>   |  One a (Sequ (a, a))
>   deriving (Show)

> empty  =  Nil

> cons a Nil        =  One a Nil
> cons a (Zero s)   =  One a s
> cons a (One b s)  =  Zero (cons (a, b) s)

> fromList = foldr cons empty

> Nil       ! _n       =  error "(!): index too large"
> Zero s    ! n        =  sel (n `mod` 2) (s ! (n `div` 2))
> One  a _s ! 0        =  a
> One _a  s ! (n + 1)  =  Zero s ! n

> sel :: Int -> (a, a) -> a
> sel 0 (a, _b)  =  a
> sel 1 (_a, b)  =  b

> lookup Nil         _n       =  Nothing
> lookup (Zero s)     n       =  fmap (sel (n `mod` 2)) (lookup s (n `div` 2))
> lookup (One  a _s) 0        =  Just a
> lookup (One _a  s) (n + 1)  =  lookup (Zero s) n

> size Nil        =  0
> size (Zero s)   =      2 * size s
> size (One _ s)  =  1 + 2 * size s
