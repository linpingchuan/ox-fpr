> module Wednesday
> where
> import Prelude hiding ((.))

Fold and unfolds revisited
--------------------------

> data List a b = Nil | Cons a b

> fold :: (List a b -> b) -> ([a] -> b)
> fold inn []      = inn Nil
> fold inn (a : x) = inn (Cons a (fold inn x))

> sum' :: (Num a) => [a] -> a
> sum' = fold (\ x -> case x of 
>                     Nil      -> 0
>                     Cons a b -> a + b)

> unfold :: (b -> List a b) -> (b -> [a])
> unfold out x
>   = case out x of
>       Nil       -> []
>       Cons a x' -> a : unfold out x'

> enumFromTo' :: (Enum a) => a -> a -> [a]
> enumFromTo' m n 
>   = unfold (\ i -> if fromEnum i > fromEnum n
>                    then Nil
>                    else Cons i (succ i)) m

> map' :: (a -> b) -> ([a] -> [b])
> map' f = unfold (\ x -> case x of
>                         []     -> Nil
>                         a : x' -> Cons (f a) x')

Sorting
-------

> insert :: (Ord a) => List a [a] -> [a]
> insert Nil         = []
> insert (Cons x []) = [x]
> insert (Cons x (y : ys))
>   | x <= y         = x : y : ys
>   | otherwise      = y : insert (Cons x ys)

> insertsort :: (Ord a) => [a] -> [a]
> insertsort = fold insert

> deleteMin :: (Ord a) => [a] -> List a [a]
> deleteMin [] = Nil
> deleteMin (x : xs)
>   = case deleteMin xs of
>       Nil           -> Cons x []
>       Cons y ys
>         | x <= y    -> Cons x (y : ys)
>         | otherwise -> Cons y (x : ys)

> selectsort :: (Ord a) => [a] -> [a]
> selectsort = unfold deleteMin

Hacking session
---------------

        Testing shows the presence, 
        not the absence of bugs.
                 Edsger W. Dijkstra

  Beware of bugs in the above code; 
  I have only proved it correct,
  not tried it.
                    Donald E. Knuth

Test data generators.

-  permutations
-  Booleans, characters, integers, pairs, lists, trees

perms "RALF"

> testSort :: (Eq a) => ([a] -> [a]) -> ([a] -> Bool)
> testSort sort xs
>   = and [ sort p == xs | p <- perms xs ]

pair nat upper 3
pair nat (pair int upper) 3
list upper 3
list (list upper) 4

> inserts :: a -> [a] -> [[a]]
> inserts a []
>   = [[a]]
> inserts a (b : bs)
>   = (a : b : bs) : [ b : cs | cs <- inserts a bs ]

> perms :: [a] -> [[a]]
> perms []
>   =  [[]]
> perms (a : as)
>   =  [ cs | bs <- perms as, cs <- inserts a bs ]

Test data generators.

> type Generator a = Int -> [a]

NB. The list should contain at least one element.

> bool :: Generator Bool
> bool n = [False, True]

> upper :: Generator Char
> upper n = take (n + 1) ['A' .. 'Z']

> nat :: Generator Int
> nat n = [0 .. n + 1]

> int :: Generator Int
> int n = [-n .. n]

> pair :: Generator a -> Generator b -> Generator (a, b)
> pair a b n = [ (x, y) | x <- a n, y <- b n ]

> list :: Generator a -> Generator [a]
> list a 0       = [[]]
> list a (n + 1) = [ x : xs | x <- a n, xs <- list a n ]

> data Tree a = Empty | Node (Tree a) a (Tree a)
>   deriving (Show)

> tree :: Generator a -> Generator (Tree a)
> tree a 0       = [Empty]
> tree a (n + 1) = [ Node l x r | l <- tree a n, x <- a n, r <- tree a n ]

Doing it with class.

gen 2 :: [Bool]
gen 2 :: [Int]
gen 2 :: [[Bool]]
gen 2 :: [(Int, Bool)]
gen 2 :: [(Int, [Bool])]

> class Gen a where
>   gen :: Generator a

> instance Gen Bool where
>   gen = bool
> instance Gen Int where
>   gen = int
> instance (Gen a, Gen b) => Gen (a, b) where
>   gen = pair gen gen
> instance (Gen a) => Gen [a] where
>   gen = list gen
> instance (Gen a) => Gen (Tree a) where
>   gen = tree gen

Note the extensive use of currying.
