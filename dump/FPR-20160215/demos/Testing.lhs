ghc --make -O2 Wednesday.lhs
ghci -Wall Wednesday.lhs

> module Wednesday
> where
> import Monday (insert, insertsort)

Hacking session
---------------

        Testing shows the presence, 
        not the absence of bugs.
                 Edsger W. Dijkstra

  Beware of bugs in the above code; 
  I have only proved it correct,
  not tried it.
                    Donald E. Knuth

Generators and properties.

< type Prop :: * -> *
< type Gen  :: * -> *

> infixr 2  \/
> infixr 1  -->

> gen     :: [a] -> Gen a
> (\/)    :: Gen a -> Gen a -> Gen a
> perms   :: [a] -> Gen [a]

> ordered :: (Ord a) => Prop [a]
> (-->)   :: Gen a -> Prop b -> Prop (a -> b)

:m +Monday
test (gen [[], [4,7,1,1], [999,998..0]] --> ordered) insertsort
test (perms [1..8] --> ordered) insertsort
test (gen [[], [4,7,1,1], [999,998..0]] \/ perms [1..8] --> ordered) insertsort

test (gen [1..9] --> gen [[1..9]] --> ordered) insert



















Implementation.

> newtype Prop a = Prop { test :: a -> Bool }

> true    :: Prop a
> true = Prop (\ a -> True)

> newtype Gen a = Gen { list :: [a] }

> gen = Gen

> g1 \/ g2 = Gen (list g1 ++ list g2)

> g --> p = Prop (\ f -> all (test p) (map f (list g)))

> ordered = Prop ord
>   where ord []                 = True
>         ord [a]                = True
>         ord (a1 : as@(a2 : _)) = a1 <= a2 && ord as

> perms as = Gen (permutations as)


> inserts :: a -> [a] -> [[a]]
> inserts a []
>   = [[a]]
> inserts a (b : bs)
>   = (a : b : bs) : [ b : cs | cs <- inserts a bs ]

> permutations :: [a] -> [[a]]
> permutations []
>   =  [[]]
> permutations (a : as)
>   =  [ cs | bs <- permutations as, cs <- inserts a bs ]

> {-

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

> -}