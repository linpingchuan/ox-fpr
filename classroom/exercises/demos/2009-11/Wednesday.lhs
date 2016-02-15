ghc --make -O2 Wednesday.lhs
ghci -Wall Wednesday.lhs

> module Wednesday
> where
> import qualified Monday

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

> (-->)   :: Gen a -> Prop b -> Prop (a -> b)

> ordered :: (Ord a) => Prop [a]

> (\/)    :: Gen a -> Gen a -> Gen a
> perms   :: [a] -> Gen [a]

Examples.

([[], [4,7,1,1], [999,998..0]] --> ordered) insertsort
(perms [1..8] --> ordered) insertsort
([[], [4,7,1,1], [999,998..0]] \/ perms [1..8] --> ordered) insertsort

([1..9] --> [[1..9]] --> ordered) Monday.insert

Implementation.

> type Prop a = a -> Bool
> type Gen a = [a]

> g --> p = \ f -> all p (map f g)

> g1 \/ g2 = g1 ++ g2

> ordered []                 = True
> ordered [_a]               = True
> ordered (a1 : as@(a2 : _)) = a1 <= a2 && ordered as

> inserts :: a -> [a] -> [[a]]
> inserts a []       = [[a]]
> inserts a (b : bs) = (a : b : bs) : [ b : cs | cs <- inserts a bs ]

> perms []       = [[]]
> perms (a : as) = [ cs | bs <- perms as, cs <- inserts a bs ]

Just for the fun of it: permutation as a fold.
  
> perms' :: [a] -> Gen [a]
> perms' = foldr (\ a s -> [ q | p <- s, q <- inserts a p ] ) [[]]

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

Insertionsort: sort is a fold, insert is an unfold (exercise).

> insert :: (Ord a) => List a [a] -> [a]
> insert Nil         = []
> insert (Cons x []) = [x]
> insert (Cons x (y : ys))
>   | x <= y         = x : y : ys
>   | otherwise      = y : insert (Cons x ys)

> insertsort :: (Ord a) => [a] -> [a]
> insertsort = fold insert

Selectionsort: sort is an unfold, delete is a fold (exercise).

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

Background
----------

What's the relation between the two implementations of fold(r)?

Well, the input to fold(r) is represented in two different, but
equivalent ways. Here is a mathematical explanation.

  (a -> b -> b) -> b -> ([a] -> b)
≅ { currying }
  (a -> b -> b) × b -> ([a] -> b)
≅ { see below }
  (List a b -> b) -> ([a] -> b)

We show that is (a -> b -> b) × b isomorphic to List a b -> b. Here,
X × Y is the Cartesian product of X and Y, written in Haskell as (X, Y).

  (a -> b -> b) × b
≅ { currying }
  (a × b -> b) × b
≅ { () -> b ≅ b }
  (a × b -> b) × (() -> b)

So, we have got two functions with the same result type. Let's
generalise a bit (setting A1 = a × b, A2 = ()):

  (A1 -> B) × (A2 -> B)
≅ { writing function types as exponentials }
  (B ^ A1) × (B ^ A2)
≅ { laws of exponentials }
  B ^ (A1 + A2)
≅ { writing function types as exponentials }
  (A1 + A2) -> B

In Haskell, A1 + A2 is the disjoint sum Either A1 A2.

< data Either a b = Left a | Right b

Continuing the calculation above:

  (A1 + A2) -> B
≅ { A1 = a × b, A2 = () }
  Either (a × b) () -> B

The term Left (x, y) corresponds to Cons x y, and Right () to Nil.

Let's program the functions that convert a pair of functions to a
single function on sums and vice versa.

> fp :: (a1 -> b, a2 -> b) -> (Either a1 a2 -> b)
> fp (f, _g) (Left  a) = f a
> fp (_f, g) (Right b) = g b

> oop :: (Either a1 a2 -> b) -> (a1 -> b, a2 -> b)
> oop h = (h . Left, h . Right)

In a sense, fp and oop realise a paradigm shift. The two OOP functions
can be seen as methods: a method called m in class A1 operating on the
internal state a1 and a method of the same name in class A2 operating
on the internal state a2. The FP function is a single function that
operates on a datatype, which is disjoint sum of two types.