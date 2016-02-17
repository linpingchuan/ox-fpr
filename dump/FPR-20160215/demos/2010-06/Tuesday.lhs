ghci -Wall Tuesday.lhs

> module Tuesday
> where
> import List (tails)

Hacking session
---------------

Determine whether a string occurs in a text.

sublist "fun" "I love functional programming!" == True
sublist "fun" "functional" == True

sublist s t <=> exists x y . x ++ s ++ y = t

Failed attempt:

 sublist  [] s      = True
 sublist (a : as) s = ... sublist as s ...

Successful attempt:

> sublist :: (Eq a) => [a] -> [a] -> Bool
> sublist s []       = null s
> sublist s (a : as) = prefix s (a : as) || sublist s as

prefix  "fun" "functional" == True

> prefix :: (Eq a) => [a] -> [a] -> Bool
> prefix []         _bs      = True
> prefix (_a : _as) []       = False
> prefix (a : as)   (b : bs) = a == b && prefix as bs

Alternative definition inspired by `member': a substring is a prefix
of a suffix.

> member :: (Eq a) => a -> [a] -> Bool
> member a bs  =  or [ a == b | b <- bs ]

> sublist' :: (Eq a) => [a] -> [a] -> Bool
> sublist' as bs  =  or [ prefix as x | x <- tails bs ]

The lists of all sublists of a given list.

> powerlist :: [a] -> [[a]]
> powerlist []        =  [[]]
> powerlist (a : as)  =  [ a : x | x <- ps ] ++ ps
>   where ps = powerlist as

Permutations of a given list.

> perms :: [a] -> [[a]]
> perms []        =  [ [] ]
> perms (a : as)  =  [ x | p <- perms as, x <- insert a p ]
> -- perms (a : as)  =  concat [ insert a p | p <- perms as ]

> insert :: t -> [t] -> [[t]]
> insert a as  =  ins as
>   where
>   ins []        =  [ [a] ]
>   ins (b : bs)  =  (a : b : bs) : [ b : x | x <- ins bs ]

Smallest element of a list, as a total function.

> data Exception a = Raise | Return a
>  deriving (Show)

> smallest :: [Integer] -> Exception Integer
> smallest = foldr min' Raise

> min' :: Integer -> Exception Integer -> Exception Integer
> min' a Raise       =  Return a
> min' a (Return b)  =  Return (a `min` b)
