ghci Bool.lhs

> {-# OPTIONS -XRankNTypes #-}

> import Prelude hiding (not, (&&), (||))

> type Boolean = forall a . a -> a -> a


> false, true :: Boolean
> false = \ x y -> y
> true  = \ x y -> x

> ifthenelse :: Boolean -> a -> a -> a
> ifthenelse b t e = b t e

> not :: Boolean -> Boolean
> not b = ifthenelse b false true

> (&&), (||) :: Boolean -> Boolean -> Boolean

> x && y =  ifthenelse x y false
> x || y =  ifthenelse x true y

> convert False = false
> convert True  = true

> leq :: (Ord a) => a -> a -> Boolean
> leq a b = convert (a <= b)

> factorial n = ifthenelse (leq n 0) 1 (n * factorial (n - 1))