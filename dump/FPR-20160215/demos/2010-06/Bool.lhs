ghci -Wall Bool.lhs

> {-# OPTIONS -XRankNTypes #-}

> import Prelude hiding (not, (||))

> type Boolean = forall a . a -> a -> a

> false :: Boolean
> false = \ _x y -> y

> true :: Boolean
> true = \ x _y -> x

> not :: Boolean -> Boolean
> not x = ifthenelse x false true

> (||) :: Boolean -> Boolean -> Boolean
> x || y = ifthenelse x true y

> ifthenelse :: Boolean -> a -> a -> a
> ifthenelse = id
