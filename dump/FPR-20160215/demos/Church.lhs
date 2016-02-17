ghci Church.hs

> {-# OPTIONS -XRankNTypes #-}

> import Prelude hiding (succ, (^))

> type Church = forall a . (a -> a) -> (a -> a)

> zero :: Church
> zero f = id

> succ :: Church -> Church
> succ n f = f . n f

> puzzle :: Church -> Church -> Church
> puzzle m n = n m

> one = succ zero
> two = succ one
> three = succ two

> isZero :: Church -> Bool
> isZero n = n (\ s -> False) True

> plus :: Church -> Church -> Church
> plus  m n f = m f . n f

> times :: Church -> Church -> Church
> times m n f = m (n f)

> power :: Church -> Church -> Church
> power m n = n m

two two two two (+ 1) 0

---

run (12 + 2 * 14) (+ 1) 0
run (12 + 34) double (return 1)

> double act =
>   do i <- act
>      print i
>      return (2 * i)

> newtype Natural = Nat { run :: forall a . (a -> a) -> (a -> a) }

> instance Show Natural
> instance Eq Natural
> instance Num Natural where
>   m + n = Nat (\ f -> run m f . run n f)
>   m * n = Nat (\ f -> run m (run n f))
>   fromInteger 0 = Nat (\ f -> id)
>   fromInteger n = Nat (\ f -> f . run (fromInteger (n - 1)) f)

> infixr 8 ^
> (^) :: Natural -> Natural -> Natural
> m ^ n = Nat ((run n) (run m))

---

> type List a = forall r . (a -> r -> r) -> r -> r

> nil :: List a
> nil f e = e

> cons :: a -> List a -> List a
> cons a x f e = f a (x f e)
