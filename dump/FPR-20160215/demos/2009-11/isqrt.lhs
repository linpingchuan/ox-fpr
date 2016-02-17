> import Wednesday

> isqrt :: Integer -> Integer
> isqrt n = loop 0 3 1
>   where loop i k s | s <= n    = loop (i + 1) (k + 2) (s + k)
>                    | otherwise = i


Produce an (infinite) list of `states'.

> squares :: [Integer]
> squares = unfold (\ (k, s) -> Cons s (k + 2, s + k)) (3, 1)

squares is an unfold.

Consume an (infinite) list of `states'.

> while' :: (a -> Bool) -> ([a] -> a)
> while' _p [] = error "while"
> while' p (a : as)
>  | p a       = while' p as
>  | otherwise = a

while is a fold.

> while :: (a -> Bool) -> ([a] -> a)
> while p = foldr (\ a s -> if p a then s else a) (error "while")

isqrt is a fold after an unfold.

> isqrt' :: Integer -> (Integer, Integer)

> isqrt' n = while (\ (_i, s) -> s <= n) (zip [0 ..] squares)
