ghci -Wall Puzzle.lhs

> {-# LANGUAGE UnicodeSyntax #-}

> import Unicode

What does the function do?

> unknown ∷ Integer → Integer
> unknown n = loop 0 3 1
>   where loop i k s | s ≤ n     = loop (i + 1) (k + 2) (s + k)
>                    | otherwise = i

Prize: a pint of `London Pride' or a beverage of your choice.
