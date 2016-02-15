ghci GADT.lhs

> {-# OPTIONS -fglasgow-exts #-}

> data Expr :: * -> * where
>   Lit :: a -> Expr a
>   Add :: Expr Int -> Expr Int -> Expr Int
>   IsZ :: Expr Int -> Expr Bool

> eval :: Expr a -> a
> eval (Lit x)   = x
> eval (Add x y) = eval x + eval y
> eval (IsZ x)   = (eval x == 0)
