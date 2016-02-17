ghci -Wall Wednesday.lhs

> {-# LANGUAGE UnicodeSyntax, NPlusKPatterns #-}
>
> module Wednesday
> where
> import Unicode

Mergesort
---------

> mergeSort  ∷  (Ord a) => [a] → Run a
> mergeSort  =  sortRuns . runs

A run is a (non-empty) ordered list (in ascending order or
non-decreasing order).

> type Run a  =  [a]

> runs ∷ (Ord a) => [a] → [Run a]
> runs []        =  []
> runs (a : as)  =  tack a (runs as)

> tack ∷ (Ord a) => a → [Run a] → [Run a]
> tack a []         =  [[a]]
> tack a ([] : rs)  =  tack a rs
> tack a ((b : bs) : rs)
>   | a <= b     =  (a : b : bs) : rs
>   | otherwise  =  [a] : (b : bs) : rs

> sortRuns ∷ (Ord a) => [Run a] → Run a
> sortRuns []   =  []
> sortRuns [r]  =  r
> sortRuns rs   =  sortRuns (mergeAdjacent rs)

> mergeAdjacent ∷ (Ord a) => [Run a] → [Run a]
> mergeAdjacent []              =  []
> mergeAdjacent [r]             =  [r]
> mergeAdjacent (r1 : r2 : rs)  =  merge r1 r2 : mergeAdjacent rs

> merge ∷ (Ord a) => Run a → Run a → Run a
> merge [] bs  =  bs
> merge as []  =  as
> merge (a : as) (b : bs)
>   | a ≤ b      =  a : merge as (b : bs)
>   | otherwise  =  b : merge (a : as) bs 

Expressions
-----------

> data Expr = Lit Integer | Add Expr Expr | Mul Expr Expr
>   deriving (Show)

> eval ∷ Expr → Integer
> eval (Lit n)    =  n
> eval (Add x y)  =  eval x + eval y
> eval (Mul x y)  =  eval x * eval y

> showExpr ∷ Expr → String
> showExpr (Lit n)      =  show n
> showExpr (Add e1 e2)  =  "(" ++ showExpr e1 ++ " + " ++  showExpr e2 ++ ")"
> showExpr (Mul e1 e2)  =  "(" ++ showExpr e1 ++ " * " ++  showExpr e2 ++ ")"

> expr1, expr2 ∷ Expr
> expr1 = Add (Mul (Lit 47) (Lit 11)) (Lit 7)
> expr2 = Mul (Add (Lit 47) (Lit 11)) (Lit 7)

Game trees
----------

> data Gtree a = GNode a [Gtree a]
>   deriving (Show)

> size ∷ Gtree a → Integer
> size (GNode _ ts)  =  1 + sum (map size ts)

> gametree ∷ (pos → [pos]) → (pos → Gtree pos)
> gametree m p  =  GNode p (map (gametree m) (m p))

> type Pos     =  (Player, Board)
> type Player  =  Char
> type Board   =  String

> next ∷ Pos → [Pos]
> next (p, b)  =  [ (turn p, b') | b' ← moves p b ]

> turn ∷ Player → Player
> turn 'x'  =  'o'
> turn 'o'  =  'x'

> moves ∷ Player → Board → [Board]
> moves _p []          =  []
> moves p  (' ' : as)  =  (p : as) : [ ' ' : xs | xs ← moves p as]
> moves p  (c : as)    =  [ c : xs | xs ← moves p as]
