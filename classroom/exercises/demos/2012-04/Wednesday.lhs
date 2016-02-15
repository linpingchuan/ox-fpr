ghci -Wall Wednesday.lhs

> {-# LANGUAGE UnicodeSyntax #-}
>
> module Wednesday
> where
> import Prelude hiding ((^))


A Compiler
----------

A simple expression type.

> data Expr  =  Int Integer    -- an integer constant
>            |  Add Expr Expr  -- addition
>            |  Mul Expr Expr  -- multiplication
>               deriving (Show)

> expr1, expr2  ∷  Expr
> expr1  =  Add (Mul (Int 4) (Int 7)) (Int 11)
> expr2  =  Mul (Add (Int 4) (Int 7)) (Int 11)

> evaluate  ∷  Expr → Integer
> evaluate (Int i)      =  i
> evaluate (Add e1 e2)  =  evaluate e1 + evaluate e2
> evaluate (Mul e1 e2)  =  evaluate e1 * evaluate e2

> showExpr  ∷  Expr → String
> showExpr (Int i)      =  show i
> showExpr (Add e1 e2)  =  "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
> showExpr (Mul e1 e2)  =  "(" ++ showExpr e1 ++ " * " ++ showExpr e2 ++ ")"

> showPrec  ∷  Int → Expr → String
> showPrec _ (Int i)      =  show i
> showPrec p (Add e1 e2)
>   =  parenthesis (p > 6) (showPrec 6 e1 ++ " + " ++ showPrec 6 e2)
> showPrec p (Mul e1 e2)
>   =  parenthesis (p > 7) (showPrec 7 e1 ++ " * " ++ showPrec 7 e2)

> parenthesis  ∷  Bool → String → String
> parenthesis True  s  =  "(" ++ s ++ ")"
> parenthesis False s  =  s


Instructions of a stack machine.

> infixr 2 :^:, ^

> data Code  =  Push Integer   -- push given integer onto stack
>            |  Plus           -- add topmost elements pushing result onto stack
>            |  Times          -- multiply
>            |  Code :^: Code  -- sequencing
>               deriving (Show)

> (^)  ∷  Code → Code → Code
> c1 ^ c2  =  c1 :^: c2

> code1  ∷  Code
> code1  =  Push 47 ^ Push 11 ^ Plus

> showCode  ∷  Code → String
> showCode (Push i)     =  "push " ++ show i
> showCode (Plus)       =  "plus"
> showCode (Times)      =  "times"
> showCode (c1 :^: c2)  =  showCode c1 ++ " ; " ++ showCode c2

Compiling.

> compile  ∷  Expr → Code
> compile (Int i)      =  Push i
> compile (Add e1 e2)  =  compile e1 ^ compile e2 ^ Plus
> compile (Mul e1 e2)  =  compile e1 ^ compile e2 ^ Times

Executing.

> type Stack  =  [Integer]

> execute  ∷  Code → (Stack → Stack)
> execute (Push i)     =  push i
> execute (Plus)       =  plus
> execute (Times)      =  times
> execute (c1 :^: c2)  =  execute c2 . execute c1

> push  ∷  Integer → (Stack → Stack)
> push i xs  =  i : xs

> plus  ∷  Stack → Stack
> plus []              =  error msg
> plus [_]             =  error "VM: empty stack"
> plus (x1 : x2 : xs)  =  x2 + x1 : xs

> times  ∷  Stack → Stack
> times []              =  error msg
> times [_]             =  error "VM: empty stack"
> times (x1 : x2 : xs)  =  x2 * x1 : xs

> msg  ∷  String
> msg  =  "VM: empty stack"


Proof of correctness:

    push (evaluate e) = execute (compile e)

Case e = Int i:				
        push (evaluate (Int i))	     	     
      = { definition of evaluate }
        push i
--------
        execute (compile (Int i))
      = { definition of compile }
        execute (Push i)
      = { definition of execute }
        push i

Case e = Add e1 e2:	
        push (evaluate (Add e1 e2)
      = { definition of evaluat e}
        push (evaluate e1 + evaluate e2) 
--------
        execute (compile (Add e1 e2))
      = { definition of compile }
        execute (compile e1 :^: compile e2 :^: Plus)
      = { definition of execute }
        plus . execute (compile e2) . execute (compile e1)
      = { induction hypothesis }
        plus . push (evaluate e2) . push (evaluate e1) 
      = { property of plus: plus . push n . push m = push (m + n) }
        push (evaluate e1 + evaluate e2)

Case e = Mult e1 e2:
        push (evaluate (Mult e1 e2))
      = { definition of evaluate }
        push (evaluate e1 * evaluate e2)
--------
        execute (compile (Mult e1 e2))
      = { definition of compile }
        execute (compile e1 :^: compile e2 :^: Times)
      = { definition of execute }
        times . execute (compile e2) . execute (compile e1)
      = { induction hypothesis }
        times . push (evaluate e2) . push (evaluate e1) 
      = { property of times: times . push n . push m = push (m * n) }
        push (evaluate e2 * evaluate e1)


Folds and unfolds
-----------------

> data List a b = Nil | Cons a b

> fold  ∷  (List a b → b) → ([a] → b)
> fold inn []       =  inn Nil
> fold inn (a : x)  =  inn (Cons a (fold inn x))

> sum  ∷  [Integer] → Integer
> sum  =  fold (\ x →  case x of 
>                      Nil      → 0
>                      Cons a b → a + b)

> unfold  ∷  (b → List a b) → (b → [a])
> unfold out x
>   =  case out x of
>      Nil        →  []
>      Cons a x'  →  a : unfold out x'

> enumFromTo  ∷  Integer → Integer → [Integer]
> enumFromTo m n 
>   = unfold (\ i → if i > n then Nil 
>                            else Cons i (i + 1)) m
