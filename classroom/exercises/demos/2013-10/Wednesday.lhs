> {-# LANGUAGE UnicodeSyntax #-}
> module Wednesday
> where
> import Unicode ((·))
> import Prelude hiding ((^))

An Interpreter
--------------

A simple datatype for arithmetic expressions.

> infixl 7 :*:
> infixl 6 :+:
>
> data Expr
>   =  Lit Integer    -- a literal
>   |  Expr :+: Expr  -- addition
>   |  Expr :*: Expr  -- multiplication
>   deriving (Show)

Some test data.

> expr1, expr2 ∷ Expr
> expr1  =  (Lit 4 :*: Lit 7) :+: (Lit 11)
> expr2  =  (Lit 4 :+: Lit 7) :*: (Lit 11)

< Lit 4 :+: Lit 7 :*: Lit 11
< 4 + 7 * 11

Number of additions.

> additions ∷  Expr → Int
> additions (Lit _)      =  0
> additions (e1 :+: e2)  =  1 + additions e1 + additions e2
> additions (e1 :*: e2)  =  0 + additions e1 + additions e2

































Evaluating expressions.

> evaluate  ∷  Expr → Integer
> evaluate (Lit i)      =  i
> evaluate (e1 :+: e2)  =  evaluate e1 + evaluate e2
> evaluate (e1 :*: e2)  =  evaluate e1 * evaluate e2



Showing expressions (vanilla).

> showExpr  ∷  Expr → String
> showExpr (Lit i)
>   =  show i
> showExpr (e1 :+: e2)
>   =  "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
> showExpr (e1 :*: e2)
>   =  "(" ++ showExpr e1 ++ " * " ++ showExpr e2 ++ ")"

< showExpr expr1 = "((4 * 7) + 11)"
< showExpr expr2 = "((4 + 7) * 11)"

Showing expressions (respecting precedence).

> showPrec  ∷  Int → Expr → String
> showPrec _ (Lit i)
>   =  show i
> showPrec p (e1 :+: e2)
>   =  parenthesis (p > 6) (showPrec 6 e1 ++ " + " ++ showPrec 6 e2)
> showPrec p (e1 :*: e2)
>   =  parenthesis (p > 7) (showPrec 7 e1 ++ " * " ++ showPrec 7 e2)

> parenthesis  ∷  Bool → String → String
> parenthesis True  s  =  "(" ++ s ++ ")"
> parenthesis False s  =  s

> showExpr'  ∷  Expr → String
> showExpr'  =  showPrec 0

< showExpr' expr1 = "4 * 7 + 11"
< showExpr' expr2 = "(4 + 7) * 11"


A Compiler
----------

< compile (Lit 4 :*: (Lit 7 :+: Lit 11))
<   =  Push 4 :^: Push 7 :^: Push 11 :^: Add :^: Mul

When executed, the stack grows and shrinks.

< []
< 4 : []
< 7 : 4 : []
< 11 : 7 : 4 : []
< 18 : 4 : []
< 72 : []

Instructions of a stack machine.

> infixr 2 :^:
>
> data Code
>   =  Push Integer   -- push integer onto stack
>   |  Add            -- add topmost two elements and push result
>   |  Mul            -- multiply
>   |  Code :^: Code  -- sequencing
>   deriving (Show)

> code1  ∷  Code
> code1  =  Push 47 :^: Push 11 :^: Add

Showing code.

> showCode  ∷  Code → String
> showCode (Push i)     =  "push " ++ show i
> showCode (Add)        =  "add"
> showCode (Mul)        =  "mul"
> showCode (c1 :^: c2)  =  showCode c1 ++ " ; " ++ showCode c2

< showCode code1 = "push 47 ; push 11 ; add"

Compiling.

> compile  ∷  Expr → Code
> compile (Lit i)      =  Push i
> compile (e1 :+: e2)  =  compile e1 :^: compile e2 :^: Add
> compile (e1 :*: e2)  =  compile e1 :^: compile e2 :^: Mul

Executing.

> type Stack  =  [Integer]

> execute  ∷  Code → (Stack → Stack)
> execute (Push i)     =  push i
> execute (Add)        =  add
> execute (Mul)        =  mul
> execute (c1 :^: c2)  =  execute c2 · execute c1

Stack transformers.

> push  ∷  Integer → (Stack → Stack)
> push i xs  =  i : xs
>
> add  ∷  Stack → Stack
> add []              =  error msg
> add [_]             =  error msg
> add (x1 : x2 : xs)  =  x2 + x1 : xs
>
> mul  ∷  Stack → Stack
> mul []              =  error msg
> mul [_]             =  error msg
> mul (x1 : x2 : xs)  =  x2 * x1 : xs
>
> msg  ∷  String
> msg  =  "VM: empty stack"

< execute (compile expr1) []
< execute (compile expr2) []

A puzzle
--------

> twice :: (a -> a) -> (a -> a)
> twice f = f · f

< twice (+ 1) 0
< twice twice (+ 1) 0
< twice twice twice (+ 1) 0

∷ ⇒ ∀ → ← ⋯ ∨ ∧ ≤ ≥ ·
