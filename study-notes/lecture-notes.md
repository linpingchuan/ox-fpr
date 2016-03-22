# Introduction

* FP is programming with values

* a = 2 * 3 / 2 is a statement, 2 * 3 / 2 is an expression

* FP focuses on expressions and not on statements

* __Referential transparency__ means that an expression always evaluates to the same value in any context. This is a 
property of (pure) functional languages. 
Ref: [Haskell Wiki - Referential_transparency](https://wiki.haskell.org/Referential_transparency),
[Stackoverflow](http://stackoverflow.com/questions/210835/what-is-referential-transparency)

# Programming with expressions and values

* Haskell is a lazy functional programming language

* Eager evaluation order is also called _applicative order_

* Lazy evaluation order is also called _normal order_

* A function f is said to be __strict__ if, when applied to a nonterminating expression, it also fails to terminate

* _Type declaration_ in script specifies type of function
Example: `square :: Integer -> Integer`

* _Lambda_ is a notation for anonymous functions

* Haskell support 2 different programming styles
    * declaration style
        
        quad :: Integer -> Integer
        quad x = square x * square x
        
    * expression style

        quad :: Integer -> Integer
        quad = \x -> square x * square x
        
* [Currying](https://wiki.haskell.org/Currying) is the process of reducing functions that take multiple arguments into 
those that take a single argument and return another function if any arguments are still needed

        add' :: Integer -> Integer -> Integer
        add' x y = x + y
    
    is the curried form of
    
        add :: (Integer, Integer) -> Integer
        add (x, y) = x + y
        
* Function application associates to the left

        f a b mean (f a) b
        
* Function type operator associates to the right
        
        Integer -> Integer -> Integer means
        Integer -> (Integer -> Integer)
        
* Function definitions
    
    * Conditional definitions
    
    Expression style: if ... then ... else ...
    
        smaller :: (Integer, Integer) -> Integer
        smaller (x, y) = if x <= y then x else y

    Declaration style: using guarded equations
     
        smaller :: (Integer, Integer) -> Integer
        smaller (x, y)
            | x <= y = x
            | x > y = y
            
    Last guard can be `otherwise`, synonym for `True`            
                                        
    * Pattern matching
    
        day :: Integer -> String
        day 1 = "Saturday"
        day 2 = "Sunday"
        day _ = "Weekday"
            
    * Local definitions
    
    
    Declaration style: using `where` clause
    
        demo :: Integer -> Integer -> Integer
        demo x y = (a + 1) * (b + 2)
            where a = x - y
                  b = x + y
                    
    Expression style: using let ... in ...
                    
        demo :: Integer -> Integer -> Integer
        demo x y = let a = x - y
                       b = x + y
                   in (a + 1) * (b + 2)
                       
# Types and polymorphism
    
* Haskell is strongly and statically typed

* Declaring new data types

        data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun 

* __Parametric polymorphism__ - Parametric polymorphism refers to when the type of a value contains one or more 
(unconstrained) type variables, so that the value may adopt any type that results from substituting those 
variables with concrete types.

Example:

        first :: (a, b) -> a
        
here, `a`, `b` are _type variables_     

* __Type synonyms__ - alternate names for types

        type Card = (Rank, Suit)
        
* __Type classes__
    
    * Provide ad hoc polymorphism / overloading
    
    * Allow us to declare which types are instances of which class and provide definitions of the overloaded operations
    associated with these classes
    
    Example:
    
        class Eq a where
            (==)            :: a -> a-> Bool
                
    * Type classes are like interfaces in Java
                    
    * Instance declaration is done as follows
                        
        instance Eq Integer where
            x == y = x `integerEq` y
            
* __Constructor__

        data Bool = True | False
        
        data Tree a = Tip | Node a (Tree a) (Tree a)

    * Type constructor - `Bool` and `Tree` are type constructors        
    
    * Data constructor - `True`, `False`, `Tip`, `Node`
     
    * Data constructors have no type. Hence it is illegal to write `Node a (Node a) (Node a)` 
    
        
    
    
    
                       
    

