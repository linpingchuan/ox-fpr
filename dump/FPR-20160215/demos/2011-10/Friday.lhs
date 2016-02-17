ghci -Wall Wednesday.lhs

> {-# LANGUAGE UnicodeSyntax #-}
>
> module Friday
> where
> import Char

Akinator: Think about a real or fictional character. I will try to
guess who it is. 
                                     Jcccc,        ,d$$$b,
                                     J$$$$$$c     ,d$$$$$$,
                                     $$$$$$$$$c,,c$$$$P$$$$,
                                    J$$$$$$$$$$$$$$$$$$3$$$$,
                                    $$$$$$$$$$$$$$$$$$$F$$$$$$c=
                                   J$$$$$$$$$$$$$?$$$$$F  """
                                zcd$$$$$$$$$$$$$Fb3$$$"
                             ,c=cc$$$$$$$$$$??$$$c
                          ,z$"   `$$$$$$$$$??$.?$$b
                         d$$",d$$c$$$$$$P"  ?$Fd$$$$
                       ,$$$,dP" "$$$$$$$$$$c `$$$$$$r
                      z$$$$P"=$c $$$$$$P""?$, $$$$$$$
             .,,ccc,,4$$$$$'  `$ $$$$"-cc  $$,$$$$$$$
         ,c$$$$$$$$$$$$$$$$ ,,d$,$$P   $$FJ$$$$$$$E3F
        ,$$$$$$$$$$$$$$$$$$L"$$'$$$ ,zd$$ $$$$$$$$???-
        $$$$$$$$$$$$$$$$P""??cc$$$$ $$$F,$$$$$$$$$$$$$$$$ccc,.
        $$$$$$$$$$$$$$P d$$$$bc3$$$bc,cd$$$$$$$$$$$$$$$$$$$$$$$bc.
         $$$$$$$$$$$$$F:$$$$$$$$$P?$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$b.
         `$$$$$$$$$$$$b.?$$$$$$$$$c"$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$c
           ?$$$$$$$$$$$ c,?$$$$$$$",$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$h
            `$$$$$$$$$"d$$$cc,,,,c $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$F
             `?$$$$$$$z$$$$$$$$$$$ $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$F
               ?$$$$$$$$$$$$$$$$$$ $$$$$$$$$$$$$$$$$$$$$?$$$$$$$$$P"
                $$$$$$$$$?$$$$$$$'d$$$$$$$$$$$$$$$$F',ccccccc,"''
                `$$$$$$$F "$$P$$P,$$$$$$$$$$$$$$P",$$$$$$$$$$$$$bc.
                 $$$$$P$$$$$$ $$b$$$$$$$$$$$$P?",$$$$$$$$$$$$$$$$$$$c
     .,,,,.      `$$$$$,"???",$$$$$$$$$$$$CCh$'J$$$$$$$$$$$$$$$$$$$$$$b.
 ,c$$$$$$$$$$c, $ $$$$$$$$$$$$$$$$$$$$$$$$$$$'d$$$$$$$$$$$$$$$$$$$$$$$$$$c
 $$$$$$$$$$$$$$ $ ?$$$$$$$$$$$$$$$$$$$$$$$$$'c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
 $$$$$$$$$$$$$$ $L`$$$$$$$$$$$$$$$$$$$$$$$$$,$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
 $$$$$$$$$$$$$$ $$,`$$$$$$$$Fl$$$$$$$$$$$$$L$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
 $$$$$$$$$$$$$$ $$$c,?$$$P"l$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
 $$$$$$$$$$$$$$,`$$$$$cccd$$$$$?$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
 $$$$$$$$$$$$$$$ ?$$$$$$$$$$$",$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
 $$$$$$)$$$$$$$$$,"$$$$$$P",c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
 $$$$$FJ$$$$$$$$$$b,,..,,d$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
> iGuessTheCelebrity  ∷ IO ()
> iGuessTheNumber     ∷ Integer → Integer → IO ()

Game logic
----------

> data Tree a b = Tip a | Branch b (Tree a b) (Tree a b)
>   deriving (Show)

> bimap ∷  (a1 → a2) → (b1 → b2) → (Tree a1 b1 → Tree a2 b2)
> bimap f _g (Tip a)         =  Tip (f a)
> bimap f g  (Branch b l r)  =  Branch (g b) (bimap f g l) (bimap f g r)

> guess ∷ Tree String String → IO ()
> guess (Tip s)
>   =  putStrLn s
> guess (Branch q l r)
>   =  do  b ← yesOrNo q
>          if b then
>            guess l
>          else
>            guess r

> yesOrNo ∷ String → IO Bool
> yesOrNo question
>   =  do putStrLn question
>         answer ← getLine
>         return (map toLower answer `elem` ["y", "yes", "j", "ja"])

I Guess The Celebrity
---------------------

> iGuessTheCelebrity
>   = do putStrLn ("Think of a celebrity.")
>        guess (bimap (\ s → s ++ "!") (\ q → q ++ "?") celebrity)

> celebrity ∷ Tree String String
> celebrity
>   =  Branch "Female"
>        (Branch "Actress"
>           (Tip "Angelina Jolie")
>           (Tip "Adele"))
>        (Branch "Actor"
>           (Tip "Brad Pitt")
>           (Tip "Steve Hackett"))

I Guess The Number
------------------

> iGuessTheNumber l r
>   =  do putStrLn ("Think of number between " ++ show l ++ " and " ++ show r ++ ".")
>         guess (bimap (\ n → show n ++ "!") (\ m → "≤ " ++ show m ++ "?") (nest l r))

> nest ∷ Integer → Integer → Tree Integer Integer
> nest l r
>   | l == r     =  Tip l
>   | otherwise  =  Branch m (nest l m) (nest  (m + 1) r)
>   where m  =  (l + r) `div` 2
