> import Char

> type Parser a  =  String -> [(a, String)]

> success  ::  a -> Parser a
> satisfy  ::  (Char -> Bool) -> Parser Char
> symbol   ::  Char -> Parser Char

> infixl 3  <>
> infixr 2  |||
> (<>)     ::  Parser (a -> b) -> Parser a -> Parser b
> (|||)    ::  Parser a -> Parser a -> Parser a

> many     ::  Parser a -> Parser [a]
> many1    ::  Parser a -> Parser [a]

---

> success a  s  =  [(a, s)]

> satisfy _ []   =  []
> satisfy p (a : s)
>   | p a        =  [(a, s)]
>   | otherwise  =  []

> symbol a  =  satisfy (== a)

> (p ||| q) s  =  p s ++ q s

> (p <> q) s  =  [ (f x, s'') | (f, s') <- p s, (x, s'') <- q s']

> many p  =   success (:) <> p <> many p
>         ||| success []

> many1 p  =  success (:) <> p <> many p

---

> digit :: Parser Integer
> digit  =  success (\ c -> toInteger (ord c - ord '0')) <> satisfy isDigit

> digits :: Parser Integer
> digits  =  success (foldl ((+) . (10 *)) 0) <> many1 digit

> negation :: (Num a) => Parser (a -> a)
> negation  =   success (const negate) <> symbol '-'
>           ||| success id

> integer :: Parser Integer
> integer  =  negation <> digits

> decimal :: Parser Float
> decimal  =   success f <> symbol '.' <> many1 digit
>          ||| success 0
>          where f _ = foldr (\ d x -> (fromInteger d + x) / 10) 0

> real :: Parser Float
> real  =  success add <> integer <> decimal
>   where add n d  =  if n < 0 then n' - d else n' + d 
>                     where n'  =  fromInteger n

> expon :: Parser (Float -> Float)
> expon  =   success f <> symbol 'e' <> integer
>        ||| success id
>        where f _ n x = x * (10 ** fromInteger n)

> float :: Parser Float
> float  =  success (\ x f -> f x) <> real <> expon
