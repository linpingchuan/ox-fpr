The module declaration below is required to prevent Intellij from complaining with the following error
The IO action ‘main’ is not defined in module ‘Main’

> module Exercise1 where

1. Numeric definitions

a. square

> square :: Integer -> Integer
> square x = x * x

b. quad

> quad :: Integer -> Integer
> quad x = square x * square x

c. larger

> larger :: Integer -> Integer -> Integer
> larger x y = if x <= y then y else x

d. area of circle

> areaOfCircle :: Double -> Double
> areaOfCircle r = pi * r * r