3 Recursive definitions on lists
================================

3.1 product of a list of integers

> prod :: [Int] -> Int
> prod [] = 1
> prod (x:xs) = x * prod xs

3.2 allTrue

> allTrue :: [Bool] -> Bool
> allTrue [] = True
> allTrue (x:xs) = x && allTrue xs

3.3 allFalse

> allFalse :: [Bool] -> Bool
> allFalse [] = True
> allFalse (x:xs) = not x && allFalse xs

3.4 decAll

> decAll :: [Int] -> [Int]
> decAll [] = []
> decAll [x] = [x-1]
> decAll (x:xs) = decAll [x] ++ decAll xs

3.5  convertIntBool

> convertIntBool :: [Int] -> [Bool]
> convertIntBool [] = []
> convertIntBool [0] = [False]
> convertIntBool [_] = [True]
> convertIntBool (x:xs) = convertIntBool [x] ++ convertIntBool xs

3.6 pairUp

> pairUp :: [Int] -> [Char] -> [(Int, Char)]
> pairUp [] _ = []
> pairUp _ [] = []
> pairUp [x] [y] = [(x, y)]
> pairUp (x:xs) (y:ys) = pairUp [x][y] ++ pairUp xs ys

3.7 takePrefix

> takePrefix :: Int -> [a] -> [a]
> takePrefix _ [] = []
> takePrefix 0 _ = []
> takePrefix 1 (x:xs) = [x]
> takePrefix n (x:xs) = takePrefix 1 [x] ++ takePrefix (n-1) xs

3.8 dropPrefix

> dropPrefix :: Int -> [a] -> [a]
> dropPrefix _ [] = []
> dropPrefix 0 x = x
> dropPrefix 1 (x: xs) = xs
> dropPrefix n (x:xs) = dropPrefix (n-1) xs

3.9 member

> member :: Eq a => [a] -> a -> Bool
> member [] _ = False
> member [x] n = x == n
> member (x:xs) n = member [x] n || member xs n

3.10 equals

> equals :: Eq a => [a] -> [a] -> Bool
> equals [] [] = True
> equals [] _ = False
> equals _ [] = False
> equals [x] [y] = x == y
> equals (x:xs) (y:ys) = equals [x] [y] && equals xs ys

3.11 select

> select :: [a] -> Int -> a
> select [] _ = error "Empty list!"
> select [a] n
>   | n == 0 = a
>   | otherwise = error "No element at position!"
> select (x:xs) n
>   | n == 0 = x
>   | otherwise = select xs (n-1)

3.12 largest

> largest :: [Int] -> Int
> largest [] = error "Empty list!"
> largest [x] = x
> largest (x:xs)
>   | x > largestxs = x
>   | otherwise = largestxs
>   where largestxs = largest xs

3.13 smallest

> smallest :: [Int] -> Int
> smallest [] = error "Empty list!"
> smallest [x] = x
> smallest (x : xs)
>   | x < smallestxs = x
>   | otherwise = smallestxs
>   where smallestxs = smallest xs
