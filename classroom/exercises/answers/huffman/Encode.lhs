>module Encode where

%%%%%%%%%%%%%%%%%%%% Start End Text Delimeters %%%%%%%%%%%%%%%%%%%%%%%%%%

> beginHaskellEncode , endHaskellEncode:: [Char]
> beginHaskellEncode = "%%Begin HaskellEncode%%"
> endHaskellEncode   = "%%End HaskellEncode%%"

%%%%%%%%%%%%%%%%%%%%%%%%% To and From binary %%%%%%%%%%%%%%%%%%%%%%%%%%%%

> binaryToNum :: [Bool] -> Int
> binaryToNum = foldl fn 0
>                where fn n True  = n * 2 + 1
>                      fn n False = n * 2

> numToBinary :: Int -> [Bool]
> numToBinary n = fn n []
>                 where fn 0 acc = acc
>                       fn n acc 
>                         | n `mod` 2==1 = fn (n `div` 2) (True:acc)
>                         | otherwise    = fn (n `div` 2) (False:acc)

> padBinary :: [Bool] -> Int -> [Bool]
> padBinary xs n = take (n - length xs) (repeat False) ++ xs

%%%%%%%%%%%%%%%%%%%%%%%%%%%% Haskell Encode %%%%%%%%%%%%%%%%%%%%%%%%%%%%

> groups :: Int -> [alpha] -> [[alpha]]
> groups n [] = []
> groups n xs = take n xs : groups n (drop n xs)

> binaryToPrintChar xs = toEnum (32 + binaryToNum xs)

> hhencode :: [Bool] -> [Char]
> hhencode xs   = beginHaskellEncode 	++ "\n" ++
>                 show length_last 	++ "\n" ++ 
>                 unlines (groups 45 (map binaryToPrintChar six_bit_chars))++
>                 endHaskellEncode ++ "\n"
>                 where
>                    six_bit_chars        = groups 6 xs
>                    length_last          = length (last six_bit_chars)
                 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Haskell Decode %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

> printCharToPadBinary :: Int -> Char -> [Bool]
> printCharToPadBinary n c = padBinary (numToBinary (fromEnum c - 32)) n

> printableCharsToBinary :: Int -> [Char] -> [Bool]
> printableCharsToBinary n []     = []
> printableCharsToBinary n [x]    = printCharToPadBinary n x
> printableCharsToBinary n (x:xs) = printCharToPadBinary 6 x ++ 
>                                   printableCharsToBinary n xs

> hhdecode :: [Char] -> [Bool]
> hhdecode xs 
>   = printableCharsToBinary length_last (concat ascii_encode)
>     where startEncode = tail (dropWhile (/=beginHaskellEncode) (lines xs))
>           length_last = fromEnum ((head.head) startEncode) - fromEnum '0'
>           ascii_encode= takeWhile (/=endHaskellEncode) (tail startEncode)
>                                                
