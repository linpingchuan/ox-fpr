> module Bitmaps where

> type Grid a = [[a]]

> catPic :: Grid Char
> catPic =
>   [ "  *     *  ",
>     " * *   * * ",
>     " *  ***  * ",
>     "*         *",
>     "*  *   *  *",
>     "*    *    *",
>     " *       * ",
>     "  ******   " ]

4.1 Bitmaps as lists of lists

4.1.1 charRender

> charRender :: Grid Char -> IO()
> charRender a = putStr (unlines a)

4.1,2.a solidSquare

> generateRow :: Int -> String
> generateRow 0 = []
> generateRow n = ['*'] ++ generateRow (n-1)

> solidSquare :: Int -> Grid Char
> solidSquare n = [generateRow n | x <- [1..n]]

> solidSquare2 :: Int -> Grid Char
> solidSquare2 n = replicate n (replicate n '*')

4.1.2.b hollow square

> hollowSquare :: Int -> Grid Char
> hollowSquare 0 = []
> hollowSquare 1 = ["*"]
> hollowSquare 2 = ["**", "**"]
> hollowSquare n = [solid n] ++ replicate (n-2) (hollow n) ++ [solid n]
>   where
>       solid n = replicate n '*'
>       hollow n = "*" ++ replicate (n-2) ' ' ++ "*"

4.1.2.c right triangle

Recursive attempt at defining a right triangle

> rightTriangle :: Int -> Grid Char
> rightTriangle 0 = []
> rightTriangle 1 = ["*"]
> rightTriangle 2 = ["*", "**"]
> rightTriangle n = rightTriangle (n-1) ++ [replicate n '*']

The above doesn't quite work and generates a leftTriangle instead. Is there an elegent recursive solution possible

Non-recursive definition of right triangle

> rightTriangle2 :: Int -> Grid Char
> rightTriangle2 n = [ replicate (n-i) ' ' ++ replicate i '*'| i <- [1 .. n]]

> catBitmap :: Grid Bool
> catBitmap = [
>     [False,False,True,False,False,False,False,False,True,False,False],
>     [False,True,False,True,False,False,False,True,False,True,False],
>     [False,True,False,False,True,True,True,False,False,True,False],
>     [True,False,False,False,False,False,False,False,False,False,True],
>     [True,False,False,True,False,False,False,True,False,False,True],
>     [True,False,False,False,False,True,False,False,False,False,True],
>     [False,True,False,False,False,False,False,False,False,True,False],
>     [False,False,True,True,True,True,True,True,False,False,False]
>   ]

4.1.3 Boolean grid

> bwCharView :: Grid Bool -> Grid Char
> bwCharView boolGrid = map (map (\x -> if x then '*' else ' ')) boolGrid

Try the above in ghci using the following command
prelude> charRender $ bwCharView catBitmap

> fprBitmap :: Grid Bool
> fprBitmap = [
>     [ f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f ],
>     [ f, t, t, t, t, f, f, t, t, t, f, f, f, t, t, t, f, f ],
>     [ f, t, f, f, f, f, f, t, f, f, t, f, f, t, f, f, t, f ],
>     [ f, t, t, t, f, f, f, t, t, t, f, f, f, t, t, t, f, f ],
>     [ f, t, f, f, f, f, f, t, f, f, f, f, f, t, f, f, t, f ],
>     [ f, t, f, f, f, f, f, t, f, f, f, f, f, t, f, f, t, f ],
>     [ f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f, f ] ]
>   where f = False ; t = True

> type Point = (Integer,Integer)

> catPoints :: [Point]
> catPoints =
>   [(2,0),(8,0),(1,1),(3,1),(7,1),(9,1),(1,2),(4,2),(5,2),(6,2),
>    (9,2),(0,3),(10,3),(0,4),(3,4),(7,4),(10,4),(0,5),(5,5),
>    (10,5),(1,6),(9,6),(2,7),(3,7),(4,7),(5,7),(6,7),(7,7)]

4.2 Points

4.2.4 pointsBitmap

> possiblePoints :: Integer -> [Point]
> possiblePoints n = [(i, j) | i <- [0..(n-1)], j <- [0, (n-1)]]

> pointsBitmap :: [Point] -> Grid Bool
> pointsBitmap points = map (\point -> if point `elem` points then [True] else [False]) (possiblePoints 10)

> logoShades :: Grid Float
> logoShades = [
>     [h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
>     [0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
>     [0,h,h,h,h,0,0,h,1,1,1,h,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
>     [0,0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
>     [0,0,0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0],
>     [0,0,0,h,h,h,h,0,0,h,1,1,1,h,0,0,h,h,h,h,h,h,h,h,h,h],
>     [0,0,0,0,h,h,h,h,0,0,1,1,1,1,0,0,0,h,h,h,h,h,h,h,h,h],
>     [0,0,0,0,0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0],
>     [0,0,0,0,h,h,h,h,0,0,1,1,1,1,1,h,0,0,h,h,h,h,h,h,h,h],
>     [0,0,0,h,h,h,h,0,0,h,1,1,1,1,1,1,0,0,0,h,h,h,h,h,h,h],
>     [0,0,0,h,h,h,h,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0],
>     [0,0,h,h,h,h,0,0,1,1,1,1,0,h,1,1,1,h,0,0,0,0,0,0,0,0],
>     [0,h,h,h,h,0,0,h,1,1,1,h,0,0,1,1,1,1,0,0,0,0,0,0,0,0],
>     [0,h,h,h,h,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0],
>     [h,h,h,h,0,0,1,1,1,1,0,0,0,0,0,h,1,1,1,h,0,0,0,0,0,0]
>   ] where h = 0.5

> charPalette :: [Char]
> charPalette = " .:oO8@"

> fprGreymap :: Grid Float
> fprGreymap = [
>   [ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ],
>   [ 0,a,a,a,a,0,0,b,b,b,0,0,0,1,1,1,0,0 ],
>   [ 0,a,0,0,0,0,0,b,0,0,b,0,0,1,0,0,1,0 ],
>   [ 0,a,a,a,0,0,0,b,b,b,0,0,0,1,1,1,0,0 ],
>   [ 0,a,0,0,0,0,0,b,0,0,0,0,0,1,0,0,1,0 ],
>   [ 0,a,0,0,0,0,0,b,0,0,0,0,0,1,0,0,1,0 ],
>   [ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ] ]
>   where a = 1/3 ; b = 2/3

> data RGB = RGB Int Int Int
>
> instance Show RGB where
>   show (RGB r g b) = show r ++" "++ show g ++" "++ show b

> fprPixmap :: Grid RGB
> fprPixmap = [
>   [ b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b ],
>   [ b,r,r,r,r,b,b,o,o,o,b,b,b,y,y,y,b,b ],
>   [ b,r,b,b,b,b,b,o,b,b,o,b,b,y,b,b,y,b ],
>   [ b,r,r,r,b,b,b,o,o,o,b,b,b,y,y,y,b,b ],
>   [ b,r,b,b,b,b,b,o,b,b,b,b,b,y,b,b,y,b ],
>   [ b,r,b,b,b,b,b,o,b,b,b,b,b,y,b,b,y,b ],
>   [ b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b ] ]
>   where r = RGB 7 0 0 ; o = RGB 7 3 0 ; y = RGB 7 7 0 ; b = RGB 0 0 0
