> module Images where
> import Data.Complex
> import Data.Bits
> import Bitmaps

> type CF = Complex Float

Useful helper funtion to convert Int to Float

> intToFloat :: Int -> Float
> intToFloat n = fromInteger (toInteger n)

Alternate to above:
*Images> fromIntegral 2 :: Float
2.0

First define function row such that
row 4 0.0 10.0 = [0.0, 3.33, 6.67, 10.0]

> row :: Int -> Float -> Float -> [Float]
> row 0 _ _ = []
> row 1 a _ = [a]
> row 2 a b = [a, b]
> row n a b = [a] ++ row (n-1) (a + split) b
>   where split = (b - a) / (intToFloat n - 1)

Complex row that adds a complex part to floats in a list
complexRow 2.0 [0.0, 3.33, 6.67, 10.0]

> complexRow :: Float -> [Float] -> [CF]
> complexRow imaginary floats = map (\ float -> float:+imaginary) floats

Now use complexRow to define grid

> grid :: Int -> Int -> CF -> CF -> Grid CF
> grid columns rows (a:+b) (c:+d) = [complexRow complexPart (row columns a c) | complexPart <- reverse (row rows b d) ]

Output:

*Images> grid 4 3 (0:+0) (3:+2)
[[0.0 :+ 2.0,1.0 :+ 2.0,2.0 :+ 2.0,3.0 :+ 2.0],[0.0 :+ 1.0,1.0 :+ 1.0,2.0 :+ 1.0,3.0 :+ 1.0],[0.0 :+ 0.0,1.0 :+ 0.0,2.0 :+ 0.0,3.0 :+ 0.0]]

> point :: CF
> point = (-1.0) :+ 0.5

> type Image c = CF -> c

> cols :: Image Bool
> cols = even . floor . realPart

Grid CF = [[CF]]
Image c = CF -> c

[[CF]] -> [[c]]

> sample :: Grid CF -> Image c -> Grid c
> sample gridCF image = (map (map image)) gridCF

Output
*Images> charRender (bwCharView (sample (grid 7 7 (0.5 :+ 0.5) (6.5 :+ 6.5)) cols))
* * * *
* * * *
* * * *
* * * *
* * * *
* * * *
* * * *

To run pbmRender "test.pbm" (sample (grid 90 90 (0.05 :+ 0.05) (8.95 :+ 8.95)) cols),
the correct Bitmaps.hs file needs to be loaded. The one in the same dir as this file, may not have the definition.

So load this file in ghci with a `-i` flag as follows

$ ghci Images.lhs -i /Users/anuragkapur/tech-stuff/ou-software-engineering/fpr/classroom/exercises/answers/images/Bitmaps.hs
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Bitmaps          ( /Users/anuragkapur/tech-stuff/ou-software-engineering/fpr/classroom/exercises/answers/images/Bitmaps.hs, interpreted )
[2 of 2] Compiling Images           ( Images.lhs, interpreted )
Ok, modules loaded: Images, Bitmaps.
*Images> pbmRender "test.pbm" (sample (grid 90 90 (0.05 :+ 0.05) (8.95 :+ 8.95)) cols)

3. Image with horizontal rows

> rows :: Image Bool
> rows = even . floor . imagPart

Testing the function

*Images> charRender (bwCharView (sample (grid 7 7 (0.5 :+ 0.5) (6.5 :+ 6.5)) rows))
*******

*******

*******

*******

4. Chequer image def

> chequer :: Image Bool
> chequer c = xor (cols c) (rows c)

*Images> charRender (bwCharView (sample (grid 7 7 (0.5 :+ 0.5) (6.5 :+ 6.5)) chequer))
 * * *
* * * *
 * * *
* * * *
 * * *
* * * *
 * * *

pbmRender "test-chequer.pbm" (sample (grid 90 90 (0.05 :+ 0.05) (8.95 :+ 8.95)) chequer)