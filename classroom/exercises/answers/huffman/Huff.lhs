> module Huff where
> import Dict
> import LabTree
> import LeafTree
> import Encode
> import Data.List(sort, group)
> import Data.Maybe(fromMaybe)

> type FreqDict         = Dict Char Int
> type HuffDict         = Dict Char [Bool]
> type WeightedHuffTree = LabTree (Char,Int) Int
> type DecodeHuffTree   = LeafTree Char

> frequencies :: String -> FreqDict
> frequencies str = list2Dict (map (\ grp -> (head grp, length grp)) (group (sort str))) 


sortOnFreq is a stable sort that sorts a list of tuples based on the
second part of the pairs.  It uses mergesort which, if probably
implemented, is stable.  To make sure it is stable, the merge function
must insert elements in the left list before elements in the right
list if they are equal.

> sortOnFreq :: Ord b => [(a,b)] -> [(a,b)]
> sortOnFreq []  = []
> sortOnFreq [x] = [x]
> sortOnFreq xs  = merge (sortOnFreq left) (sortOnFreq right)
>    where
>      (left,right) = splitAt (length xs `div` 2) xs
>      merge [] ys = ys
>      merge xs [] = xs
>      merge ((x1,x2):xs) ((y1,y2):ys) 
>               | x2 <= y2   = (x1,x2) : (merge xs ((y1,y2):ys)) -- using '<' breaks stability
>               | otherwise  = (y1,y2) : (merge ((x1,x2):xs) ys)

The weight of a WeightedHuffTree is cached, so we simply pattern match
and return the field containing the cached data

> weight :: WeightedHuffTree -> Int
> weight (LabLeaf (char,freq)) = freq
> weight (LabBranch wt _ _)    = wt

insert puts a WeightHuffTree into a sorted list such that it maintains
the sorting.  The specified invariant, that it inserts a combined tree
in front of other trees of equal weight, is determined by the test
"weight wht > weight t", which only makes the recursive call if the
tree being inserted is strictly greater

> insert :: WeightedHuffTree -> [WeightedHuffTree] -> [WeightedHuffTree]
> insert wht []     = [wht]
> insert wht (t:ts)
>       | weight wht > weight t = t : insert wht ts
>       | otherwise             = wht:(t:ts)

combine has two patterns, one for lists of two or more elements, and
everything else.  If the list contains two or more elements, in
combines the first two using the LabBranch constructor.  Note that, in
doing so, it adds their weights together to cash the reduction of the
subtrees. 

> combine :: [WeightedHuffTree] -> [WeightedHuffTree]
> combine (t:u:ts) = insert (LabBranch (weight t + weight u) t u) ts
> combine ts       = ts  

mkHuffTree implements the algorithm specified in the practical.  It
uses "until" from the Prelude, which repeatedly applies combine until
the predicate returns True, which is when the list only has one or
zero elements.  Note that this program errors when given an empty FreqDict.

> mkHuffTree :: FreqDict -> WeightedHuffTree
> mkHuffTree dict = head $ until (\ x -> singleton x || null x) combine (map LabLeaf sortedLst)
>    where
>       sortedLst = (sortOnFreq (sort (dict2List dict)))

> singleton :: [a] -> Bool 
> singleton [x] = True
> singleton _   = False

huffCodeFor generates the code for a given character.  The idea behind
this is that a path from a root to a character is the same as the path
from a subtree, with an additional True if it is the left
subtree or False if it is the right.  (hence the use of map (True:)
and map (False:) in front of the recursive call.  The algorithm
actually searches all paths of the tree, but paths that lead to
other characters are "wiped out" by returning [], since map (True:) []
= [].  Successful searches return [[]], which is different because 
map (True:) [[]] = True:[] = [True], meaning that the path is not wiped
out.

> huffCodeFor :: WeightedHuffTree -> Char -> [[Bool]]
> huffCodeFor (LabLeaf (c,_))      c' | c == c' = [[]]
> huffCodeFor (LabBranch w l r) c  = map (True:) (huffCodeFor l c) ++
>                                    map (False:) (huffCodeFor r c)
> huffCodeFor _                 _  = [] 

mkHuffdict creates a HuffDict by calculating the coding for every
letter in the dictionary.  This is a bit inefficient because it means
flattening the tree to obtain a list of characters in it, and then
huffCodeFor traverses the entire tree for every character.

> mkHuffDict :: WeightedHuffTree -> HuffDict
> mkHuffDict tree = list2Dict $ map charCode (flatten tree [])
>    where
>      flatten (LabLeaf a)       acc = a:acc
>      flatten (LabBranch _ l r) acc = flatten l (flatten r acc) 
>      
>      charCode (c,_) = (c, head (huffCodeFor tree c))

A more efficient but less elegant way of doing this is to calculate
the Huffman codes for every character in one go:

> mkHuffDict' t = list2Dict $ mkHuffList t
>     where
>       mkHuffList (LabLeaf (c,_))   = [(c,[])]
>       mkHuffList (LabBranch _ l r) = (map (\ (c,path) -> (c,True:path))  $ mkHuffList l) ++ 
>                                      (map (\ (c,path) -> (c,False:path)) $ mkHuffList r)

huffiseDocument maps every character to the Huffman coding given by
the supplied dictionary and concats them together, erroring if a char
is not found.

> huffiseDocument :: HuffDict -> [Char] -> [Bool]
> huffiseDocument dict str = concatMap (fromMaybe (error "Char not found in dictionary") . lookupDict dict) str  

> binToZeroOnes :: [Bool] -> [Char]
> binToZeroOnes []         = ""
> binToZeroOnes (True:xs)  = '1':binToZeroOnes xs
> binToZeroOnes (False:xs) = '0':binToZeroOnes xs  

> beginHuffDict = "%%Begin HuffDict %%"
> endHuffDict = "%%End HuffDict %%"

> pprHuffDict :: HuffDict -> [Char]
> pprHuffDict dict = beginHuffDict ++ "\n" ++ 
>                    concatMap ppr (dict2List dict) ++ "\n" ++
>                    endHuffDict ++ "\n"
>   where
>       ppr (x,y) = x:'\t':binToZeroOnes y ++ "\t"

> zeroOnesToBin :: [Char] -> [Bool]
> zeroOnesToBin []       = []
> zeroOnesToBin ('0':xs) = False : zeroOnesToBin xs
> zeroOnesToBin ('1':xs) = True  : zeroOnesToBin xs
> zeroOnesToBin (_:xs)   = error "Malformed binary string"

readHuffDict reads a string represenation of a Huffman dictionary by
converting the text into a list of strings (one line per element),
isolating the data from the delimiters, and then converting the
tab-separated pairs into a Haskell character-code pairs

> readHuffDict :: [Char] -> [(Char,[Bool])]
> readHuffDict xs
>   = toPairs (unlines 
>               (takeWhile (/=endHuffDict)
>                          (tail (dropWhile (/=beginHuffDict) (lines xs)))))
>     where
>       toPairs (x:'\t':ys) = (x,zeroOnesToBin (takeWhile (/='\t') ys)):
>                             toPairs (tail (dropWhile (/='\t') ys))
>       toPairs other       = []

mkDecodeHuffTree constructs a leaf tree (the weight information is
lost and unnecessary) that reconstructs the paths of the original
Huffman tree to be used in decoding text

> mkDecodeHuffTree :: [(Char,[Bool])] -> DecodeHuffTree
> mkDecodeHuffTree [(x,[])] = Leaf x
> mkDecodeHuffTree xs  
>   = Branch (mkDecodeHuffTree (getLefts xs))
>            (mkDecodeHuffTree (getRights xs))
>     where 
>       getLefts [] = []
>       getLefts ((x,True:xs):ys) = (x,xs):getLefts ys
>       getLefts (y:ys)           = getLefts ys
>                    
>       getRights [] = []
>       getRights ((x,False:xs):ys) = (x,xs):getRights ys
>       getRights (y:ys)             = getRights ys

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

> huffAndHaskellEncode :: [Char] -> [Char]
> huffAndHaskellEncode xs
>   = pprHuffDict huff_tab ++ hhencode (huffiseDocument huff_tab xs)
>     where 
>       huff_tab = mkHuffDict (mkHuffTree (frequencies xs))      

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

> decodeHuffBin :: DecodeHuffTree -> DecodeHuffTree -> [Bool] -> [Char]
> decodeHuffBin orig (Leaf c)     []         = [c]
> decodeHuffBin orig (Leaf c)     xs         = c : decodeHuffBin orig orig xs
> decodeHuffBin orig (Branch l r) []         = error "run out of bits"
> decodeHuffBin orig (Branch l r) (True:xs)  = decodeHuffBin orig l xs
> decodeHuffBin orig (Branch l r) (False:xs) = decodeHuffBin orig r xs

> huffAndHaskellDecode :: [Char] -> [Char]
> huffAndHaskellDecode input
>   = (decodeHuffBin decode_tree decode_tree binary)
>     where 
>       input_lines = lines input
>       table       = unlines (takeWhile (/=beginHaskellEncode) input_lines)
>       binary      = hhdecode 
>                       (unlines (dropWhile (/=beginHaskellEncode) input_lines))
>
>       decode_tree= mkDecodeHuffTree (readHuffDict table)

> processFile :: FilePath -> (String->String) -> FilePath -> IO ()
> processFile inf f outf = readFile inf >>= (writeFile outf . f)
