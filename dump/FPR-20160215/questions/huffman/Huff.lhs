> module Huff where
> import Dict
> import Encode
> import Tree
> import LabTree
> import LeafTree

> type FreqDict         = Dict Char Int
> type HuffDict         = Dict Char [Bool]
> type WeightedHuffTree = LabTree (Char,Int) Int
> type DecodeHuffTree   = LeafTree Char

> frequencies :: String -> FreqDict
> frequencies = error "to be defined"

> sortOnFreq :: Ord b => [(a,b)] -> [(a,b)]
> sortOnFreq = error "to be completed"

> weight :: WeightedHuffTree -> Int
> weight = error "to be defined"

> combine :: [WeightedHuffTree] -> [WeightedHuffTree]
> combine = error "to be defined"

> insert :: WeightedHuffTree -> [WeightedHuffTree] -> [WeightedHuffTree]
> insert = error "to be defined"

> mkHuffTree :: FreqDict -> WeightedHuffTree
> mkHuffTree = error "to be defined"

> mkHuffDict :: WeightedHuffTree -> HuffDict
> mkHuffDict = error "to be defined"

> huffCodeFor :: WeightedHuffTree -> Char -> [[Bool]]
> huffCodeFor = error "to be defined"

> huffiseDocument :: HuffDict -> [Char] -> [Bool]
> huffiseDocument = error "to be defined"

> binToZeroOnes :: [Bool] -> [Char]
> binToZeroOnes = error "to be defined"

> pprHuffDict :: HuffDict -> [Char]
> pprHuffDict = error "to be defined"

> zeroOnesToBin :: [Char] -> [Bool]
> zeroOnesToBin = error "to be defined"

> readHuffDict :: [Char] -> [(Char,[Bool])]
> readHuffDict = error "to be defined"

> mkDecodeHuffTree :: [(Char,[Bool])] -> DecodeHuffTree
> mkDecodeHuffTree = error "to be defined"

> huffAndHaskellEncode :: [Char] -> [Char]
> huffAndHaskellEncode = error "to be defined"

> decodeHuffBin :: DecodeHuffTree -> DecodeHuffTree -> [Bool] -> [Char]
> decodeHuffBin orig (Leaf c)     []         = [c]
> decodeHuffBin orig (Leaf c)     xs         = c : decodeHuffBin orig orig xs
> decodeHuffBin orig (Branch l r) []         = error "run out of bits"
> decodeHuffBin orig (Branch l r) (True:xs)  = decodeHuffBin orig l xs
> decodeHuffBin orig (Branch l r) (False:xs) = decodeHuffBin orig r xs

> huffAndHaskellDecode :: [Char] -> [Char]
> huffAndHaskellDecode = error "to be defined"

> processFile :: FilePath -> (String->String) -> FilePath -> IO ()
> processFile inf f outf = readFile inf >>= (writeFile outf . f)
