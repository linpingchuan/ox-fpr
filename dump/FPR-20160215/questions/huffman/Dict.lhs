> module Dict
>   (Dict, addDict, lookupDict, list2Dict, dict2List)
> where

> import DictTree

> type Dict a b = DictTree a b

> dict2List :: Dict a b -> [(a,b)]
> dict2List = error "to be defined"

> list2Dict :: (Ord a, Ord b) => [(a,b)] -> Dict a b
> list2Dict = error "to be defined"

> addDict :: Ord a => Dict a b -> a -> b -> Dict a b
> addDict = error "to be defined"

> lookupDict :: Ord a => Dict a b -> a -> Maybe b
> lookupDict = error "to be defined"
