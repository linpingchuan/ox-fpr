> module Dict where
> import DictTree
> import Tree
> import Data.List(sort)

> type Dict a b = DictTree a b

dict2List can be implemented by using the "design pattern" for this
datatype and appending the results of the recursive call

> dict2List :: Dict a b -> [(a,b)]
> dict2List Empty                = []
> dict2List (Node l (a, b) r)    = dict2List l ++ [(a,b)] ++ dict2List r

or, using an accumulating parameter, which is more efficient:

> dict2List' :: Dict a b -> [(a,b)]
> dict2List' t = go t []
>    where
>       go Empty             acc = acc
>       go (Node l (a, b) r) acc = go l ((a,b):go r acc)

list2Dict sorts the list, and then divides it in half, taking the
middle tuple as the root, and recursively building the subtrees.  This
results in a balanced tree.

> list2Dict :: (Ord a, Ord b) => [(a,b)] -> Dict a b
> list2Dict xs = go (sort xs)
>    where
>       go [] = Empty
>       go ys = Node (go left) (a, b) (go right)
>          where
>             (left,(a,b):right) = splitAt (length ys `div` 2) ys

addDict maintains the search tree invariant when inserting a single
key-value pair, but does not preserve balancing

> addDict :: Ord a => Dict a b -> a -> b -> Dict a b
> addDict Empty             x y = Node Empty (x, y) Empty
> addDict (Node l (a, b) r) x y 
>       | x < a     = Node (addDict l x y) (a, b) r
>       | x > a     = Node l (a, b) (addDict r x y)
>       | otherwise = Node l (x, y) r -- when x == a

lookupDict uses the search tree invariant to guide the search, so that
it only has to search either the left or the right subtree of any node

> lookupDict :: Ord a => Dict a b -> a -> Maybe b
> lookupDict Empty _               = Nothing
> lookupDict (Node l (a, b) r) x 
>       | x < a     = lookupDict l x
>       | x > a     = lookupDict r x
>       | otherwise = Just b -- when x == a  

> showdict :: (a -> String) -> (b -> String) -> Dict a b -> String
> showdict s t = drawDictTree drawpair 
>                where 
>                  drawpair s' t' = s s' ++" -> "++ t t'


