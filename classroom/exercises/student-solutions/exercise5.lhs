> module Exercise5 where

Recursive definitions on trees

> data Tree a = Empty | Node (Tree a) a (Tree a) deriving Show

1. function size

> size :: Tree a -> Integer
> size Empty = 0
> size (Node a _ c) = size a + 1 + size c

2. list to tree

> tree :: [a] -> Tree a
> tree [] = Empty
> tree [a] = Node Empty a Empty
> tree (x:y:zs) = Node (tree [x]) y (tree zs)

Output:
Exercise5> tree [1, 2, 3, 4, 5]
Exercise5> Node (Node Empty 1 Empty) 2 (Node (Node Empty 3 Empty) 4 (Node Empty 5 Empty))

  2
1   4
   3 5

3. member

> memberT:: Eq a => a -> Tree a -> Bool
> memberT _ Empty = False
> memberT a (Node l p r) = memberT a l || p == a || memberT a r

Output:
*Exercise5> memberT 3 (tree [1, 2, 3, 4, 5])
True

*Exercise5> memberT 3 (Node (Node Empty 1 Empty) 2 (Node (Node Empty 3 Empty) 4 (Node Empty 5 Empty)))
True

4. searchTree

> searchTree :: Ord a => [a] -> Tree a
> searchTree [] = Empty
> searchTree [a] = Node Empty a Empty
> searchTree (x:xs) = insert x (searchTree xs)
>   where
>   insert a Empty = Node Empty a Empty
>   insert a (Node l p r)
>       | a < p = Node (insert a l) p r
>       | otherwise = Node l p (insert a r)

Output:
*Exercise5> inOrder $ searchTree [4,2,6,1,2,5]
[1,2,2,4,5,6]

5. memberS - member in a search tree

> memberS :: Ord a => a -> Tree a -> Bool
> memberS _ Empty = False
> memberS a (Node l p r)
>   | a == p = True
>   | a < p = memberS a l
>   | a > p = memberS a r

Output:
*Exercise5> memberS 3 (searchTree [4,2,6,1,2,5])
False

*Exercise5> memberS 6 (searchTree [4,2,6,1,2,5])
True

6. In-order traversal

> inOrder :: Tree a -> [a]
> inOrder Empty = []
> inOrder (Node l p r) = (inOrder l) ++ [p] ++ (inOrder r)

Output:
*Exercise5> inOrder $ searchTree [4,2,6,1,2,5]
[1,2,2,4,5,6]




