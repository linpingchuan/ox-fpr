> import Data.List(sort, nub)

> data Tree a = Empty | Node (Tree a) a (Tree a) deriving Show


QUESTION 1

There is one element in each Node, so the size of the tree is also the
number of Nodes in the tree. The Empty tree is clearly not a Node; and
a tree of the form Node l a r has one Node at the root, plus however
many Nodes are in l and r.

> size :: Tree a -> Integer 
> size Empty        = 0
> size (Node l a r) = size l + 1 + size r


QUESTION 2

The naive way to implement tree is to construct a right-leaning tree
where all left subtrees are empty and the right subtree contains the
the rest of the tree, but this results in a very unbalanced tree.

> tree :: [a] -> Tree a
> tree []     = Empty
> tree (x:xs) = Node Empty x (tree xs) 

To get a balanced tree, it is possible to split the list in half, use
the middle element as the root node, and then use the left half of the
list to create the left subtree, and the right half to create the
right subtree

> tree' :: [a] -> Tree a
> tree' [] = Empty
> tree' xs = Node (tree' right) x (tree' left)
>     where
>       (right,x:left) = splitAt (length xs `div` 2) xs

(This is a bit inefficient: in the recursive case, it traverses the
list once to split it in two, and then a second time to build the two
subtrees. Can you see how to remove this inefficiency?)


QUESTION 3

It is clear that a value cannot be in the Empty tree. A value can be
in a non-empty tree in three ways: at the root, in the left subtree,
or in the right subtree. Note that because || is non-strict in its
right argument, when a value is found in some Node it is not necessary
to continue searching in the subtrees of that Node.

> memberT :: (Eq a) => a -> Tree a -> Bool
> memberT x Empty  = False
> memberT x (Node l a r)
>                  = (x == a) || memberT x l || memberT x r 


QUESTION 4

A search tree is one in which, for each node, the left subtree
contains only elements smaller than the node, and the right subtree
only elements that are greater (usually there are not duplicate
elements).  One way to create a searchTree is to write an insert
function that inserts an element into a searchTree, and recursively
insert elements of a list into an initially Empty search tree.

> searchTree :: (Ord a) => [a] -> Tree a
> searchTree []     = Empty
> searchTree (x:xs) = insert x (searchTree xs)
>     where
>       insert x Empty      = Node Empty x Empty
>       insert x (Node l a r) 
>           | x < a     = Node (insert x l) a r
>           | x > a     = Node l a (insert x r)
>           | otherwise = Node l a r

This can result in a highly unbalanced tree, however, if the elements
are in sorted or almost sorted order.  One way to guarantee a balanced
tree is to remove the duplicate elements (using nub), sort the list
(using sort), and then use "tree'" to create a balanced tree.  Because the
tree is sorted, this will result in a tree that obeys the search tree
invariant

> searchTree' :: (Ord a) => [a] -> Tree a
> searchTree' = tree' . sort . nub 


QUESTION 5

When searching for an element of a search tree, it is possible to
exploit the ordering invariant to make the search more efficient.
Specifically, at any node, it is only necessary to search either the
left or the right subtree by finding out if the current element is
greater or less than the value sought.  If the elements are equal, the
search is over.

> memberS :: (Ord a) => a -> Tree a -> Bool
> memberS x Empty = False
> memberS x (Node l a r) 
>     | x < a = memberS x l
>     | x > a = memberS x r
>     | otherwise = True


QUESTION 6

An inorder traversal of a tree is one in which, at each node, the left
subtree is recursively traversed, then element at that node is
examined, and then the right subtree.  For inOrder, the elements
should be collected into a list in that order.  The most obvious way
to do this is to concatenate the recursive calls and the element at
the current node together.

> inOrder :: Tree a -> [a] 
> inOrder Empty        = []
> inOrder (Node l a r) = inOrder l ++ [a] ++ inOrder r

This is somewhat inefficient: in the recursive case, it constructs the
list inOrder l, and then traverses it again to append elements on the
end. The inefficiency can be removed via an accumulating parameter;
the more general function inOrderThen traverses a tree and appends a
list all in one go.

> inOrder' :: Tree a -> [a]
> inOrder' t = inOrderThen t []
>     where
>       inOrderThen Empty        x = x
>       inOrderThen (Node l a r) x = inOrderThen l (a : inOrderThen r x)  