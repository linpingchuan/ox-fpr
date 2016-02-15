Infinity function. Invoking this function does not terminate.

> infinity :: Integer
> infinity = 1 + infinity

Invoking this via the following command:

$ ghci infinity.lhs

and then calling the terminate function, does not terminate