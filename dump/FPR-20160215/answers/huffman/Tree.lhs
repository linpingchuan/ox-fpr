> module Tree where

> data Tree a = Empty | Node (Tree a) a (Tree a)

> instance (Show a) => Show (Tree a) where
>   show = drawTree

> drawTree :: Show a => Tree a -> [Char]
> drawTree
>  = unlines . thd3 . pic
>    where
>      thd3 (_,_,z) = z
>      pic (Empty)           = (1,1,["--|"])
>      pic (Node r a l) = (hl+hr+3, hl+2, top pl ++ mid ++ bot pr)
>                             where (hl,bl,pl) = pic l
>                                   (hr,br,pr) = pic r
>                                   top        = zipWith (++)
>                                                     (replicate (bl -1) "   " ++
>                                                      [" ,-"] ++
>                                                      replicate (hl-bl) " | ")
>                                   mid        = [" |",
>                                                 "-{ "++ show a ++ "}",
>                                                 " |"]
>                                   bot        = zipWith (++)
>                                                     (replicate (br-1) " | "  ++
>                                                      [" `-"] ++
>                                                      replicate (hr-br) "   ")
