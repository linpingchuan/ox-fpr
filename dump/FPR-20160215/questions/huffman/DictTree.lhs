> module DictTree where
> import Tree

> type DictTree a b = Tree (a, b)

> drawDictTree :: (a -> b -> [Char]) -> DictTree a b -> [Char]
> drawDictTree labelShow
>  = unlines . thd3 . pic
>    where
>      thd3 (_,_,z) = z
>
>      pic (Empty)           = (1,1,["--|"])
>      pic (Node r (a, b) l) = (hl+hr+3, hl+2, top pl ++ mid ++ bot pr)
>                             where (hl,bl,pl) = pic l
>                                   (hr,br,pr) = pic r
>                                   top        = zipWith (++) 
>                                                     (replicate (bl-1) "   " ++
>                                                      [" ,-"] ++
>                                                      replicate (hl-bl) " | ")
>                                   mid        = [" |",
>                                                 "-{ "++labelShow a b++ "}",
>                                                 " |"]
>                                   bot        = zipWith (++) 
>                                                     (replicate (br-1) " | " ++
>                                                      [" `-"] ++
>                                                      replicate (hr-br) "   ")
