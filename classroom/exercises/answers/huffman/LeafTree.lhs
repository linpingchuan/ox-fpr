> module LeafTree where

> data LeafTree a = Leaf a
>                 | Branch (LeafTree a) (LeafTree a)

> instance Show a => Show (LeafTree a) where
>   show = drawLeafTree show

> drawLeafTree :: (a -> [Char]) -> LeafTree a -> [Char]
> drawLeafTree tipShow
>  = unlines . thd3 . pic
>    where
>      thd3 (_,_,z) = z
>
>      pic (Leaf x)      = (1,1,["--" ++ tipShow x])
>      pic (Branch r  l) = (hl+hr+3, hl+2, top pl ++ mid ++ bot pr)
>                           where (hl,bl,pl) = pic l
>                                 (hr,br,pr) = pic r
>                                 top        = zipWith (++) 
>                                                   (replicate (bl-1) "   " ++
>                                                    [" ,-"] ++
>                                                    replicate (hl-bl) " | ")
>                                 mid        = ["-{"]
>                                 bot        = zipWith (++) 
>                                                   (replicate (br-1)  " | "++
>                                                    [" `-"] ++
>                                                    replicate (hr-br) "   ")

