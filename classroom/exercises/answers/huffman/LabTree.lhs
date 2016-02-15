> module LabTree where

> data LabTree a b = LabLeaf a
>                  | LabBranch b (LabTree a b) (LabTree a b)

> instance (Show a, Show b) => Show (LabTree a b) where
>   show = drawLabLeafTree show show

> drawLabLeafTree :: (a -> [Char]) -> 
>                     (b -> [Char])  -> LabTree a b -> [Char]
> drawLabLeafTree tipShow labelShow
>  = unlines . thd3 . pic
>    where
>      thd3 (_,_,z) = z
>
>      pic (LabLeaf x)           = (1,1,["--" ++ tipShow x])
>      pic (LabBranch a r l) = (hl+hr+3, hl+2, top pl ++ mid ++ bot pr)
>                              where (hl,bl,pl) = pic l
>                                    (hr,br,pr) = pic r
>                                    top        = zipWith (++) 
>                                                      (replicate (bl-1) "   " ++
>                                                       [" ,-"] ++
>                                                       replicate (hl-bl) " | ")
>                                    mid        = [" |",
>                                                  "-{ "++labelShow a ++ " }",
>                                                  " |"]
>                                    bot        = zipWith (++) 
>                                                      (replicate (br-1) " | " ++
>                                                       [" `-"] ++
>                                                       replicate (hr-br) "   ")

