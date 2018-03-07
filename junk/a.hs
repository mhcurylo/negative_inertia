go :: (a -> a -> (a, a)) -> a -> [a] -> [a]
go f x [] = [x]
go f x (y:xs) = yy:(go f xx xs) 
  where
    (xx, yy) = f x y

go2 :: (a -> a -> (a, a)) -> [a] -> [a]
go2 f [] = []
go2 f [x] = [x]
go2 f (x:xs) = last r : (go2 f (init r))
  where 
    r = go f x xs

ff (idx, sx) y@(idy, sy) = ((idx, idy:sx), (idy, idx:sy))

z = []::[Int]

main = print (go2 ff [(1,z),(2,z),(3,z),(4,z)])
