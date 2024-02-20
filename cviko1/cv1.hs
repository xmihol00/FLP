
idx (x:xs) (0) = x
idx (x:xs) (n) = idx xs (n-1)

elem' [] _ = False
elem' (x:xs) (y) = if x == y then True else elem' xs y 

nd [] = []
nd (x:xs) = if elem' (nd xs) x then nd xs else x : nd xs

nd_helper xs [] = []
nd_helper xs (y:ys) = if elem' xs y then nd_helper xs ys else y:(nd_helper (y:xs) ys)

nd' [] = []
nd' (x:xs) = x : (nd_helper (x:[]) (xs))


--nd' [] = []
--nd' (x:xs) 
--    | elem' xs x = nd' xs
--    | otherwise = x : (nd' xs)


ncd (x:xs)
    | xs == [] = [x]
    | x == head xs = ncd xs
    | otherwise = x : ncd xs
